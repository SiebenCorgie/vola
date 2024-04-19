use core::panic;

use ahash::{AHashMap, AHashSet};
use rspirv::{
    dr::{Builder, Operand},
    spirv::{FunctionControl, Word},
};
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::{NodeType, StructuralNode},
    region::RegionLocation,
    smallvec::{smallvec, SmallVec},
    NodeRef, SmallColl,
};

use crate::{
    graph::BackendOp,
    spv::{ArithBaseTy, SpvType, TyShape},
    BackendSpirvError, SpirvBackend, SpirvConfig, SpirvModule,
};

pub struct EmitCtx {
    pub extinst_ids: AHashMap<String, Word>,
    ///All known mappings from a SpvType to a type id in the module
    type_mapping: AHashMap<SpvType, Word>,
    ///NOTE: we identify the nodes by their outport. Since for instance the different
    //       arguments of a region map to different SPIR-V parameter ids.
    //       So a function-def is defined by its def-port, and a _simple-node_ is defined by its single
    //       output port
    node_mapping: AHashMap<OutportLocation, Word>,
}

impl SpirvBackend {
    pub fn into_spv_module(&self, config: &SpirvConfig) -> Result<SpirvModule, BackendSpirvError> {
        //Build the initial _empty_ module with the header specified by config
        let mut b = Builder::new();
        let mut ctx = EmitCtx {
            extinst_ids: AHashMap::default(),
            type_mapping: AHashMap::default(),
            node_mapping: AHashMap::default(),
        };
        b.set_version(config.version_major, config.version_minor);
        b.memory_model(
            rspirv::spirv::AddressingModel::Logical,
            rspirv::spirv::MemoryModel::Vulkan,
        );
        //always emitting a linkable shader module
        b.capability(rspirv::spirv::Capability::Linkage);
        b.capability(rspirv::spirv::Capability::Shader);
        b.capability(rspirv::spirv::Capability::VulkanMemoryModel);

        for ext in &config.extensions {
            b.extension(ext.clone());
        }

        for ext_inst in &config.ext_inst {
            let id = b.ext_inst_import(ext_inst.clone());
            assert!(ctx.extinst_ids.insert(ext_inst.clone(), id).is_none());
        }

        self.emit_into(&mut ctx, &mut b)?;

        Ok(b.module())
    }

    fn emit_into(&self, ctx: &mut EmitCtx, builder: &mut Builder) -> Result<(), BackendSpirvError> {
        //Our strategy is a bottom-up procedure, where we assign each node a
        //id in the module.
        //
        //Since SPIR-V is a SCF-graph, construction _should_ be possible directly.
        //
        // We proceed by first building a dependecy graph of all top-level λ-nodes that are exported.
        // We then process the dependencies of each export first, and finally process the exported λ-nodes.
        // This is possible, because the graph is already in a SPIR-V-like form. So no λ-nesting etc. _should_
        // be present.
        //
        // NOTE: In theory you can forward declare function... but eh, its not that hard, and we don't have
        //       cyclic dependencies by definition.

        let mut lmd_dependecies: AHashMap<OutportLocation, SmallVec<[OutportLocation; 3]>> =
            AHashMap::default();
        for tlnode_ref in self
            .graph
            .region(&self.graph.toplevel_region())
            .unwrap()
            .nodes
            .iter()
        {
            let tlnode = self.graph.node(*tlnode_ref);
            let mut deps: SmallColl<_> = SmallColl::new();
            match &tlnode.node_type {
                NodeType::Lambda(lmd) => {
                    //checkout all dependecies.
                    for cvidx in 0..lmd.context_variable_count() {
                        if let Some(cv) = tlnode.inport(&InputType::ContextVariableInput(cvidx)) {
                            if let Some(edg) = cv.edge {
                                deps.push(self.graph.edge(edg).src().clone());
                            }
                        }
                    }
                }
                _ => {
                    return Err(BackendSpirvError::Any {
                        text: format!("Unexpected none-λ node in toplevel of SPIR-V graph."),
                    })
                }
            }
            //in this case we always work on lambda declerations.
            let key = OutportLocation {
                node: *tlnode_ref,
                output: OutputType::LambdaDeclaration,
            };
            lmd_dependecies.insert(key, deps);
        }

        //NOTE: lets us find out if some processed lmd needs to be exported, and also
        //      deduplicates those by being a Set.
        let exported_nodes: AHashSet<NodeRef> = self
            .graph
            .region(&self.graph.toplevel_region())
            .unwrap()
            .results
            .iter()
            .filter_map(|res| {
                if let Some(edg) = res.edge {
                    Some(self.graph.edge(edg).src().node)
                } else {
                    None
                }
            })
            .collect();

        //now, setup all λ-nodes with a id

        //the stack of _yet to be added_ λs
        let mut work_queue: Vec<_> = lmd_dependecies.keys().cloned().collect();
        while !work_queue.is_empty() {
            let mut resolved_any = false;
            let mut wait_queue = Vec::with_capacity(work_queue.len());

            'lmd_worker: while let Some(tbw) = work_queue.pop() {
                //check if we can emit that λ, if not, push into wait_queue
                for dep in lmd_dependecies.get(&tbw).unwrap().iter() {
                    if !ctx.node_mapping.contains_key(dep) {
                        //dependecy is not met, continue
                        wait_queue.push(tbw);
                        continue 'lmd_worker;
                    }
                }

                let exported_name = if exported_nodes.contains(&tbw.node) {
                    if let Some(name) = self.idents.get(&tbw.node.into()) {
                        Some(name.clone())
                    } else {
                        #[cfg(feature = "log")]
                        log::error!(
                            "Node is exported, but unnamed, naming after the node id: {}",
                            tbw.node
                        );
                        Some(format!("{}", tbw.node))
                    }
                } else {
                    None
                };
                //if we reached till this point, we can process
                let lmd_id = self.emit_lmd(builder, ctx, tbw.node, exported_name)?;
                assert!(tbw.output == OutputType::LambdaDeclaration);
                ctx.node_mapping.insert(tbw, lmd_id);
                //flag that we produced at least one
                resolved_any = true;
            }

            if !resolved_any {
                return Err(BackendSpirvError::Any {
                    text: format!(
                        "Failed to resolve any λ-Node in SPIR-V emission with still {} nodes left",
                        wait_queue.len()
                    ),
                });
            }
            //switch out queues
            std::mem::swap(&mut wait_queue, &mut work_queue);
        }

        Ok(())
    }

    fn emit_lmd(
        &self,
        builder: &mut Builder,
        ctx: &mut EmitCtx,
        lmd: NodeRef,
        exported: Option<String>,
    ) -> Result<Word, BackendSpirvError> {
        //init function
        //get return type and map to struct if needed
        let rettys = self.get_return_type(lmd).unwrap();
        let retty = match rettys.len() {
            0 => builder.type_void(),
            1 => register_or_get_type(builder, ctx, &rettys[0]),
            _ => tupel_to_composite(builder, ctx, rettys),
        };

        //now collect the argument types and do the same
        let argument_tys = self.recover_signature_from_caller(lmd).unwrap();
        let argument_tys = match argument_tys.len() {
            0 => smallvec!(),
            _ => argument_tys
                .iter()
                .map(|t| register_or_get_type(builder, ctx, t))
                .collect::<SmallColl<_>>(),
        };

        let function_type = builder.type_function(retty, argument_tys.clone());

        let fid = builder
            .begin_function(
                retty,
                None,
                FunctionControl::CONST | FunctionControl::PURE,
                function_type,
            )
            .unwrap();

        if let Some(export_name) = exported {
            let _export_id = builder.decorate(
                fid,
                rspirv::spirv::Decoration::LinkageAttributes,
                [
                    Operand::LiteralString(export_name),
                    Operand::LinkageType(rspirv::spirv::LinkageType::Export),
                ],
            );
        }

        //add all the parameters
        for argidx in 0..argument_tys.len() {
            let paramid = builder.function_parameter(argument_tys[argidx]).unwrap();
            //safe the paramid as mapping from a region-local argument
            ctx.node_mapping.insert(
                OutportLocation {
                    node: lmd,
                    output: OutputType::Argument(argidx),
                },
                paramid,
            );
        }

        //register all context-variable ids as outport->word mapping
        let cvcount = self
            .graph
            .node(lmd)
            .node_type
            .unwrap_lambda_ref()
            .context_variable_count();
        for cvidx in 0..cvcount {
            let def_node = self
                .graph
                .find_callabel_def(OutportLocation {
                    node: lmd,
                    output: OutputType::ContextVariableArgument(cvidx),
                })
                .expect("Could not get call-def of used cv");
            //now register it as the cvport
            let def_id = ctx.node_mapping.get(&def_node).unwrap();
            //this forwards the λ-def mapping as the cv into this region
            ctx.node_mapping.insert(
                OutportLocation {
                    node: lmd,
                    output: OutputType::ContextVariableArgument(cvidx),
                },
                *def_id,
            );
        }

        //now build the region ctx.

        //Now begin the block
        builder.begin_block(None).unwrap();
        //TODO fill function and provide all the ids.

        //NOTE: There are three cases for a return.
        //      1: Void -> in that case, just call ret.
        //      2: Single value, is easy call return as intended
        //      3: Multiple values: In that case, inject a OpCompositeConstuct for the `retty` that takes care of
        //         Assembling the return struct as declared.
        let return_id = self.emit_region(
            ctx,
            builder,
            RegionLocation {
                node: lmd,
                region_index: 0,
            },
            retty,
        )?;

        //return value if needed
        if let Some(id) = return_id {
            builder.ret_value(id).unwrap();
        } else {
            builder.ret().unwrap();
        }

        builder.end_function().unwrap();
        //now return the id
        Ok(fid)
    }

    ///Emits `reg` into `builder`, assuming the builder has just begone a new block. If the return-type is not Void,
    ///returns the result id. This includes a constructed composite in case of multiple results.
    fn emit_region(
        &self,
        ctx: &mut EmitCtx,
        builder: &mut Builder,
        reg: RegionLocation,
        result_type_id: Word,
    ) -> Result<Option<Word>, BackendSpirvError> {
        //explore from the results upwards and build the
        //node -> word mapping bottom up. So similar to the λ-dependency scheduler,
        // but region local.

        let mut dependencies: AHashMap<OutportLocation, SmallColl<OutportLocation>> =
            AHashMap::new();
        for node in &self.graph.region(&reg).unwrap().nodes {
            let mut deps = AHashSet::new();
            for inpidx in 0..self.graph.node(*node).inputs().len() {
                if let Some(src) = self.graph.node(*node).input_src(&self.graph, inpidx) {
                    deps.insert(src);
                }
            }

            dependencies.insert(
                OutportLocation {
                    node: *node,
                    output: OutputType::Output(0),
                },
                deps.into_iter().collect(),
            );
        }

        //NOTE: there _shouldn't_ be any dead nodes.
        let mut work_queue = self
            .graph
            .region(&reg)
            .unwrap()
            .nodes
            .clone()
            .into_iter()
            //map to the outport location of each node in the region
            .map(|node| OutportLocation {
                node,
                output: OutputType::Output(0),
            })
            .collect::<Vec<_>>();

        while !work_queue.is_empty() {
            let mut changed_any = false;
            let mut local_queue = Vec::with_capacity(work_queue.len());
            //move all elements into the local queue
            local_queue.append(&mut work_queue);
            'node_worker: for node in local_queue {
                let node_dependencies = dependencies.get(&node).unwrap();
                for dep in node_dependencies {
                    if !ctx.node_mapping.contains_key(dep) {
                        //add back to work queue and continue
                        work_queue.push(node);
                        continue 'node_worker;
                    }
                }
                //Get the input srcs, in order. We can't use the node_dependecies directly, since those
                //do not contain duplicates etc.
                let input_srcs = self
                    .graph
                    .node(node.node)
                    .input_srcs(&self.graph)
                    .into_iter()
                    .map(|src| src.expect("Expected all srcs of the node to be connected!"))
                    .collect();
                //all dependecies statisfied, therfore
                //call the serializer and add to mapping
                let result_type = self
                    .get_single_node_result_type(node.node)
                    .expect("Expected simple node to have retun type!");
                let node_id =
                    self.serialize_node(ctx, builder, &input_srcs, &result_type, node.node)?;
                ctx.node_mapping.insert(node, node_id);
                changed_any = true;
            }

            if !work_queue.is_empty() && !changed_any {
                #[cfg(feature = "log")]
                log::error!(
                    "Node serializer stopped working, but there where still {} nodes in {reg:?}",
                    work_queue.len()
                );
                return Err(BackendSpirvError::Any { text: format!("Failed to serialize all nodes in reg {reg:?}. There was probably a dependecy cycle, which is not allowed!") });
            }
        }

        //build the result based on the amount of outputs.
        //for single output we can just return the id of the output-connected node.
        // of all others we

        match self
            .graph
            .node(reg.node)
            .node_type
            .unwrap_lambda_ref()
            .result_count()
        {
            0 => Ok(None),
            1 => {
                let result_connected = self
                    .graph
                    .region(&reg)
                    .unwrap()
                    .result_src(&self.graph, 0)
                    .unwrap();
                let result_id = ctx.node_mapping.get(&result_connected).unwrap();
                Ok(Some(*result_id))
            }
            rescount => {
                //in this case, build a composite out of all results and return that.
                let constituents: Vec<Word> = (0..rescount)
                    .map(|idx| {
                        let src_port = self
                            .graph
                            .region(&reg)
                            .unwrap()
                            .result_src(&self.graph, idx)
                            .unwrap();
                        *ctx.node_mapping.get(&src_port).unwrap()
                    })
                    .collect();

                let result_id = builder
                    .composite_construct(result_type_id, None, constituents)
                    .unwrap();
                Ok(Some(result_id))
            }
        }
    }

    fn serialize_node(
        &self,
        ctx: &mut EmitCtx,
        builder: &mut Builder,
        input_srcs: &SmallColl<OutportLocation>,
        result_type: &SpvType,
        node: NodeRef,
    ) -> Result<Word, BackendSpirvError> {
        let mut src_ids = input_srcs
            .iter()
            .map(|node| *ctx.node_mapping.get(node).unwrap())
            .collect::<SmallColl<Word>>();
        match &self.graph.node(node).node_type {
            NodeType::Simple(s) => match &s.op {
                BackendOp::SpirvOp(spvop) => {
                    let result_id = builder.id();
                    let result_type_id = register_or_get_type(builder, ctx, result_type);
                    let instruction =
                        spvop.build_instruction(&ctx, &src_ids, result_type_id, result_id);

                    //TODO: kinda hack atm.
                    if spvop.instruction_is_type_or_constant() {
                        //goes into the type_constant header
                        builder.module_mut().types_global_values.push(instruction);
                    } else {
                        //Goes into the current builder
                        builder
                            .insert_into_block(rspirv::dr::InsertPoint::End, instruction)
                            .unwrap();
                    }

                    Ok(result_id)
                }
                BackendOp::Dummy => panic!("Unexpected Dummy node in SPIR-V serialization"),
            },
            NodeType::Apply(_a) => {
                //allways translates to a call.
                //The region construction allready took care of forwarding the λ-def to the cv-ports,
                //so we really just have to map 1:1 to the call.
                let result_type_id = *ctx.type_mapping.get(result_type).unwrap();

                //NOTE: remove the first, since we resolve the call_id ourselfs below
                src_ids.remove(0);

                let call_id = {
                    let lmddef = self
                        .graph
                        .find_producer_inp(InportLocation {
                            node,
                            input: InputType::Input(0),
                        })
                        .unwrap();
                    *ctx.node_mapping.get(&lmddef).unwrap()
                };

                let result_id = builder
                    .function_call(result_type_id, None, call_id, src_ids)
                    .unwrap();
                Ok(result_id)
            }
            //TODO: implement ifs / matches and loops so we could _in principle_ emit those.
            any => panic!("Unsupported node type {any:?}"),
        }
    }

    ///Returns the order of argument tys for a λ-function, or Non if `lmd` is not a lambda node.
    pub fn get_arg_tys(&self, lmdref: NodeRef) -> Option<SmallColl<Option<SpvType>>> {
        if self.graph.node(lmdref).node_type.is_lambda() {
            let mut argtys = SmallColl::new();
            let lmd = self.graph.node(lmdref).node_type.unwrap_lambda_ref();

            let argcount = lmd.argument_count();
            for argidx in 0..argcount {
                //NOTE: we use the port tagging here, since an argument might be present, but unused
                let ty = if let Some(ty) = self.typemap.get(
                    &OutportLocation {
                        node: lmdref,
                        output: OutputType::Argument(argidx),
                    }
                    .into(),
                ) {
                    Some(ty.clone())
                } else {
                    //try to get the type from a connected edge
                    if lmd.argument(argidx).unwrap().edges.len() > 0 {
                        Some(
                            self.graph
                                .edge(lmd.argument(argidx).unwrap().edges[0])
                                .ty
                                .get_type()
                                .unwrap()
                                .clone(),
                        )
                    } else {
                        None
                    }
                };
                argtys.push(ty);
            }

            Some(argtys)
        } else {
            None
        }
    }

    ///Recovers the argument signature of `lmd` from its (first) caller (an apply node). Panics if the callee's signature is not a
    /// super-set of the signature used within the λ.
    pub fn recover_signature_from_caller(&self, lmd: NodeRef) -> Option<SmallColl<SpvType>> {
        if !self.graph.node(lmd).node_type.is_lambda() {
            return None;
        }
        let internal_sig = self.get_arg_tys(lmd)?;

        //find the calle of lmd
        let caller = self.graph.find_caller(lmd)?;
        if caller.len() == 0 {
            #[cfg(feature = "log")]
            log::error!("Cannot produce signature from caller, since λ {lmd} is never called. Substituting unknown args with Void.");

            return Some(
                internal_sig
                    .into_iter()
                    .map(|possible_ty| possible_ty.unwrap_or(SpvType::Void))
                    .collect(),
            );
        }

        let final_sig = self.build_input_sig_of_apply_node(caller[0]);
        assert!(Self::signature_is_subset(&internal_sig, &final_sig));

        //TODO: for sanity, do it for all other callers as well atm. But that could be removed
        //later on.
        for c in 1..caller.len() {
            let sig = self.build_input_sig_of_apply_node(caller[c]);
            assert!(Self::signature_is_subset(&internal_sig, &sig));
        }

        Some(final_sig)
    }

    fn build_input_sig_of_apply_node(&self, apply: NodeRef) -> SmallColl<SpvType> {
        let mut tys = SmallColl::new();
        //NOTE: skipping callee input.
        for input in self.graph.node(apply).inputs().iter().skip(1) {
            let ty = self
                .graph
                .edge(
                    input
                        .edge
                        .expect("Expected all inputs of apply node to be used!"),
                )
                .ty
                .get_type()
                .expect("expected apply-node argument to be value edge.");
            tys.push(ty.clone());
        }

        tys
    }

    fn signature_is_subset(
        subset: &SmallColl<Option<SpvType>>,
        superset: &SmallColl<SpvType>,
    ) -> bool {
        if subset.len() > superset.len() {
            return false;
        }

        for idx in 0..subset.len() {
            if let Some(subty) = &subset[idx] {
                if *subty != superset[idx] {
                    return false;
                }
            }
        }

        true
    }

    ///Returns the return type of the λ-function, or None, if `lmd` is not a lambda node.
    pub fn get_return_type(&self, lmd: NodeRef) -> Option<SmallColl<SpvType>> {
        if self.graph.node(lmd).node_type.is_lambda() {
            //collect all result port types.
            //at this point we don't allow _unconnected_ result ports. Cause we don't known
            //what default data to provide in that case.

            let mut result_types = SmallColl::new();
            let lmd = self.graph.node(lmd).node_type.unwrap_lambda_ref();
            for retidx in 0..lmd.result_count() {
                let retport = lmd.result(retidx).unwrap();
                if let Some(edg) = retport.edge {
                    let ty = self.graph.edge(edg).ty.get_type().unwrap();
                    result_types.push(ty.clone());
                } else {
                    panic!("Resultport {retidx} was not connected!");
                }
            }

            Some(result_types)
        } else {
            None
        }
    }
}

///registers or gets the given type
fn register_or_get_type(builder: &mut Builder, ctx: &mut EmitCtx, ty: &SpvType) -> Word {
    if let Some(known) = ctx.type_mapping.get(ty) {
        return *known;
    }

    let tyword = match ty {
        SpvType::Void => builder.type_void(),
        SpvType::Undefined => panic!("Cannot create undefined type!"),
        SpvType::State => panic!("Cannot create a type from State"),
        SpvType::Arith(a) => {
            let basetype = match a.base {
                ArithBaseTy::Integer { signed } => {
                    builder.type_int(a.resolution, if signed { 1 } else { 0 })
                }
                ArithBaseTy::Float => builder.type_float(a.resolution),
            };

            match &a.shape {
                TyShape::Scalar => basetype,
                TyShape::Vector { width } => builder.type_vector(basetype, *width),
                TyShape::Matrix { width, height } => {
                    let column_type = builder.type_vector(basetype, *height);
                    builder.type_matrix(column_type, *width)
                }
                TyShape::Tensor { dim: _ } => {
                    panic!("tensors not (yet) supported (how did you get those at all?)")
                }
            }
        }
        SpvType::RuntimeArray(ty) => {
            let arithty = register_or_get_type(builder, ctx, &SpvType::Arith(ty.clone()));
            builder.type_runtime_array_id(None, arithty)
        }
    };

    ctx.type_mapping.insert(ty.clone(), tyword);

    tyword
}

///Helper, that mapps a tupel of types to a composite in the same order.
/// This is the way the rust-gpu codegen realises tupel, so we do it the same way.
fn tupel_to_composite(builder: &mut Builder, ctx: &mut EmitCtx, tys: SmallColl<SpvType>) -> Word {
    let local_mapped = tys
        .iter()
        .map(|t| register_or_get_type(builder, ctx, t))
        .collect::<SmallColl<_>>();

    builder.type_struct(local_mapped)
}
