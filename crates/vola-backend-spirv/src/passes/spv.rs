use ahash::{AHashMap, AHashSet};
use rspirv::{
    dr::Builder,
    spirv::{FunctionControl, Word},
};
use rvsdg::{
    builder,
    edge::{InputType, OutportLocation, OutputType},
    nodes::{NodeType, StructuralNode},
    smallvec::{smallvec, SmallVec},
    NodeRef, SmallColl,
};

use crate::{
    spv::{ArithBaseTy, SpvType, TyShape},
    BackendSpirvError, SpirvBackend, SpirvConfig, SpirvModule,
};

struct EmitCtx {
    extinst_ids: AHashMap<String, Word>,
    ///All known mappings from a SpvType to a type id in the module
    type_mapping: AHashMap<SpvType, Word>,
}

impl SpirvBackend {
    pub fn into_spv_module(&self, config: &SpirvConfig) -> Result<SpirvModule, BackendSpirvError> {
        //Build the initial _empty_ module with the header specified by config
        let mut b = Builder::new();
        let mut ctx = EmitCtx {
            extinst_ids: AHashMap::default(),
            type_mapping: AHashMap::default(),
        };
        b.set_version(config.version_major, config.version_minor);
        b.memory_model(
            rspirv::spirv::AddressingModel::Logical,
            rspirv::spirv::MemoryModel::Vulkan,
        );
        //always emitting a linkable shader module
        b.capability(rspirv::spirv::Capability::Linkage);
        b.capability(rspirv::spirv::Capability::Shader);

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

        let mut lmd_dependecies = AHashMap::default();
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
                                deps.push(self.graph.edge(edg).src().node);
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
            lmd_dependecies.insert(tlnode_ref, deps);
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

        //Maps each λ to its corresbonding OpFunction id in the module.
        let mut lmd_mapping: AHashMap<NodeRef, Word> = AHashMap::new();

        //the stack of _yet to be added_ λs
        let mut work_queue: Vec<_> = lmd_dependecies.keys().cloned().collect();
        while !work_queue.is_empty() {
            let mut resolved_any = false;
            let mut wait_queue = Vec::with_capacity(work_queue.len());

            'lmd_worker: while let Some(tbw) = work_queue.pop() {
                //check if we can emit that λ, if not, push into wait_queue
                for dep in lmd_dependecies.get(&tbw).unwrap().iter() {
                    if !lmd_mapping.contains_key(dep) {
                        //dependecy is not met, continue
                        wait_queue.push(tbw);
                        continue 'lmd_worker;
                    }
                }

                let exported_name = if exported_nodes.contains(tbw) {
                    if let Some(name) = self.idents.get(&tbw.into()) {
                        Some(name.clone())
                    } else {
                        #[cfg(feature = "log")]
                        log::error!(
                            "Node is exported, but unnamed, naming after the node id: {tbw}"
                        );
                        Some(format!("{tbw}"))
                    }
                } else {
                    None
                };
                //if we reached till this point, we can process
                let lmd_id = self.emit_lmd(builder, ctx, *tbw, exported_name)?;
                lmd_mapping.insert(*tbw, lmd_id);
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
        println!("Emitting lmd {lmd}");
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

        let mut parameter_ids = SmallColl::new();
        //add all the parameters
        for argidx in 0..argument_tys.len() {
            let paramid = builder.function_parameter(argument_tys[argidx]);
            parameter_ids.push(paramid);
        }

        //Now begin the block
        builder.begin_block(None).unwrap();
        //TODO fill function and provide all the ids.

        //NOTE: There are three cases for a return.
        //      1: Void -> in that case, just call ret.
        //      2: Single value, is easy call return as intended
        //      3: Multiple values: In that case, inject a OpCompositeConstuct for the `retty` that takes care of
        //         Assembling the return struct as declared.

        builder.ret().unwrap(); //TODO add that routine
        builder.end_function().unwrap();
        //now return the id
        Ok(fid)
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
        println!("Found {} caller!", caller.len());
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
        println!("ApplyNode {apply}");
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
            println!("{ty:?}");
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
                    println!("{idx}: {:?} != {:?}", subty, superset[idx]);
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
