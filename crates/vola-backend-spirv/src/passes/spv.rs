use core::panic;

use ahash::{AHashMap, AHashSet, HashMap};
use rspirv::{
    dr::{Builder, Instruction, Operand},
    spirv::{FunctionControl, LoopControl, SelectionControl, Word},
};
use rvsdg::{
    builder,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::{ApplyNode, NodeType, StructuralNode},
    region::RegionLocation,
    smallvec::{smallvec, SmallVec},
    util::cfg::{Cfg, CfgNode, CfgRef},
    NodeRef, SmallColl,
};

use crate::{
    graph::BackendOp,
    spv::{ArithBaseTy, SpvOp, SpvType, TyShape},
    BackendSpirvError, SpirvBackend, SpirvConfig, SpirvModule,
};

use super::cfg_dot::cfg_to_svg;

pub struct EmitCtx {
    pub extinst_ids: AHashMap<String, Word>,
    ///All known mappings from a SpvType to a type id in the module
    type_mapping: AHashMap<SpvType, Word>,
    ///NOTE: we identify the nodes by their outport. Since for instance the different
    //       arguments of a region map to different SPIR-V parameter ids.
    //       So a function-def is defined by its def-port, and a _simple-node_ is defined by its single
    //       output port
    //
    node_mapping: AHashMap<OutportLocation, Word>,
}

impl EmitCtx {
    pub fn get_port_id(&self, port: &OutportLocation) -> Option<Word> {
        self.node_mapping.get(port).cloned()
    }

    //Might overwrite and return the last known port
    pub fn set_port_id(&mut self, port: OutportLocation, id: Word) -> Option<Word> {
        self.node_mapping.insert(port, id)
    }
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
                    if ctx.get_port_id(dep).is_none() {
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
                ctx.set_port_id(tbw, lmd_id);
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
            .begin_function(retty, None, FunctionControl::empty(), function_type)
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
            ctx.set_port_id(
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
            let def_id = ctx.get_port_id(&def_node).unwrap();
            //this forwards the λ-def mapping as the cv into this region
            ctx.set_port_id(
                OutportLocation {
                    node: lmd,
                    output: OutputType::ContextVariableArgument(cvidx),
                },
                def_id,
            );
        }

        //now build the region ctx.

        //Now begin the block
        let lmdregion = RegionLocation {
            node: lmd,
            region_index: 0,
        };

        //TODO fill function and provide all the ids.

        //NOTE: There are three cases for a return.
        //      1: Void -> in that case, just call ret.
        //      2: Single value, is easy call return as intended
        //      3: Multiple values: In that case, inject a OpCompositeConstuct for the `retty` that takes care of
        //         Assembling the return struct as declared.

        self.emit_region(ctx, builder, lmdregion)?;

        //build the result based on the amount of outputs.
        //for single output we can just return the id of the output-connected node.
        // of all others we
        let return_id = match self
            .graph
            .node(lmd)
            .node_type
            .unwrap_lambda_ref()
            .result_count()
        {
            0 => None,
            1 => {
                let result_connected = self
                    .graph
                    .region(&lmdregion)
                    .unwrap()
                    .result_src(&self.graph, 0)
                    .unwrap();
                let result_id = ctx.get_port_id(&result_connected).unwrap();
                Some(result_id)
            }
            rescount => {
                //in this case, build a composite out of all results and return that.
                let constituents: Vec<Word> = (0..rescount)
                    .map(|idx| {
                        let src_port = self
                            .graph
                            .region(&lmdregion)
                            .unwrap()
                            .result_src(&self.graph, idx)
                            .unwrap();
                        ctx.get_port_id(&src_port).unwrap()
                    })
                    .collect();

                let result_id = builder
                    .composite_construct(retty, None, constituents)
                    .unwrap();
                Some(result_id)
            }
        };

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
    ) -> Result<(), BackendSpirvError> {
        println!("Begin {reg:?}");
        //Generate the CSG for this region
        let cfg = self
            .graph
            .region_to_cfg_scfr(reg)
            .expect("Could not transform region to CFG");

        cfg_to_svg(&cfg, &format!("{reg:?}.svg"));

        //Now setup ids for all BasicBlock in the BB.
        //This'll let us setup any control-flow reliably.
        //
        // after that walk the CSG and emit all Simple nodes of each
        // BB. Whenever we reach a branch, set that up accordingly to the
        // CFG.
        let bb_label = cfg
            .nodes
            .iter()
            .filter_map(|(key, cfgnode)| match cfgnode {
                CfgNode::BasicBlock(_) => Some((key.clone(), builder.id())),
                //CfgNode::LoopHeader { .. } => Some((key.clone(), builder.id())),
                //CfgNode::LoopCtrlTail { .. } => Some((key.clone(), builder.id())),
                CfgNode::BranchHeader { .. } => Some((key.clone(), builder.id())),
                CfgNode::BranchMerge { .. } => Some((key.clone(), builder.id())),
                _ => None,
            })
            .collect::<AHashMap<_, _>>();

        //Now setup all simple-node-output ids.
        //this lets us fetch node-ids before they are actually emitted later on
        for cfg_node in cfg.nodes.values() {
            if let CfgNode::BasicBlock(bb) = cfg_node {
                for simple_node in &bb.nodes {
                    ctx.node_mapping.insert(simple_node.output(0), builder.id());
                }
            }
        }

        //Now start the CFG walker that emit our node _in-oreder_.
        self.serialize_cfg_node(ctx, builder, &cfg, &bb_label, cfg.root)
    }

    fn serialize_cfg_node(
        &self,
        ctx: &mut EmitCtx,
        builder: &mut Builder,
        cfg: &Cfg,
        bb_label: &AHashMap<CfgRef, Word>,
        node: CfgRef,
    ) -> Result<(), BackendSpirvError> {
        match cfg.nodes.get(node).unwrap() {
            CfgNode::Root(r) => {
                //begin the first block of the cfg
                builder
                    .begin_block(Some(*bb_label.get(r).unwrap()))
                    .unwrap();
                self.serialize_cfg_node(ctx, builder, cfg, bb_label, *r)
            }
            CfgNode::Null => Ok(()),
            CfgNode::BasicBlock(bb) => {
                for node in &bb.nodes {
                    let input_srcs = self
                        .graph
                        .node(*node)
                        .input_srcs(&self.graph)
                        .into_iter()
                        .map(|src| src.expect("Expected all srcs of the node to be connected!"))
                        .collect::<SmallColl<_>>();
                    let result_type = self
                        .get_single_node_result_type(*node)
                        .expect("Expected simple node to have retun type!");

                    match &self.graph.node(*node).node_type {
                        NodeType::Simple(s) => {
                            let _ = self.serialize_simple_node(
                                ctx,
                                builder,
                                &input_srcs,
                                &result_type,
                                &s.op,
                                *node,
                            )?;
                        }
                        NodeType::Apply(_a) => {
                            let _ = self.serialize_apply_node(
                                ctx,
                                builder,
                                &input_srcs,
                                &result_type,
                                *node,
                            )?;
                        }
                        other => panic!("Unexpected node type in basic block: {other}"),
                    }
                }

                //now recurse to the next node
                self.serialize_cfg_node(ctx, builder, cfg, bb_label, bb.exit_node)
            }
            CfgNode::LoopHeader {
                src_node,
                loop_entry_bb,
                ctrl_tail,
                pre_loop_bb,
            } => {
                //The loop header branches _into_ the loop-header,
                //then uses phi-nodes to select the actual value for each
                // loop variable and then branches into the body.
                //let loop_header_id = *bb_label.get(&node).unwrap();
                let loop_entry_id = *bb_label.get(loop_entry_bb).unwrap();
                builder.branch(loop_entry_id).unwrap();
                //begin the loop block
                builder.begin_block(Some(loop_entry_id)).unwrap();
                let lvcount = self
                    .graph
                    .node(*src_node)
                    .node_type
                    .unwrap_theta_ref()
                    .loop_variable_count();

                //build the phi nodes that'll flag out Loop-Variables
                //with the correct id by
                // first finding the pre-loop origin, and then the in-loop-origin.
                // use both to mark the loop-argument output with the generated result id of
                // the phi-instruction
                for i in 0..lvcount {
                    //NOTE: we pre-allocate the id for the argument, so if in_loop_origin
                    //      is also the argument, the id is already valid

                    let loop_variable_id = builder.id();
                    ctx.set_port_id(
                        OutportLocation {
                            node: *src_node,
                            output: OutputType::Argument(i),
                        },
                        loop_variable_id,
                    );
                }

                let pre_loop_bb_id = *bb_label.get(pre_loop_bb).unwrap();
                let (last_loop_bb, post_loop_bb, header_bb) = {
                    if let CfgNode::LoopCtrlTail {
                        last_bb,
                        post_loop_bb,
                        header,
                        ..
                    } = cfg.nodes.get(*ctrl_tail).unwrap()
                    {
                        (last_bb, post_loop_bb, header)
                    } else {
                        panic!("Expected this to be ctrl tail actually");
                    }
                };

                let post_loop_bb_id = *bb_label.get(&post_loop_bb).unwrap();
                //let ctrl_bb_id = *bb_label.get(&ctrl_tail).unwrap();
                let last_loop_bb_id = *bb_label.get(&last_loop_bb).unwrap();
                for i in 0..lvcount {
                    let pre_loop_origin = self
                        .graph
                        .find_producer_inp(InportLocation {
                            node: *src_node,
                            input: InputType::Input(i),
                        })
                        .unwrap();
                    let in_loop_origin = self
                        .graph
                        .find_producer_inp(InportLocation {
                            node: *src_node,
                            input: InputType::Result(i),
                        })
                        .unwrap();

                    let pre_loop_origin_id = *ctx.node_mapping.get(&pre_loop_origin).unwrap();
                    let in_loop_origin_id = *ctx.node_mapping.get(&in_loop_origin).unwrap();
                    let argument_id = *ctx
                        .node_mapping
                        .get(&OutportLocation {
                            node: *src_node,
                            output: OutputType::Argument(i),
                        })
                        .unwrap();
                    //NOTE: currently theta can only produce one outpu
                    let result_type = self
                        .find_type(
                            OutportLocation {
                                node: *src_node,
                                output: OutputType::Argument(i),
                            }
                            .into(),
                        )
                        .unwrap();
                    let result_type_id = register_or_get_type(builder, ctx, &result_type);
                    //now append phi- for both
                    builder
                        .phi(
                            result_type_id,
                            Some(argument_id),
                            [
                                (pre_loop_origin_id, pre_loop_bb_id),
                                (in_loop_origin_id, last_loop_bb_id),
                            ],
                        )
                        .unwrap();
                }
                //recurse into the loop body
                self.serialize_cfg_node(ctx, builder, cfg, bb_label, *loop_entry_bb)
            }
            CfgNode::LoopCtrlTail {
                last_bb,
                loop_entry_bb,
                post_loop_bb,
                condition_src,
                header,
                src_node,
            } => {
                //after the loop body, add the merge and conditional branch
                // _outside_ of the loop body. Then start the next block
                //and recurse

                let loop_entry_id = *bb_label.get(loop_entry_bb).unwrap();
                //let loop_header_id = *bb_label.get(header).unwrap();
                let post_loop_id = *bb_label.get(post_loop_bb).unwrap();
                let last_bb_id = *bb_label.get(last_bb).unwrap();
                let condition_id = *ctx.node_mapping.get(condition_src).unwrap();

                //Placing the loop merge is somewhat complex. So the rules
                //says, that the loop header is the block that ends on the loop-merge+branch
                // In case of a single block loop, thats the same as the loop-end
                //
                // in the multi-block case however, we have to make sure that the
                // loopmerge is in front of a unconditional branch into the loop body.
                // so what we do is, we

                let current_block = builder.selected_block();
                let entry_bb_index = Self::find_block_index_for_id(builder, loop_entry_id).unwrap();
                builder.select_block(Some(entry_bb_index)).unwrap();
                //iff we insert the merge into the same block, don't offest, since the cond-branch is not yet
                //appended
                let offset = if current_block == Some(entry_bb_index) {
                    0
                } else {
                    1
                };
                builder
                    .insert_into_block(
                        rspirv::dr::InsertPoint::FromEnd(offset),
                        Instruction::new(
                            rspirv::spirv::Op::LoopMerge,
                            None,
                            None,
                            vec![
                                Operand::IdRef(post_loop_id),
                                Operand::IdRef(last_bb_id),
                                Operand::LoopControl(LoopControl::empty()),
                            ],
                        ),
                    )
                    .unwrap();
                //change back to the currently build block
                builder.select_block(current_block).unwrap();

                builder
                    .branch_conditional(condition_id, loop_entry_id, post_loop_id, [])
                    .unwrap();
                //mapping from each connected lv_output to its loop-internal src if there is any

                let lvcount = self
                    .graph
                    .node(*src_node)
                    .node_type
                    .unwrap_theta_ref()
                    .loop_variable_count();
                for lv in 0..lvcount {
                    if self
                        .graph
                        .node(*src_node)
                        .node_type
                        .unwrap_theta_ref()
                        .lv_output(lv)
                        .unwrap()
                        .edges
                        .len()
                        == 0
                    {
                        continue;
                    }

                    let in_loop_src = self
                        .graph
                        .find_producer_inp(InportLocation {
                            node: *src_node,
                            input: InputType::Result(lv),
                        })
                        .expect("If the loop output is used, a result is expected");
                    let src_id = *ctx.node_mapping.get(&in_loop_src).unwrap();
                    ctx.node_mapping.insert(
                        OutportLocation {
                            node: *src_node,
                            output: OutputType::Output(lv),
                        },
                        src_id,
                    );
                }

                builder.begin_block(Some(post_loop_id)).unwrap();
                //recurse to next block
                self.serialize_cfg_node(ctx, builder, cfg, bb_label, *post_loop_bb)
            }
            CfgNode::BranchHeader {
                src_node,
                condition_src,
                last_bb,
                true_branch,
                false_branch,
                merge,
                post_merge_block,
            } => {
                //the branch takes care of
                //setting up the branch-local context for each,
                //as well as
                let branch_header_id = *bb_label.get(&node).unwrap();
                let branch_merge_id = *bb_label.get(&merge).unwrap();
                let conditional_src_id = ctx.get_port_id(condition_src).unwrap();
                let true_branch_id = *bb_label.get(true_branch).unwrap();
                let false_branch_id = *bb_label.get(false_branch).unwrap();
                let post_merge_bb_id = *bb_label.get(post_merge_block).unwrap();

                //branch into the branch header
                builder.branch(branch_header_id).unwrap();
                builder.begin_block(Some(branch_header_id)).unwrap();

                //recursing first into the branches, and then into the merge branch
                builder
                    .selection_merge(branch_merge_id, SelectionControl::empty())
                    .unwrap();
                let _cond_branch = builder
                    .branch_conditional(
                        conditional_src_id,
                        *bb_label.get(true_branch).unwrap(),
                        *bb_label.get(false_branch).unwrap(),
                        [],
                    )
                    .unwrap();

                let ev_count = self
                    .graph
                    .node(*src_node)
                    .node_type
                    .unwrap_gamma_ref()
                    .entry_var_count();
                //setup the entry-var id for each argument.
                for ev in 0..ev_count {
                    let src = self
                        .graph
                        .find_producer_inp(InportLocation {
                            node: *src_node,
                            input: InputType::EntryVariableInput(ev),
                        })
                        .unwrap();

                    let src_id = *ctx.node_mapping.get(&src).unwrap();
                    for bidx in 0..2 {
                        ctx.node_mapping.insert(
                            OutportLocation {
                                node: *src_node,
                                output: OutputType::EntryVariableArgument {
                                    branch: bidx,
                                    entry_variable: ev,
                                },
                            },
                            src_id,
                        );
                    }
                }

                builder.begin_block(Some(true_branch_id)).unwrap();
                self.serialize_cfg_node(ctx, builder, cfg, bb_label, *true_branch)?;
                //end last block with an unconditional jump to the merge label
                builder.begin_block(Some(false_branch_id)).unwrap();
                self.serialize_cfg_node(ctx, builder, cfg, bb_label, *false_branch)?;

                //now begin the merge block, that ties the whole thing back up,
                //by using phi nodes
                builder.begin_block(Some(branch_merge_id)).unwrap();

                let (last_true_bb, last_false_bb) = {
                    if let CfgNode::BranchMerge {
                        src_true,
                        src_false,
                        ..
                    } = cfg.nodes.get(*merge).unwrap()
                    {
                        (*src_true, *src_false)
                    } else {
                        panic!("Should be merge")
                    }
                };
                let last_true_bb_id = *bb_label.get(&last_true_bb).unwrap();
                let last_false_bb_id = *bb_label.get(&last_false_bb).unwrap();

                //for each connected output of the gamma-node,
                //setup the exit value by
                // _finding_
                // both branches producer nodes
                //
                let exit_var_count = self
                    .graph
                    .node(*src_node)
                    .node_type
                    .unwrap_gamma_ref()
                    .exit_var_count();
                //preset all ev_output ids
                for ev in 0..exit_var_count {
                    ctx.node_mapping.insert(
                        OutportLocation {
                            node: *src_node,
                            output: OutputType::ExitVariableOutput(ev),
                        },
                        builder.id(),
                    );
                }
                //now collect all ev connected ev ports
                for ev in 0..exit_var_count {
                    let ev_location = OutportLocation {
                        node: *src_node,
                        output: OutputType::ExitVariableOutput(ev),
                    };
                    //Do nothing if the exit variable is not connected
                    if self
                        .graph
                        .node(ev_location.node)
                        .outport(&ev_location.output)
                        .unwrap()
                        .edges
                        .len()
                        == 0
                    {
                        continue;
                    }
                    println!("Try {ev_location:?}");
                    let in_true_src = self
                        .graph
                        .find_producer_inp(InportLocation {
                            node: *src_node,
                            input: InputType::ExitVariableResult {
                                branch: 0,
                                exit_variable: ev,
                            },
                        })
                        .unwrap();
                    println!("true_src: {in_true_src:?}");
                    let in_true_src_id = *ctx.node_mapping.get(&in_true_src).unwrap();

                    let in_false_src = self
                        .graph
                        .find_producer_inp(InportLocation {
                            node: *src_node,
                            input: InputType::ExitVariableResult {
                                branch: 1,
                                exit_variable: ev,
                            },
                        })
                        .unwrap();
                    let in_false_src_id = *ctx.node_mapping.get(&in_false_src).unwrap();
                    //now write the phi node into the builder with the given
                    //id

                    let phi_id = *ctx.node_mapping.get(&ev_location).unwrap();
                    let result_type = self.find_type(ev_location.into()).unwrap();
                    let result_type_id = register_or_get_type(builder, ctx, &result_type);
                    builder
                        .phi(
                            result_type_id,
                            Some(phi_id),
                            [
                                (in_true_src_id, last_true_bb_id),
                                (in_false_src_id, last_false_bb_id),
                            ],
                        )
                        .unwrap();
                }

                //finally start the post_merge block
                builder.branch(post_merge_bb_id).unwrap();
                builder.begin_block(Some(post_merge_bb_id)).unwrap();
                //finally continue with the post-merge block
                self.serialize_cfg_node(ctx, builder, cfg, bb_label, *post_merge_block)
            }
            CfgNode::BranchMerge {
                src_node,
                src_true,
                src_false,
                next,
            } => {
                //tell whoever serializes this, that we need to branch to the
                //output
                builder.branch(*bb_label.get(&node).unwrap()).unwrap();

                //Merge of branches doesn recurse, instead we let the header controll recursion
                //this basically builds a dfs over all branches when building the spirv-module
                Ok(())
            }
        }
    }

    //Tries to find the block that is labeled by id within the currently selected function
    fn find_block_index_for_id(builder: &Builder, id: Word) -> Option<usize> {
        let fidx = builder.selected_function().unwrap();
        for (bidx, block) in builder
            .module_ref()
            .functions
            .get(fidx)
            .unwrap()
            .blocks
            .iter()
            .enumerate()
        {
            if block.label_id() == Some(id) {
                return Some(bidx);
            }
        }

        None
    }

    //Serializes the `simple_node` into the current builder. Assumes that the caller makes sure that all
    //`input_srcs` have been serialized and are accessible by the current builder location / active block.
    fn serialize_simple_node(
        &self,
        ctx: &mut EmitCtx,
        builder: &mut Builder,
        input_srcs: &SmallColl<OutportLocation>,
        result_type: &SpvType,
        simple_node: &BackendOp,
        node: NodeRef,
    ) -> Result<Word, BackendSpirvError> {
        match simple_node {
            BackendOp::SpirvOp(spvop) => {
                println!("{} -> {}", node, spvop.name());
                //Collect the SPIR-V ids of all inputs
                let src_ids = input_srcs
                    .iter()
                    .map(|node| *ctx.node_mapping.get(node).unwrap())
                    .collect::<SmallColl<Word>>();

                let result_id = *ctx.node_mapping.get(&node.output(0)).unwrap();
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
                //insert int emit_ctx
                ctx.node_mapping.insert(node.output(0), result_id);
                Ok(result_id)
            }
            BackendOp::HlOp(o) => panic!("Unexpected HlOp in SPIR-V serialization: {o:?}"),
            BackendOp::Dummy => panic!("Unexpected Dummy node in SPIR-V serialization"),
        }
    }

    fn serialize_apply_node(
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
        //allways translates to a call.
        //The region construction allready took care of forwarding the λ-def to the cv-ports,
        //so we really just have to map 1:1 to the call.
        let result_type_id = *ctx.type_mapping.get(result_type).unwrap();

        //NOTE: remove the first, since we resolve the call_id ourselfs below
        let _ = src_ids.remove(0);

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

        let result_id = *ctx.node_mapping.get(&node.output(0)).unwrap();
        let _ = builder
            .function_call(result_type_id, Some(result_id), call_id, src_ids)
            .unwrap();
        Ok(result_id)
    }
    /*
        fn serialize_node(
            &self,
            ctx: &mut EmitCtx,
            builder: &mut Builder,
            input_srcs: &SmallColl<OutportLocation>,
            result_type: &SpvType,
            parent_region: RegionLocation,
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
                    BackendOp::HlOp(o) => panic!("Unexpected HlOp in SPIR-V serialization: {o:?}"),
                    BackendOp::Dummy => panic!("Unexpected Dummy node in SPIR-V serialization"),
                },
                NodeType::Apply(_a) => {}
                NodeType::Gamma(g) => {
                    //Build the gamma-region scope for each branch, and then recurse the branch.
                    //After all of them returning,
                    //append the (SPIR-V)-phi node and return the result id.

                    assert!(g.regions().len() == 2);
                    //NOTE: all branches have the same result type, always.
                    let result_type_id = *ctx.type_mapping.get(result_type).unwrap();
                    let merge_label_id = builder.id();
                    let _selection_merge_id = builder
                        .selection_merge(merge_label_id, SelectionControl::NONE)
                        .unwrap();
                    let if_label = builder.id();
                    let else_label = builder.id();
                    //TODO: implement some kind of static gamma-analysis to have nice branch weights.
                    let _cond_branch = builder
                        .branch_conditional(src_ids[0], if_label, else_label, [])
                        .unwrap();

                    //This is the Vec<(branch_result_id, branch_block_id)> used by op phi, but already flattened
                    //the way _it isi needed_.
                    let mut result_parent_pairs: SmallColl<(u32, u32)> = SmallVec::new();
                    //now, for both gamma-blocks: Setup the jump-label,
                    //then emit the block, finally, emit the unconditional branch to the merge_label
                    for (blockid, index) in [(if_label, 0), (else_label, 1)] {
                        let gamma_region = RegionLocation {
                            node,
                            region_index: index,
                        };
                        let block_entry_label = builder.begin_block(Some(blockid)).unwrap();
                        ctx.push_block_id(gamma_region, block_entry_label);
                        //make it possible for the emit_region to resolve,
                        //by seeding all entry_variabels
                        for idx in 1..src_ids.len() {
                            //map the entry_var_input idx-1 to entry_var_argument
                            ctx.node_mapping.insert(
                                OutportLocation {
                                    node,
                                    output: OutputType::EntryVariableArgument {
                                        branch: index,
                                        entry_variable: idx - 1,
                                    },
                                },
                                src_ids[idx],
                            );
                        }

                        self.emit_region(ctx, builder, gamma_region)
                            .expect("Expected return value from gamma-region");

                        //Retrive the result_id, by looking it up in the just used ctx
                        let result_connected_port = self
                            .graph
                            .region(&gamma_region)
                            .unwrap()
                            .result_src(&self.graph, 0)
                            .unwrap();

                        let result_id = *ctx.node_mapping.get(&result_connected_port).unwrap();
                        //read out the last block id of our own region, and use that as src-label
                        //for the following _out-of-branch_ phi-instruction.
                        let exit_block_id = ctx
                            .region_blocks
                            .get(&gamma_region)
                            .unwrap()
                            .last()
                            .cloned()
                            .unwrap();

                        //NOTE: We need to reconstruct the basic-block id of result_id, since we _don't know_
                        //      what emit_region above might have done basic-block wise.
                        //      so the idea is, to go to result_id's definition, and then reverse
                        //      until we find the def.
                        //
                        //      A nice thing is, that we can use the blockid pre-given as a _lower-bound_.
                        //      so... thats nice I guess.
                        result_parent_pairs.push((result_id, exit_block_id));
                        //branch to the merge label
                        builder.branch(merge_label_id).unwrap();
                    }

                    //finally, append the merge label, and the phi, that'll emit our actual _created_ value.
                    let _ = builder.begin_block(Some(merge_label_id)).unwrap();
                    ctx.push_block_id(parent_region, merge_label_id);
                    let resid = builder
                        .phi(result_type_id, None, result_parent_pairs)
                        .unwrap();

                    //also register the associated _out_of_gamma_port_
                    //with the id
                    ctx.node_mapping.insert(
                        OutportLocation {
                            node,
                            output: OutputType::ExitVariableOutput(0),
                        },
                        resid,
                    );
                    Ok(resid)
                }
                NodeType::Theta(t) => {
                    //We basically build a do-while thingy here.
                    //which works by branching into the loop block, then selecting the
                    //loop index and loop-variable based on
                    //weather we come from the last iteration, or not.
                    //we then just append the whole block,
                    //and finally branch of, based on the condition
                    //that was emitted.

                    let loop_body_label = builder.id();
                    let loop_merge_label = builder.id();
                    //The id given to the loop index after it changed
                    let loop_index_id = builder.id();
                    let loop_index_src = src_ids[0];

                    //The loop dominating block id
                    let pre_loop_block_id = *ctx
                        .region_blocks
                        .get(&parent_region)
                        .unwrap()
                        .last()
                        .unwrap();
                    let theta_region = RegionLocation {
                        node,
                        region_index: 0,
                    };
                    let _ = builder.branch(loop_body_label).unwrap();
                    let _into_body_label = builder.begin_block(Some(loop_body_label)).unwrap();
                    ctx.push_block_id(theta_region, loop_body_label);

                    //NOTE: for each used LV, use a phi-instruction to select the correct value _within_ the body
                    //      however, at this point we don't know (yet) the _in-loop_ id for those. So what we do is
                    //      pre-allocate an id for each loop-variable which gets associated with the lv-port
                    //      then we build the block, and afterwards prepend the phi instructions with the newly allocated
                    //      IDs to the start of loop_body_label.
                    //      The out-of-loop id will be the src_id[lv_idx], the _in_loop_ id wil be the _last_use_id_ that is connected to the
                    //      respective lv_result
                    let loop_body_label_block_index = builder.selected_block().unwrap();

                    for i in 0..t.loop_variable_count() {
                        let loop_variable_id = builder.id();
                        ctx.node_mapping.insert(
                            OutportLocation {
                                node,
                                output: OutputType::Argument(i),
                            },
                            loop_variable_id,
                        );
                    }

                    //now emit the body
                    self.emit_region(ctx, builder, theta_region)?;

                    //now collect all result_connected lvs and prepend the phi-node to the start of the
                    //loop_body

                    let post_loop_block = builder.selected_block().unwrap();
                    builder
                        .select_block(Some(loop_body_label_block_index))
                        .unwrap();
                    for i in 0..t.loop_variable_count() {
                        let pre_loop_id = src_ids[i];
                        //early out if the result is not connected
                        if t.lv_result(i).unwrap().edge.is_none() {
                            continue;
                        }
                        let in_loop_src = self
                            .graph
                            .edge(t.lv_result(i).unwrap().edge.unwrap())
                            .src()
                            .clone();
                        let in_loop_id = *ctx.node_mapping.get(&in_loop_src).unwrap();
                        //get the pre-allocated id back.
                        let phi_id = *ctx
                            .node_mapping
                            .get(&OutportLocation {
                                node,
                                output: OutputType::Argument(i),
                            })
                            .unwrap();
                        let typeid = {
                            //recover the type by checking the result-connected edge type
                            //FIXME: we might want to unify that with the lv-input connected edge type.
                            //       but in theory this should be already done by the type pass.
                            let edgety = self
                                .graph
                                .edge(t.lv_result(i).unwrap().edge.unwrap())
                                .ty
                                .get_type()
                                .unwrap();
                            register_or_get_type(builder, ctx, &edgety)
                        };
                        builder
                            .insert_into_block(
                                rspirv::dr::InsertPoint::Begin,
                                Instruction::new(
                                    rspirv::spirv::Op::Phi,
                                    Some(typeid),
                                    Some(phi_id),
                                    vec![
                                        //Use the pre-loop-id, if we come from pre-loop-block-id
                                        Operand::IdRef(pre_loop_id),
                                        Operand::IdRef(pre_loop_block_id),
                                        //Use the in_loop_id, if we come from the loop body
                                        Operand::IdRef(in_loop_id),
                                        Operand::IdRef(loop_body_label),
                                    ],
                                ),
                            )
                            .unwrap();
                    }
                    //return to the end of the theta-region related blocks
                    builder.select_block(Some(post_loop_block)).unwrap();

                    //finally, merge the loop and _out_of_loop_ flow and branch based on the predicate
                    let predicate_port = self.graph.edge(t.loop_predicate().edge.unwrap()).src();
                    let predicate_id = *ctx.node_mapping.get(predicate_port).unwrap();

                    builder
                        .loop_merge(loop_merge_label, loop_body_label, LoopControl::empty(), [])
                        .unwrap();
                    builder
                        .branch_conditional(predicate_id, loop_body_label, loop_merge_label, [])
                        .unwrap();

                    //finally start post-theta block
                    builder.begin_block(Some(loop_merge_label)).unwrap();
                    ctx.push_block_id(parent_region, loop_merge_label);
                    //And return the loop-value. Note that, by convention this is always the
                    // 3rd output of the loop
                    //so we just use the 3rd-result-connected-node.
                    //Also, we select the right loop value after branching _into_ the loop, since this is a do-while loop, not a while-do. So
                    //basically at this point the result connected node is _already_ the right one.
                    let loop_value_src = self
                        .graph
                        .edge(t.lv_result(2).unwrap().edge.unwrap())
                        .src()
                        .clone();
                    let loop_value_id = *ctx.node_mapping.get(&loop_value_src).unwrap();
                    //NOTE: Also write that to the result id, if someone else want's to use / query
                    //      the result port
                    ctx.node_mapping.insert(
                        OutportLocation {
                            node,
                            output: OutputType::Output(2),
                        },
                        loop_value_id,
                    );
                    Ok(loop_value_id)
                }
                //TODO: implement ifs / matches and loops so we could _in principle_ emit those.
                any => panic!("Unsupported node type {any:?}"),
            }
        }
    */
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
                ArithBaseTy::Bool => builder.type_bool(),
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
