/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # Field dispatch
//!
//! This is one of the most important parts of the optimizer. It transforms the tree-structure of the compiled field into an call-graph
//! that can actually be _used_ by a CPU/GPU-like IR.
//!
//! There are several several stages to the pass
//!
//!
//! For each [TreeAccess](crate::csg::TreeAccess) node create a _dispathed_ λ-Node, and replace the TreeAccess with a call to that node
//!     For each node in the connected tree:
//!         1 Specialize node
//!             a) (CSGOp) inline the implementation of that nodes's concept
//!             b) (CSGCall) inline the called field def compleatly, then update the sub tree node with the inlined tree
//!         2. import _original_ values from the export_fn (if needed) by adding an argument to the λ-Node, and hooking it up in the export_fn to the apply-node
//!
//NOTE: Right now the dispatcher is pretty "dump". Another version would be, to avoid inlining, by importing the λ-Nodes via CVs, and hooking up the tree-specified
//      correct λ-Nodes to their CVs instead.
//      However, in that case we'd stilly have to duplicate the λ-Nodes, since we have to hook-up the CVs based on some
//      tree-definition.
//
//      Right now this way works, and has the added benefit, that one tree-call has _all the formulas_ in one place. To we could do ✨optimization✨ on
//      the _whole_ thing if wanted. Local optimization can still be done _before_ specializing.
//
//
//      Another thing that should be considered. We could fuse FieldAccess with a common tree. This would allow us to practically evaluate the same tree with multiple
//      concepts _at once_. As specially with common-node elimination this might create interesting optimization opportunities.
//
//      Yet another idea is, to first specialise into a meta-tree that already resolved the sub-calls in each implblock, then resolve those to the actual lambda

use std::env;

use ahash::AHashMap;
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::{self, LangNode, Node, StructuralNode},
    region::{Inport, RegionLocation},
    smallvec::{smallvec, SmallVec},
    util::copy::StructuralClone,
    EdgeRef, NodeRef,
};
use vola_common::{report, Span};

use crate::{
    alge::DummyNode, csg::TreeAccess, error::OptError, OptEdge, OptNode, Optimizer, TypeState,
};

struct SpecializationContext {
    concept_name: String,
    //The span of the TreeAccess node based uppon we specialize.
    specialization_src: Span,
}

impl Optimizer {
    ///dispatches all field_exports that are currently registered in the optimizer.
    pub fn dispatch_all_exports(&mut self) -> Result<(), OptError> {
        let mut num_errors = 0;
        let exports = self
            .export_fn
            .iter()
            .map(|(ident, exp)| (ident.clone(), exp.span.clone()))
            .collect::<Vec<_>>();
        for (ident, span) in exports {
            if let Err(err) = self.dispatch_export(&ident) {
                report(err, span.get_file());
                num_errors += 1;
            }
        }

        if num_errors > 0 {
            Err(OptError::Any {
                text: format!("Failed to dispatch all fields with {num_errors} errors"),
            })
        } else {
            Ok(())
        }
    }

    fn trace_copy_node(&mut self, srcnode: NodeRef) -> NodeRef {
        let src_node_region = self.graph.node(srcnode).parent.unwrap();

        assert!(self.graph.node(src_node_region.node).node_type.is_lambda());

        let parent_region = self.graph.node(src_node_region.node).parent.unwrap();
        //Right now this _should_ hold true
        assert!(parent_region == self.graph.toplevel_region());
        let dst_region_lambda = self
            .graph
            .shallow_copy_node(src_node_region.node, parent_region);

        let dst_region = RegionLocation {
            node: dst_region_lambda,
            region_index: 0,
        };

        //NOTE: we set the λ-Node to have only one output atm.
        self.graph.region_mut(&dst_region).unwrap().results = smallvec![Inport::default(); 1];

        let mut node_mapping = AHashMap::new();
        //push λ-remap
        node_mapping.insert(src_node_region.node, dst_region_lambda);
        let mut predecessors: Vec<_> = self
            .graph
            .walk_predecessors(srcnode)
            .map(|pred| pred.node.clone())
            .collect();

        //push our src node into the predecessors as well
        predecessors.push(srcnode);

        for pred in &predecessors {
            //insert all predecessors into the new region, and add to mapping
            if !node_mapping.contains_key(pred) {
                let deep_copy = self.graph.deep_copy_node(*pred, dst_region);
                node_mapping.insert(*pred, deep_copy);
            }
        }

        //After deep_copying all nodes, rebuild the edges.
        let all_src_edges = self.graph.region(&src_node_region).unwrap().edges.clone();
        for edgeref in all_src_edges {
            let (mut src, mut dst, ty) = {
                let edg = self.graph.edge(edgeref);
                (edg.src().clone(), edg.dst().clone(), edg.ty.clone())
            };

            //If we have the src of that edge in our mapping, copy the edge compleatly
            if node_mapping.contains_key(&src.node) && node_mapping.contains_key(&dst.node) {
                src.node = *node_mapping.get(&src.node).unwrap();
                dst.node = *node_mapping.get(&dst.node).unwrap();

                //Overwrite result ports, since we have only one, and the TreeAccess is expected
                //to only return one result.
                if dst.input.is_result() {
                    dst.input = InputType::Result(0);
                }

                //Now connect
                self.graph.connect(src, dst, ty).unwrap();
            }
        }

        //Before we can finish, we have to handle the
        // _outside_ connection of our srcnode, or more specifically, the context variables. So iter
        // those as well, and reconcet them with outrs

        let cvcount = self
            .graph
            .node(src_node_region.node)
            .node_type
            .unwrap_lambda_ref()
            .context_variable_count();
        for cv in 0..cvcount {
            let (cvsrc, cv_edge_ty) = {
                if let Some(edg) = self
                    .graph
                    .node(src_node_region.node)
                    .node_type
                    .unwrap_lambda_ref()
                    .cv_input(cv)
                    .unwrap()
                    .edge
                {
                    let edg = self.graph.edge(edg);
                    (edg.src().clone(), edg.ty.structural_copy())
                } else {
                    //Was not connected, so ignore
                    continue;
                }
            };

            let dst = InportLocation {
                node: dst_region.node,
                input: InputType::ContextVariableInput(cv),
            };
            self.graph.connect(cvsrc, dst, cv_edge_ty).unwrap();
        }

        //return new lambda
        dst_region_lambda
    }

    ///Runs the actual specialization of an region.
    fn specialize_region(&mut self, region: RegionLocation) -> Result<(), OptError> {
        //we specialize the region by recursively calling _csg_-dialect nodes
        //with _specialize-node_ function. However, first we need to read out the field access
        //node, so we know our entry condition / entry-concept. The AccessField node itself will be removed in the
        //process, since we'll hook up the first specialized_node() to the output afterwards.

        //soo, for the thingy to work, we first inline all _calls_ into the region. Which are calls to field_defs atm.
        //we do that by just iterating _all_ nodes, and if its an apply-node,
        // and if it is, call the rvsdg native inliner.

        {
            let all_nodes = self.graph.region(&region).unwrap().nodes.clone();
            for node in all_nodes {
                if self.graph.node(node).node_type.is_apply() {
                    self.graph.inline_apply_node(node).unwrap();
                }
            }
        }

        //TODO: remove and do all the other stuff
        return Ok(());
        let access_field_node_port = self
            .graph
            .region(&region)
            .unwrap()
            .result_src(&self.graph, 0)
            .unwrap();

        let (entry_concept, entry_csg_op, entry_arguments) = {
            assert!(
                self.graph
                    .node(access_field_node_port.node)
                    .node_type
                    .unwrap_simple_ref()
                    .node
                    .dialect()
                    == "csg",
                "Expected csg node at pruned-result"
            );
            let tree_access_node = self
                .graph
                .node(access_field_node_port.node)
                .node_type
                .unwrap_simple_ref()
                .try_downcast_ref::<TreeAccess>()
                .unwrap();
            //Sort out what the entry op is, and what the args are.
            //by definition
            (
                tree_access_node.called_concept.clone(),
                tree_access_node.get_op_edge(),
                tree_access_node.get_args(),
            )
        };
        let access_span = self
            .graph
            .node(access_field_node_port.node)
            .node_type
            .unwrap_simple_ref()
            .span
            .clone();
        let spec_ctx = SpecializationContext {
            concept_name: entry_concept.clone(),
            specialization_src: access_span,
        };

        let entry_node_port = self
            .graph
            .edge(entry_csg_op.expect("expected csgop to be connected!"))
            .src();
        let specialized_entry_node = self.specialize_node(&spec_ctx, entry_node_port.node)?;
        //reconncet output to the resulting node. This will effectively render all CSG-Nodes _dead_.
        let result_edge = self.graph.region(&region).unwrap().results[0]
            .edge
            .clone()
            .unwrap();
        self.graph.disconnect(result_edge).unwrap();
        //now connect the specialized_entry_node to that output
        self.graph
            .on_region(&region, |reg| {
                reg.connect_to_result(specialized_entry_node.output(0), InputType::Result(0))
                    .unwrap()
            })
            .unwrap();
        Ok(())
    }

    ///Inlines the `callnode` by querying the optimizer for that field def, and if found, deep-copying
    //the field content to our region. Afte that we
    fn inline_csg_call(&mut self, callnode: NodeRef) -> Result<NodeRef, OptError> {
        todo!()
    }

    fn specialize_node(
        &mut self,
        ctx: &SpecializationContext,
        node: NodeRef,
    ) -> Result<NodeRef, OptError> {
        //At first we need to find out what kind of op this is. If it has no csg-dialect predecessors,
        // it is either a CSGOp or CSGCall.
        // If it has any csg-dialect predecessors, it will always be a CsgOp.
        // So our first action is to scrape the inputs of that node for
        // csg and non-csg arguments. Right now the CSG-Args have to be the first ones,
        //so we try that first. If we find any CSG-Arg _after_ the first alge_arg, we bail.

        let mut csg_args: SmallVec<[OutportLocation; 3]> = SmallVec::default();
        let mut alge_args: SmallVec<[OutportLocation; 3]> = SmallVec::default();
        let mut saw_alge_arg = false;
        let argcount = self.graph.node(node).inputs().len();
        for inidx in 0..argcount {
            if let Some(arg_edge) = self.graph.node(node).inputs()[inidx].edge {
                let src = self.graph.edge(arg_edge).src().clone();
                //check out the argument type
                match self
                    .graph
                    .node(src.node)
                    .node_type
                    .unwrap_simple_ref()
                    .node
                    .dialect()
                {
                    "csg" => {
                        //If we already saw an alge arg, the call convention is violated. Shouldn't happen,
                        //but since we are in the "[...] around and find out" stage of the project, test it anyways.
                        if saw_alge_arg {
                            let argnodespan = self
                                .graph
                                .node(src.node)
                                .node_type
                                .unwrap_simple_ref()
                                .span
                                .clone();
                            let err = OptError::AnySpannedWithSource {
                            source_span: ctx.specialization_src.clone().into(),
                            source_text: format!("While specializing for this"),
                            text: format!("Argument {inidx} was of csg-dialect, after already seeing algebraic arguments. This is a call-convention violation..."),
                            span: argnodespan.into(),
                            span_text: format!("This is the argument node "),
                            };
                            report(err.clone(), ctx.specialization_src.get_file());
                            return Err(err);
                        }
                        csg_args.push(src);
                    }
                    "alge" => {
                        //change to alge args if not done so already
                        saw_alge_arg = true;
                        alge_args.push(src);
                    }
                    e => {
                        let argnodespan = self
                            .graph
                            .node(src.node)
                            .node_type
                            .unwrap_simple_ref()
                            .span
                            .clone();
                        let err = OptError::AnySpannedWithSource {
                            source_span: ctx.specialization_src.clone().into(),
                            source_text: format!("While specializing for this"),
                            text: format!("Argument {inidx} was of unexpected dialect \"{e}\""),
                            span: argnodespan.into(),
                            span_text: format!("This is the argument node "),
                        };
                        report(err.clone(), ctx.specialization_src.get_file());
                        return Err(err);
                    }
                }
            } else {
                let argnodespan = self
                    .graph
                    .node(node)
                    .node_type
                    .unwrap_simple_ref()
                    .span
                    .clone();
                let err = OptError::AnySpannedWithSource {
                    source_span: ctx.specialization_src.clone().into(),
                    source_text: format!("While specializing for this"),
                    text: format!("Argument {inidx} was not specified"),
                    span: argnodespan.into(),
                    span_text: format!("While specializing this"),
                };
                report(err.clone(), ctx.specialization_src.get_file());
                return Err(err);
            }
        }

        //alright, sorted out the csg and alge-args. now continue by dispatching to the right kind of specialization. We've got
        // - CSGEntity Specialization: For CSGOps with no sub tree
        // - CSGCall Specialization: For CSGCalls (also no sub-trees),
        // - CSGOpertaion: For CSGOps with 1..n sub trees.
        //
        // in each case we first checkout that the argument count matches, then we
        todo!("implement the CSGOP only dispatch");
        Ok(node)
    }

    fn dispatch_csg_operation(
        &mut self,
        ctx: &SpecializationContext,
        srcnode: NodeRef,
        subtrees: SmallVec<[OutportLocation; 3]>,
        arguments: SmallVec<[OutportLocation; 3]>,
    ) -> Result<NodeRef, OptError> {
        todo!()
    }

    ///Dispatches the export_field with the given `ident`ifier.
    pub fn dispatch_export(&mut self, ident: &str) -> Result<(), OptError> {
        //Find all TreeAccess nodes, replace the node with the

        //Right now just copy
        let (copy_node_list, export_fn_region, argcount) =
            if let Some(exp) = self.export_fn.get(ident) {
                let mut copy_nodes: SmallVec<[NodeRef; 3]> = SmallVec::default();
                let result_count = self.graph.region(&exp.lambda_region).unwrap().results.len();
                for i in 0..result_count {
                    if let Some(port) = self
                        .graph
                        .region(&exp.lambda_region)
                        .unwrap()
                        .result_src(&self.graph, i)
                    {
                        copy_nodes.push(port.node);
                    }
                }

                let argcount = self
                    .graph
                    .node(exp.lambda)
                    .node_type
                    .unwrap_lambda_ref()
                    .argument_count();
                (copy_nodes, exp.lambda_region.clone(), argcount)
            } else {
                //No such export fn
                return Err(OptError::Any {
                    text: format!("No export function named {} found!", ident),
                });
            };

        //we know the list of field accesses, so first things first, to a trace-copy
        //of all of them.
        let mut access_new_lambda_pairs: SmallVec<[(NodeRef, NodeRef); 3]> = SmallVec::default();
        for node in copy_node_list {
            let new_lambda = self.trace_copy_node(node);
            access_new_lambda_pairs.push((node, new_lambda));
        }

        for (src_field_access, new_lambda) in &access_new_lambda_pairs {
            //At this point we build the correct λ-Node, now import into the
            //original import location, delete `node`, import `new_lambda`
            //and call it with all arguments to the export_fn.

            let import_cv = self
                .graph
                .node_mut(export_fn_region.node)
                .node_type
                .unwrap_lambda_mut()
                .add_context_variable();
            let import_cv_port_in = InportLocation {
                node: export_fn_region.node,
                input: InputType::ContextVariableInput(import_cv),
            };
            let import_cv_port_out = OutportLocation {
                node: export_fn_region.node,
                output: OutputType::ContextVariableArgument(import_cv),
            };
            self.graph
                .connect(
                    OutportLocation {
                        node: *new_lambda,
                        output: OutputType::LambdaDeclaration,
                    },
                    import_cv_port_in,
                    OptEdge::Value {
                        ty: TypeState::Unset,
                    },
                )
                .unwrap();
            //hook up the node internally

            let target_port = {
                assert!(
                    self.graph.node(*src_field_access).outputs().len() == 1,
                    "expected FieldAccess to have only one input!"
                );
                let targets = self
                    .graph
                    .node(*src_field_access)
                    .output_dsts(&self.graph, 0)
                    .unwrap()
                    .clone();
                assert!(targets.len() == 1);
                targets[0]
            };

            assert!(
                target_port.input.is_result(),
                "Expected accessField to be connected to a result port!"
            );
            //Since we saved the outport dst we can finally remove the node, and setup the new apply node.
            self.graph.remove_node(*src_field_access).unwrap();

            let input_args = (0..argcount)
                .map(|idx| OutportLocation {
                    node: export_fn_region.node,
                    output: OutputType::Argument(idx),
                })
                .collect::<SmallVec<[OutportLocation; 3]>>();
            self.graph.on_region(&export_fn_region, |reg| {
                let (call_node, _) = reg
                    .call(import_cv_port_out, &input_args)
                    .expect("Failed to hook up replacement Apply node");

                reg.ctx_mut()
                    .connect(
                        call_node.output(0),
                        target_port,
                        OptEdge::Value {
                            ty: TypeState::Unset,
                        },
                    )
                    .unwrap();
            });
        }

        //TODO: after trace copy, do the actual iterative replacement
        // of the nodes with the impl-block content, based on the tree structure.
        //
        // special attention is required here, so that
        // 1. subtree count is as expected
        // 2. implementations exist, as requested by the imbl-block that is inlined.

        if env::var("VOLA_DUMP_ALL").is_ok() || env::var("DUMP_BEFOR_SPECIALIZE").is_ok() {
            self.dump_svg("before_specialize.svg");
        }
        //NOTE: the first NodeRef of the pair's is invalid at this point
        for (_, new_lambda) in access_new_lambda_pairs {
            let lambda_region = RegionLocation {
                node: new_lambda,
                region_index: 0,
            };
            self.specialize_region(lambda_region)?;
        }
        Ok(())
    }
}
