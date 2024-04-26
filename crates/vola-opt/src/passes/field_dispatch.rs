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
    nodes::{NodeType, StructuralNode},
    region::{Inport, RegionLocation},
    smallvec::{smallvec, SmallVec},
    util::copy::StructuralClone,
    NodeRef, SmallColl,
};
use rvsdg_viewer::View;
use vola_common::{report, Span};

use crate::{
    alge::{implblock::ConceptImplKey, EvalNode},
    common::Ty,
    csg::{CsgOp, TreeAccess},
    error::OptError,
    OptEdge, OptNode, Optimizer, TypeState,
};

#[derive(Clone, Debug)]
struct SpecializationContext {
    //The span of the TreeAccess node based uppon we specialize.
    specialization_src: Span,
    //the region we are specializing in
    spec_region: RegionLocation,
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

    ///Dispatches the export_field with the given `identifier.
    pub fn dispatch_export(&mut self, ident: &str) -> Result<(), OptError> {
        //Find all TreeAccess nodes, and prepare regions for specialization.
        //This currently means creating a λ-Copy of the export region for each access,
        //that then gets called in place of the access node.
        let (copy_node_list, export_fn_region, arg_tys) =
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

                let lmd = self.graph.node(exp.lambda).node_type.unwrap_lambda_ref();
                let argcount = lmd.argument_count();
                let argtys = (0..argcount)
                    .map(|idx| {
                        self.find_type(
                            &OutportLocation {
                                node: exp.lambda,
                                output: OutputType::Argument(idx),
                            }
                            .into(),
                        )
                        .expect("failed to get argument type")
                    })
                    .collect::<SmallColl<_>>();

                (copy_nodes, exp.lambda_region.clone(), argtys)
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

            //derive the result type from the accessed concept
            let result_type = self
                .find_type(
                    &InportLocation {
                        node: *new_lambda,
                        input: InputType::Result(0),
                    }
                    .into(),
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
            //NOTE: clone since we are in a loop :eyes:
            let arg_tys = arg_tys.clone();
            let input_args = (0..arg_tys.len())
                .map(|idx| OutportLocation {
                    node: export_fn_region.node,
                    output: OutputType::Argument(idx),
                })
                .collect::<SmallVec<[OutportLocation; 3]>>();

            self.graph
                .on_region(&export_fn_region, |reg| {
                    let (call_node, input_edges) = reg
                        .call(import_cv_port_out, &input_args)
                        .expect("Failed to hook up replacement Apply node");
                    //the +1 is the call edge that gets added by `call`
                    assert!(input_edges.len() == arg_tys.len() + 1);
                    //NOTE: skip, since the first edge is the call edge
                    for (edg, ty) in input_edges.into_iter().skip(1).zip(arg_tys.into_iter()) {
                        reg.ctx_mut().edge_mut(edg).ty.set_type(ty);
                    }

                    reg.ctx_mut()
                        .connect(
                            call_node.output(0),
                            target_port,
                            OptEdge::Value {
                                ty: TypeState::Set(result_type.clone()),
                            },
                        )
                        .unwrap();
                })
                .unwrap();
        }

        //For some house keeping, remove unused imports on the export after trace
        //copying
        self.graph
            .remove_unused_context_variables(export_fn_region.node);

        if env::var("VOLA_DUMP_ALL").is_ok() || env::var("DUMP_BEFORE_SPECIALIZE").is_ok() {
            //self.dump_svg("before_specialize.svg", true);
            self.push_debug_state("before specialize");
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

    //Inline-linke helper, that inlines only `srcnode`-connected nodes into a new λ-Node
    // in the optimizer's omega-region / toplevel-region.
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

    //Small recursive helper that explores all Apply nodes, and inlines their call-defs, before
    //inlining itself.
    fn fully_inline_region(&mut self, region: RegionLocation) -> Result<(), OptError> {
        //NOTE: We only inline connected apply nodes, since we'd outherwise might touch _undefined_
        //      parts of the region.

        let applynodes = self
            .graph
            .live_nodes(region)
            .iter()
            .filter(|n| self.graph.node(**n).node_type.is_apply())
            .cloned()
            .collect::<Vec<_>>();

        //        let mut ninline = 0;
        for node in applynodes {
            if self.graph.node(node).node_type.is_apply() {
                let apply_node_call_port = InportLocation {
                    node,
                    input: InputType::Input(0),
                };
                if let Some(prod) = self.graph.find_producer_inp(apply_node_call_port) {
                    assert!(self.graph.node(prod.node).node_type.is_lambda());
                    //recursively inline this
                    self.fully_inline_region(RegionLocation {
                        node: prod.node,
                        region_index: 0,
                    })?;
                    //ninline += 1;
                    //now inline ourselfs
                    self.graph.inline_apply_node(node).unwrap();
                } else {
                    #[cfg(feature = "log")]
                    log::error!("ApplyNode {node} had no def in {region:?}");
                    //NOTE: this can happen, if the all_nodes of the region changed. So we ignore that atm.
                }
            }
        }

        //for good measures, remove all unused CVs after importing _everything_
        self.graph.remove_unused_context_variables(region.node);
        Ok(())
    }

    ///Runs the actual specialization of an region.
    fn specialize_region(&mut self, region: RegionLocation) -> Result<(), OptError> {
        //soo, for the thingy to work, we first inline all _calls_ into the region. Which are calls to field_defs atm.
        //we do that by just iterating _all_ nodes, and if its an apply-node,
        // and if it is, call the rvsdg native inliner.

        self.fully_inline_region(region)?;

        //NOTE: This dump is _specific_, but good if you need to know if some CSG-Tree inline failes
        //      unexpectedly.
        if env::var("VOLA_DUMP_ALL").is_ok() || env::var("DUMP_AFTER_INLINE_CALL_TREE").is_ok() {
            //self.dump_svg(&format!("after_inline_call_tree_{}.svg", region.node), true);
            self.push_debug_state(&format!("post inline call-tree on {}", region.node));
        }

        let access_field_node_port = self
            .graph
            .region(&region)
            .unwrap()
            .result_src(&self.graph, 0)
            .unwrap();

        let access_span = self
            .graph
            .node(access_field_node_port.node)
            .node_type
            .unwrap_simple_ref()
            .span
            .clone();

        let spec_ctx = SpecializationContext {
            specialization_src: access_span.clone(),
            spec_region: region,
        };

        //NOTE: exchange the tree-access node with an eval-node, where the single
        //      _entry-node-tree_ is hooked up, as well as the arguments, than use [specialzie_node]
        //      to start the _standard_ specialization.
        //NOTE: the -1 is, because EvalNode::new() adds an input for the csg-argument, which is in the
        //      inputs().len() already.
        let access_argcount = self.graph.node(access_field_node_port.node).inputs().len() - 1;
        let accessed_concept = self.query_concept(access_field_node_port.node)?;
        //create the dummy node we use and connect that in place of the tree_access_node
        let eval_node = OptNode::new(
            EvalNode::new(access_argcount, accessed_concept),
            access_span,
        );

        let seeding_eval_node = self
            .graph
            .replace_node(access_field_node_port.node, eval_node)
            .unwrap();

        if env::var("VOLA_DUMP_ALL").is_ok() || env::var("DUMP_AFTER_ACCESSNODE_REWRITE").is_ok() {
            /*self.dump_svg(
                &format!("after_access_node_rewrite_{}.svg", region.node),
                true,
            );*/
            self.push_debug_state(&format!("after access-node rewrite on {}", region.node));
        }

        //NOTE we seed the specialization with the entry_csg node, and the TreeAccess node we start at.
        let _specialized_entry_node = self.specialize_eval_node(&spec_ctx, seeding_eval_node)?;
        //After specializing, use recursive inliner again, to bring all ops into the same region
        self.fully_inline_region(region).unwrap();

        Ok(())
    }

    fn specialize_eval_node(
        &mut self,
        ctx: &SpecializationContext,
        //the eval-like node we are specializing on.
        dispatch_node: NodeRef,
    ) -> Result<NodeRef, OptError> {
        if env::var("VOLA_DUMP_ALL").is_ok() || env::var("DUMP_DISPATCH").is_ok() {
            /*self.dump_svg(
                &format!(
                    "dispatch_reg_{:?}_{dispatch_node:?}.svg",
                    ctx.spec_region.node
                ),
                true,
            );*/
            self.push_debug_state(&format!(
                "dispatch region {}[{}] on {}",
                ctx.spec_region.node, ctx.spec_region.region_index, dispatch_node
            ));
        }

        let region = self
            .graph
            .node(dispatch_node)
            .parent
            .as_ref()
            .cloned()
            .unwrap();
        //check that the first argument is in fact a subtree. If not, this is an invalid tree-op pair
        //since we _expect_ to have a subtree, but there are no more csg-nodes
        let subtree = {
            let srcport = self
                .graph
                .node(dispatch_node)
                .input_src(&self.graph, 0)
                .unwrap();
            //to be sure, trace the src port compleatly to its producer
            let srcport = self.graph.find_producer_out(srcport).unwrap();

            if let NodeType::Simple(s) = &self.graph.node(srcport.node).node_type {
                //now make sure this is in fact a csg node
                if s.node.dialect() != "csg" {
                    panic!("Expected CSG sub node!");
                } else {
                    srcport
                }
            } else {
                panic!("Was not a simple node");
            }
        };

        //NOTE index 0 was already checked
        //NOTE (as well). By definition the first n-arguments are the _fields_ of the
        //     entity or argument that is implemented. So the alge-args to the csg-node.
        //     After that the alge-args to the concept that is implemented follow, which are the alge args to the
        //     eval-node we are handling

        let mut alge_args: SmallVec<[OutportLocation; 3]> = SmallVec::default();
        let csg_argcount = self.graph.node(subtree.node).inputs().len();
        for csgarg in 0..csg_argcount {
            let srcport = self
                .graph
                .node(subtree.node)
                .input_src(&self.graph, csgarg)
                .unwrap();
            let producer_port = self.graph.find_producer_out(srcport).unwrap();
            match &self.graph.node(producer_port.node).node_type {
                NodeType::Simple(s) => {
                    if s.node.dialect() == "alge" {
                        //If this is an argument to the subtree that is being called, add it to
                        //the list
                        alge_args.push(srcport);
                    }
                }
                //This is the _hard_case_. What we do is check, if we can _import_ the port into our region,
                //if needed, and hook that up instead.
                _ => {
                    let imported = if self.graph.node(srcport.node).parent.unwrap() != region {
                        self.graph.import_context(srcport, region).unwrap()
                    } else {
                        srcport
                    };
                    //now push it regardless
                    alge_args.push(imported);
                }
            }
        }
        //now append the alge-args of the eval-node
        let argcount = self.graph.node(dispatch_node).inputs().len();
        for inidx in 1..argcount {
            let srcport = self
                .graph
                .node(dispatch_node)
                .input_src(&self.graph, inidx)
                .unwrap();
            let producer_port = self.graph.find_producer_out(srcport).unwrap();
            match &self.graph.node(producer_port.node).node_type {
                NodeType::Simple(s) => {
                    if s.node.dialect() != "alge" {
                        panic!("Was not of alge dialect!");
                    } else {
                        alge_args.push(srcport);
                    }
                }
                //again the _hard case_. The rational is _if we don't find a simple_node as produces, this must be an _
                // _outside argument, so it must be an alge-argument_. This holds true, since CSGs would all be sowhere
                // hooked up as a CV-Arg to some parent region.
                _ => {
                    let imported = if self.graph.node(srcport.node).parent.unwrap() != region {
                        self.graph.import_context(srcport, region).unwrap()
                    } else {
                        srcport
                    };
                    alge_args.push(imported);
                }
            }
        }
        //now query the concept of the dispatch node, as well as the
        //entity/operation that is dispatched, since we have this info only now.
        let concept = self.query_concept(dispatch_node)?;
        let entity_or_op_name = self.query_eoo_name(subtree.node)?;
        //now we can build the Implblock key which we need to import.
        let implblock_key = ConceptImplKey {
            node_name: entity_or_op_name,
            concept_name: concept,
        };

        //peek the subtrees of the csg_node that is hooked up to the node
        //NOTE: the csg_subtree is always in the same region as its predecessor, since we inlined all of them.
        let mut csg_subtrees: SmallVec<[OutportLocation; 3]> = SmallVec::new();
        for inidx in 0..self.graph.node(subtree.node).inputs().len() {
            if let Some(src) = self.graph.node(subtree.node).input_src(&self.graph, inidx) {
                if let NodeType::Simple(s) = &self.graph.node(src.node).node_type {
                    if s.node.dialect() == "csg" {
                        csg_subtrees.push(src);
                    }
                }
            }
        }

        //Now, we _inline_ by deep-copying the λ-region of the concept into our region, then hooking up the sub-trees and
        //arguments as described, and finally replacing the eval_node with a apply node that calls the specialized local λ-node
        //with the apropriate arguments.
        let local_lmd_node = {
            //TODO error handling
            let concept = if let Some(c) = self.concept_impl.get(&implblock_key) {
                c
            } else {
                let eval_span = self
                    .graph
                    .node(dispatch_node)
                    .node_type
                    .unwrap_simple_ref()
                    .span
                    .clone();
                let opspan = self
                    .graph
                    .node(subtree.node)
                    .node_type
                    .unwrap_simple_ref()
                    .span
                    .clone();
                let opname = self
                    .graph
                    .node(subtree.node)
                    .node_type
                    .unwrap_simple_ref()
                    .name();
                let err = OptError::DispatchAnyError {
                    opspan: opspan.into(),
                    treeaccessspan: ctx.specialization_src.clone().into(),
                    evalspan: eval_span.into(),
                    concept: implblock_key.concept_name.clone(),
                    opname,
                    errstring: format!(
                        "Concept {} was not implemented for operation or entity {}",
                        implblock_key.concept_name, implblock_key.node_name
                    ),
                };
                report(err.clone(), ctx.specialization_src.get_file());
                return Err(err);
            };
            self.graph.deep_copy_node(concept.lambda, region)
        };

        //hook up the subtrees
        for (subidx, subtree_output) in csg_subtrees.iter().enumerate() {
            //If needed, import the subtree_output to our region.
            let subtree_output = if self.graph.node(subtree_output.node).parent.unwrap() != region {
                self.graph.import_context(*subtree_output, region).unwrap()
            } else {
                *subtree_output
            };

            self.graph
                .connect(
                    subtree_output,
                    InportLocation {
                        node: local_lmd_node,
                        input: InputType::ContextVariableInput(subidx),
                    },
                    OptEdge::Value {
                        ty: TypeState::Set(Ty::CSGTree),
                    },
                )
                .unwrap();
        }

        //now hookup the arguments, as needed to the apply node, that calls this
        //before we can actually do that, we have to make sure, that the argument is actually in our context thought.
        for argport in &mut alge_args {
            *argport = if self.graph.node(argport.node).parent.unwrap() != region {
                self.graph.import_context(*argport, region).unwrap()
            } else {
                *argport
            };
        }
        let lmd_call = self
            .graph
            .on_region(&region, |reg| {
                //This is one of those rare cases, where we construct the apply node, since we currently do not known the actuall correct
                //pattern
                let (callnode, _) = reg
                    .call(
                        local_lmd_node.as_outport_location(OutputType::LambdaDeclaration),
                        &alge_args,
                    )
                    .unwrap();
                callnode
            })
            .unwrap();

        //reconnect output to the resulting node. This will effectively render all CSG-Nodes _dead_.
        let result_edges = self.graph.node(dispatch_node).output_edges()[0].clone();
        for edg in result_edges {
            let dst = self.graph.edge(edg).dst().clone();
            let result_ty = self.graph.disconnect(edg).unwrap();
            //now connect the specialized_entry_node to that output
            self.graph
                .connect(
                    OutportLocation {
                        node: lmd_call,
                        output: OutputType::Output(0),
                    },
                    dst,
                    result_ty,
                )
                .unwrap();
        }
        self.graph.remove_node(dispatch_node).unwrap();
        //for sanity, check that
        #[cfg(debug_assertions)]
        self.graph.is_legal_structural().unwrap();
        assert!(!self.graph.region_has_cycles(region));

        //finally, recurse into the just created region.
        let allnodes = self
            .graph
            .region(&RegionLocation {
                node: local_lmd_node,
                region_index: 0,
            })
            .unwrap()
            .nodes
            .clone();
        for node in allnodes {
            let is_eval_node = match &self.graph.node(node).node_type {
                NodeType::Simple(s) => s.try_downcast_ref::<EvalNode>().is_some(),
                _ => false,
            };
            if is_eval_node {
                self.specialize_eval_node(ctx, node)?;
            }
        }

        self.graph.remove_unused_context_variables(local_lmd_node);

        Ok(lmd_call)
    }

    fn query_concept(&self, node: NodeRef) -> Result<String, OptError> {
        match &self.graph.node(node).node_type {
            NodeType::Simple(s) => {
                //first try downcasting to Eval, if that doesn't work, try downcasting to TreeAccess,
                //if that doesn't work either, something is buggy
                if let Some(eval) = s.try_downcast_ref::<EvalNode>() {
                    return Ok(eval.concept().clone());
                }

                if let Some(taccess) = s.try_downcast_ref::<TreeAccess>() {
                    return Ok(taccess.called_concept.clone());
                }

                //if we reached this, something is bugg.
                let err = OptError::AnySpanned {
                    span: s.span.clone().into(),
                    text: "Expected either a TreeAccess expression, or a EvalNode expression!"
                        .to_owned(),
                    span_text: "Failed to get this nodes called concept".to_owned(),
                };
                report(err.clone(), s.span.get_file());
                Err(err)
            }
            _ => Err(OptError::Any {
                text: format!("Cound not query concept, was no simple node!"),
            }),
        }
    }

    ///Returns the EntityOrOp (eoo) name the `node` calls. Assumes that `node` is a CsgOp. Otherwise
    /// returns an error.
    fn query_eoo_name(&self, node: NodeRef) -> Result<String, OptError> {
        match &self.graph.node(node).node_type {
            NodeType::Simple(s) => {
                if let Some(csgop) = s.try_downcast_ref::<CsgOp>() {
                    Ok(csgop.op.clone())
                } else {
                    let err = OptError::AnySpanned {
                        span: s.span.clone().into(),
                        text: "Expected operation or entity!".to_owned(),
                        span_text: "Failed to get this nodes entity or operation name".to_owned(),
                    };
                    report(err.clone(), s.span.get_file());
                    Err(err)
                }
            }
            _ => Err(OptError::Any {
                text: format!("Colud not query eoo-name, expected simple node!"),
            }),
        }
    }
}