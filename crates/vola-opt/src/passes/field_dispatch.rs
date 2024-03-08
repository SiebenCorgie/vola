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
    nodes::{self, LangNode, Node, NodeType, StructuralNode},
    region::{Argument, Inport, RegionLocation},
    smallvec::{smallvec, SmallVec},
    util::copy::StructuralClone,
    EdgeRef, NodeRef,
};
use vola_common::{report, Span};

use crate::{
    alge::{implblock::ConceptImplKey, EvalNode},
    csg::{CsgOp, TreeAccess},
    error::OptError,
    OptEdge, Optimizer, TypeState,
};

#[derive(Clone, Debug)]
struct SpecializationContext {
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

    //Small recursive helper that explores all Apply nodes, and inlines their call-defs, before
    //inlining itself.
    fn fully_inline_region(&mut self, region: RegionLocation) -> Result<(), OptError> {
        let all_nodes = self.graph.region(&region).unwrap().nodes.clone();

        for node in all_nodes {
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
                    //now inline ourselfs
                    self.graph.inline_apply_node(node).unwrap();
                } else {
                    //#[cfg(feature = "log")]
                    //log::error!("ApplyNode {node} had no def in {region:?}");
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
        //we specialize the region by recursively calling _csg_-dialect nodes
        //with _specialize-node_ function. However, first we need to read out the field access
        //node, so we know our entry condition / entry-concept. The AccessField node itself will be removed in the
        //process, since we'll hook up the first specialized_node() to the output afterwards.

        //soo, for the thingy to work, we first inline all _calls_ into the region. Which are calls to field_defs atm.
        //we do that by just iterating _all_ nodes, and if its an apply-node,
        // and if it is, call the rvsdg native inliner.

        self.fully_inline_region(region)?;

        //NOTE: This dump is prett _specific_, but good if you need to know if some CSG-Tree inline failes
        //      unexpectedly.
        if env::var("VOLA_DUMP_ALL").is_ok() || env::var("DUMP_AFTER_INLINE_CALL_TREE").is_ok() {
            self.dump_svg(&format!("after_inline_call_tree_{}.svg", region.node));
        }

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
            specialization_src: access_span,
        };

        let entry_node_port = self
            .graph
            .edge(entry_csg_op.expect("expected csgop to be connected!"))
            .src();
        //NOTE we seed the specialization with the entry_csg node, and the TreeAccess node we start at.
        let specialized_entry_node =
            self.specialize_node(&spec_ctx, entry_node_port.node, access_field_node_port.node)?;

        //reconnect output to the resulting node. This will effectively render all CSG-Nodes _dead_.
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

    fn specialize_node(
        &mut self,
        ctx: &SpecializationContext,
        //the csg node that the specialization is based on
        csg_node: NodeRef,
        //the eval-like node we are specializing on.
        dispatch_node: NodeRef,
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
        let argcount = self.graph.node(csg_node).inputs().len();
        for inidx in 0..argcount {
            if let Some(arg_edge) = self.graph.node(csg_node).inputs()[inidx].edge {
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
                    .node(csg_node)
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

        //now query the concept of the dispatch node, as well as the
        //entity/operation that is dispatched, since we have this info only now.
        let concept = self.query_concept(dispatch_node)?;
        let entity_or_op_name = self.query_eoo_name(csg_node)?;
        //now we can build the Implblock key which we need to import.
        let implblock_key = ConceptImplKey {
            node_name: entity_or_op_name,
            concept_name: concept,
        };

        //now we have enough information to pull all the nodes in here
        let new_node = self.inline_impl_block(
            ctx,
            csg_node,
            dispatch_node,
            implblock_key,
            csg_args,
            alge_args,
        )?;

        Ok(new_node)
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

    //Inlines all nodes of a _block_key_ into `region`. Connects all `subrees` to the first
    //arguments of the apropriate eval-nodes, as well as all arguments to the apropriate nodes.
    //Then recurses while passing down the _right_ part of the subtree to [specialize_node].
    //
    // So the whole thing can be thought of as _inline-with-csg-tree-context_.
    fn inline_impl_block(
        &mut self,
        ctx: &SpecializationContext,
        csg_node: NodeRef,
        dispatch_node: NodeRef,
        block_key: ConceptImplKey,
        subtrees: SmallVec<[OutportLocation; 3]>,
        arguments: SmallVec<[OutportLocation; 3]>,
    ) -> Result<NodeRef, OptError> {
        //First inline _block_key_, hookup the evla-nodes appropriatly.
        // Then recurse based on the hooked up sub-tree.
        // Finally, return the _output_ node of the inlined impl_block

        //NOTE at first we check the subtree-count matches. Type-Checking is handled based on the
        //     concept definition. But we don't yet know that an appropriate concept implementation
        //     exists, since we only know the tree yet.

        let opspan = self
            .graph
            .node(csg_node)
            .node_type
            .unwrap_simple_ref()
            .span
            .clone();
        let evalspan = self
            .graph
            .node(dispatch_node)
            .node_type
            .unwrap_simple_ref()
            .span
            .clone();

        let (all_nodes, all_edges, concept_region) = {
            let concept_impl = if let Some(blk) = self.concept_impl.get(&block_key) {
                blk
            } else {
                let err = OptError::DispatchAnyError {
                    opspan: opspan.into(),
                    treeaccessspan: ctx.specialization_src.clone().into(),
                    evalspan: evalspan.into(),
                    concept: block_key.concept_name.clone(),
                    opname: block_key.node_name.clone(),
                    errstring: "concept is not implemented for this operation or entity".to_owned(),
                };
                report(err.clone(), ctx.specialization_src.get_file());
                return Err(err);
            };

            //check that th impl block matches the subtree lenght for expected subtrees.

            if concept_impl.cv_desc.len() != subtrees.len() {
                let err = OptError::DispatchAnyError {
                    opspan: opspan.into(),
                    treeaccessspan: ctx.specialization_src.clone().into(),
                    evalspan: evalspan.into(),
                    concept: block_key.concept_name.clone(),
                    opname: block_key.node_name.clone(),
                    errstring: format!(
                        "concept is implemented, but for {} subtrees, not {}",
                        concept_impl.cv_desc.len(),
                        subtrees.len()
                    ),
                };
                report(err.clone(), ctx.specialization_src.get_file());
                return Err(err);
            }

            //TODO: Decide if we wanna re-test that the arguments also match at this point?
            //      That is already handled already before by another pass (while transforming AST->RVSDG),
            //      but we kinda rely on it happening

            let reg = self.graph.region(&concept_impl.lambda_region).unwrap();
            (
                reg.nodes.clone(),
                reg.edges.clone(),
                concept_impl.lambda_region,
            )
        };

        //Allright, passed semantic checks, so lets copy over all the nodes and edges we found.
        //also, while at it, read out if they where connected to an argument or cv (on the input side), or an
        //result to the output side. In that case, record the remapping.
        let mut node_remapping: AHashMap<NodeRef, NodeRef> = AHashMap::default();
        let mut inport_remapping: AHashMap<InportLocation, InportLocation> = AHashMap::default();
        let mut outport_remapping: AHashMap<OutportLocation, OutportLocation> = AHashMap::default();

        for node in &all_nodes {}

        //alright, after copying, re-map all edges as well.

        //finally, find all _eval_ nodes and recurse via _inline_impl_block_ and the apropriate
        //subtree.

        todo!("Failed")
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

        //For some house keeping, remove unused imports on the export after trace
        //copying
        self.graph
            .remove_unused_context_variables(export_fn_region.node);

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
