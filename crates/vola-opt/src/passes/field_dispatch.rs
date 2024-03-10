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

        let entry_csg_op = {
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
            tree_access_node.get_op_edge()
        };
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

        //TODO exchange the tree-access node with an eval-node, where the single
        //     _entry-node-tree_ is hooked up, as well as the arguments, than use [specialzie_node]
        //     to start the _standard_ specialization.
        //NOTE: the -1 is, becasue EvalNode::new() adds an input for the csg-argument, which is in the
        //      inputs().len() already.
        let access_argcount = self.graph.node(access_field_node_port.node).inputs().len() - 1;
        println!("Entry eval gets {access_argcount}");
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

        //We exected the seeding eval node to be connected directly to its src tree on input-0.
        let entry_csg_node = self
            .graph
            .node(seeding_eval_node)
            .input_src(&self.graph, 0)
            .unwrap()
            .node;

        if env::var("VOLA_DUMP_ALL").is_ok() || env::var("DUMP_AFTER_ACCESSNODE_REWRITE").is_ok() {
            self.dump_svg(&format!("after_access_node_rewrite_{}.svg", region.node));
        }

        //NOTE we seed the specialization with the entry_csg node, and the TreeAccess node we start at.
        let specialized_entry_node = self.specialize_eval_node(&spec_ctx, seeding_eval_node)?;

        Ok(())
    }

    fn specialize_eval_node(
        &mut self,
        ctx: &SpecializationContext,
        //the eval-like node we are specializing on.
        dispatch_node: NodeRef,
    ) -> Result<NodeRef, OptError> {
        if env::var("VOLA_DUMP_ALL").is_ok() || env::var("DUMP_DISPATCH").is_ok() {
            self.dump_svg(&format!(
                "dispatch_reg_{:?}_{dispatch_node:?}.svg",
                ctx.spec_region.node
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
                    println!("Specializing {}", s.name());
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
            match &self.graph.node(srcport.node).node_type {
                NodeType::Simple(s) => {
                    if s.node.dialect() == "alge" {
                        //If this is an argument to the subtree that is being called, add it to
                        //the list
                        alge_args.push(srcport);
                    }
                }
                NodeType::Lambda(_l) => {
                    if srcport.node == region.node {
                        println!("Detected argport");
                        alge_args.push(srcport);
                    } else {
                        panic!("Unexpected λ-Node");
                    }
                }
                _ => panic!("Unexpected node type!"),
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
            match &self.graph.node(srcport.node).node_type {
                NodeType::Simple(s) => {
                    if s.node.dialect() != "alge" {
                        panic!("Was not of alge dialect!");
                    } else {
                        alge_args.push(srcport);
                    }
                }
                NodeType::Lambda(_l) => {
                    if srcport.node == region.node {
                        println!("Detected argport");
                        alge_args.push(srcport);
                    } else {
                        panic!("Unexpected λ-Node");
                    }
                }
                _ => panic!("Unexpected node type!"),
            }
        }
        println!("... and {} args", alge_args.len());
        //now query the concept of the dispatch node, as well as the
        //entity/operation that is dispatched, since we have this info only now.
        let concept = self.query_concept(dispatch_node)?;
        let entity_or_op_name = self.query_eoo_name(subtree.node)?;
        //now we can build the Implblock key which we need to import.
        let implblock_key = ConceptImplKey {
            node_name: entity_or_op_name,
            concept_name: concept,
        };

        //peek the subtrees of the csg_node that is hooked up to the nodei
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
            let concept = self
                .concept_impl
                .get(&implblock_key)
                .expect(&format!("{implblock_key:?} was not implemented"));
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
            println!("Hook up to {:?}", dst);
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

            if concept_impl.operands.len() != subtrees.len() {
                let err = OptError::DispatchAnyError {
                    opspan: opspan.into(),
                    treeaccessspan: ctx.specialization_src.clone().into(),
                    evalspan: evalspan.into(),
                    concept: block_key.concept_name.clone(),
                    opname: block_key.node_name.clone(),
                    errstring: format!(
                        "concept is implemented, but for {} subtrees, not {}",
                        concept_impl.operands.len(),
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

        //aight, passed semantic checks, so lets copy over all the nodes and edges we found.
        let mut node_remapping: AHashMap<NodeRef, NodeRef> = AHashMap::default();
        let mut outport_remapping: AHashMap<OutportLocation, OutportLocation> = AHashMap::default();
        let mut inport_remapping: AHashMap<InportLocation, InportLocation> = AHashMap::default();

        for subtree in &subtrees {
            println!("Subtree from {subtree:?}");
        }

        //register cv mapping
        for cv in 0..self
            .graph
            .node(concept_region.node)
            .node_type
            .unwrap_lambda_ref()
            .context_variable_count()
        {
            println!("Add remapping {cv}");
            outport_remapping.insert(
                OutportLocation {
                    node: concept_region.node,
                    output: OutputType::ContextVariableArgument(cv),
                },
                subtrees[cv].clone(),
            );
        }

        //register argument mapping
        for argidx in 0..arguments.len() {
            println!("Remapping arg[{argidx}] to {:?}", arguments[argidx]);
            outport_remapping.insert(
                OutportLocation {
                    node: concept_region.node,
                    output: OutputType::Argument(argidx),
                },
                arguments[argidx].clone(),
            );
        }

        //note add the region nodes, as well
        let _ = node_remapping.insert(concept_region.node, ctx.spec_region.node);
        //Argument ports are mapped to the provided _args_
        for node in &all_nodes {
            let cpy = self.graph.deep_copy_node(*node, ctx.spec_region);
            let old = node_remapping.insert(*node, cpy);
            assert!(old.is_none());
        }

        let mut result_src_nodes: SmallVec<[NodeRef; 1]> = SmallVec::default();
        //re-map all edges as well. There is a lil
        //thing, that is, we don't respect result-connected edges. Instead
        //we just don't connect them at all, since that will be handeled by the
        //caller (of this function), when hooking up the result.
        for edge in &all_edges {
            let (src, dst, ty) = {
                let edg = self.graph.edge(*edge);
                (edg.src(), edg.dst(), edg.ty.structural_copy())
            };

            //try remapping the port
            let src = if let Some(alternate_src) = outport_remapping.get(&src) {
                println!("Use remapping from {src:?} to {alternate_src:?}");
                *alternate_src
            } else {
                //if not in remapping, remap the src's node instead and keep the output type

                OutportLocation {
                    node: *node_remapping.get(&src.node).unwrap(),
                    output: src.output,
                }
            };

            //if the dst is a result, register the node in result_src_nodes instead.
            let dst = if dst.input.is_result() {
                //use node remapping tho
                result_src_nodes.push(src.node);
                continue;
            } else {
                //is not a result, therfore remapp the node part
                InportLocation {
                    node: *node_remapping.get(&dst.node).unwrap(),
                    input: dst.input,
                }
            };

            //aight, lets hook them up again
            self.graph.connect(src, dst, ty).unwrap();
        }

        //finally, find all _eval_ nodes and recurse via _inline_impl_block_ and the apropriate
        //subtree.
        for node in &all_nodes {
            //use the remapping
            let mapped_node = node_remapping.get(node).unwrap();
            let is_eval_node = {
                match &self.graph.node(*mapped_node).node_type {
                    NodeType::Simple(s) => s.try_downcast_ref::<EvalNode>().is_some(),
                    _ => false,
                }
            };

            if !is_eval_node {
                continue;
            }

            //aigh, is a eval node, so recurse with the csg node that is connected to the eval node
            let csg_node = self
                .graph
                .node(*mapped_node)
                .input_src(&self.graph, 0)
                .unwrap();
            {
                println!("using csgnode {}", csg_node.node);
                let name = &self
                    .graph
                    .node(csg_node.node)
                    .node_type
                    .unwrap_simple_ref()
                    .try_downcast_ref::<CsgOp>()
                    .unwrap()
                    .op;
                println!("recursing with subtree {name}");
            }

            let output_node = self.specialize_eval_node(ctx, *mapped_node)?;
            //now disconnect the eval node, and hookup the output node instead
            let dst_nodes = self
                .graph
                .node(*mapped_node)
                .output_dsts(&self.graph, 0)
                .unwrap();
            //also carry over the type of the edges
            let ty = self
                .graph
                .edge(self.graph.node(*node).outputs()[0].edges[0])
                .ty
                .structural_copy();
            assert!(self.graph.node(*mapped_node).outputs().len() == 1);

            self.graph.remove_node(*mapped_node).unwrap();
            for dst in dst_nodes {
                self.graph
                    .connect(
                        OutportLocation {
                            node: output_node,
                            output: OutputType::Output(0),
                        },
                        dst,
                        ty.structural_copy(),
                    )
                    .unwrap();
            }
        }

        assert!(
            result_src_nodes.len() == 1,
            "expected only one result node (atm)"
        );
        Ok(result_src_nodes[0])
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
