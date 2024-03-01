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

use std::fmt::format;

use ahash::AHashMap;
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::{Inport, RegionLocation},
    smallvec::SmallVec,
    NodeRef,
};
use vola_common::report;

use crate::{error::OptError, OptGraph, Optimizer};

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
                //Now connect
                self.graph.connect(src, dst, ty).unwrap();
            }
        }

        //return new lambda
        dst_region_lambda
    }

    ///Dispatches the export_field with the given `ident`ifier.
    pub fn dispatch_export(&mut self, ident: &str) -> Result<(), OptError> {
        //Find all TreeAccess nodes, replace the node with the

        //Right now just copy
        let copy_node_list = if let Some(exp) = self.export_fn.get(ident) {
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

            copy_nodes
        } else {
            //No such export fn
            return Err(OptError::Any {
                text: format!("No export function named {} found!", ident),
            });
        };

        //TODO: instead reimport λ and replace `node` with a call to that lambda
        //
        //TODO: after trace copy, do the actual iterative replacement of the nodes with the impl-block content, based on the tree structure.
        //
        // special attention is required here, so that
        // 1. subtree count is as expected
        // 2. implementations exist, as requested by the imbl-block that is inlined.
        for node in copy_node_list {
            let new_lambda = self.trace_copy_node(node);
            let omega_node = self.graph.toplevel_region().node;
            //then hook up export
            let export_port = self.graph.on_omega_node(|omg| {
                let new_export = omg.node_mut().add_export();
                InportLocation {
                    node: omega_node,
                    input: InputType::Result(new_export),
                }
            });
            self.graph
                .connect(new_lambda.output(0), export_port, crate::OptEdge::State)
                .unwrap();
        }

        Ok(())
    }
}
