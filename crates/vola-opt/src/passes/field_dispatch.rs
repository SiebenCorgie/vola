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

use ahash::AHashMap;
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::{Inport, RegionLocation},
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
        todo!()
        /*
        //Trace copy worst similar to the normal copy, but instead of copying _all_ nodes,
        // we just copy all connected nodes.

        let mut tbcopied_nodes = Vec::new();
        //All edges we know of.
        let mut edges = Vec::new();
        //Frist push src node
        tbcopied.push(srcnode);
        //then iterate all conncted nodes
        for pred in self.graph.walk_predecessors(srcnode) {
            tbcopied.push(pred.node);
        }*/
    }

    ///Dispatches the export_field with the given `ident`ifier.
    pub fn dispatch_export(&mut self, ident: &str) -> Result<(), OptError> {
        //Find all TreeAccess nodes, replace the node with the

        //Right now just copy
        if let Some(exp) = self.export_fn.get(ident) {
            let new_node = self.trace_copy_node(exp.lambda);
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
                .connect(new_node.output(0), export_port, crate::OptEdge::State)
                .unwrap();
        }

        Ok(())
    }
}
