/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # Forward-Mode auto differentiation
//!
//! Implements the forward autodiff pass based on a diff-entry point node.
//!
//! ## Basic idea
//!
//! The idea of the forward pass is relatively straight forward. Similar to the specialization pass we walk
//! _through_ our expression graph. For any _dynamic_ node, so any node that uses _the_ wrt-argument, we split off
//! the walk through as f'(g(x)) = f'(g(x)) * g'(x), where f'(g(x)) is one split, and g'(x) is the other split.
//!
//! In order to be able to do that we assume that the expression is already in _ad-canonicalized-form_. So is only made from
//! essential operations.
//!
//! In forward mode, control flow stays _in the same order_ so branches/gamma nodes work the same way (chose
//! one of the derivatives in a branch), and loops also work the same way, just for the derivative.
//!
//! In practice we copy over control-flow nodes into the derivative calculation part and replace a result r with the
//! derivative of r.
//!
//! ## Implementation details
//!
//! ### Highlevel
//!
//! We start from just a single `AutoDiff` node.
//! The first part is canonicalizing the subgraph referenced by the first argument into ad-able form.
//! That means mostly transforming/unfolding complex expressions into _elementary_ operations, as well as checking
//! That no _none-ad-able_ nodes are in the graph.
//!
//! NOTE: Right now we also inline all function calls, because that makes life easier atm. However, it would be nicer
//! to specialize a _derivative-of-that-λ_, that could be reused if needed.
//!
//! After that we create the derivative of all wrt arguments (see below _derivative creation_).
//!
//! The last part is replacing the `AutoDiff` node with either the single derivative (in case of scalar output),
//! or a vector-constructor (in case of vector-output).
//!
//! ## Derivative creation
//!
//! Derivative creation is a two step process.
//! The first pass explores the computational tree, and tags nodes that are depending on the wrt-argument, _dynamically_.
//! That lets the actual forward pass distinguish the case f'(g(x)) = f'(g(x)) * g'(x) from the case f'(c) = 0
//!
//!
//! Finally we are ready to do the forward accumulation. The nice
//! part being, that for any constant we can reuse parts of the original
//! graph.

use rvsdg::{NodeRef, SmallColl};

use crate::{alge::Construct, autodiff::AutoDiff, common::Ty, OptError, Optimizer};

impl Optimizer {
    ///Executes forward-ad pass on `entrypoint`. Assumes that it is a `AutoDiff` node. If that is the case, the node will be replaced
    /// with the differentiated value(s) after this pass (successfuly) ends.
    pub fn forward_ad(&mut self, entrypoint: NodeRef) -> Result<(), OptError> {
        if !self.is_node_type::<AutoDiff>(entrypoint) {
            return Err(OptError::Internal(format!(
                "AD Entrypoint was not of type AutoDiff"
            )));
        }

        //Canonicalize the subtree for our AD pass
        self.canonicalize_for_ad(entrypoint)?;

        //now dispatch the sub-ad part for all wrt arguments
        let wrt_src = self
            .graph
            .inport_src(entrypoint.as_inport_location(AutoDiff::wrt_input()))
            .unwrap();

        //If the src-node of the wrt-argument is a constructor,
        // collect the constructor's inputs instead
        let wrt_args = if self.is_node_type::<Construct>(wrt_src.node) {
            let mut wrt_args = SmallColl::new();
            for input in self.graph.node(wrt_src.node).inport_types() {
                wrt_args.push(
                    self.graph
                        .inport_src(wrt_src.node.as_inport_location(input))
                        .unwrap(),
                );
            }

            //For sanity, make sure the wrt-arg is the expected
            //vector type with the amount of wrt-constructors
            assert!({
                let wrt_arg_edge = self
                    .graph
                    .node(entrypoint)
                    .inport(&AutoDiff::wrt_input())
                    .unwrap()
                    .edge
                    .unwrap();

                self.graph.edge(wrt_arg_edge).ty.get_type()
                    == Some(&Ty::Vector {
                        width: wrt_args.len(),
                    })
            });

            wrt_args
        } else {
            //just use the node as wrt-arg
            let mut wrt_args = SmallColl::new();
            wrt_args.push(wrt_src);
            wrt_args
        };

        for wrt in &wrt_args {
            println!("Wrt-Arg: {wrt:?}");
        }

        todo!()
    }
}
