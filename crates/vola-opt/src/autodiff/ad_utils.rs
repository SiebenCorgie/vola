/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # AutoDiff Utilities
//!
//! Implements utility passes for the auto-diff implementations

use rvsdg::{
    attrib::{AttribLocation, FlagStore},
    edge::OutportLocation,
    NodeRef, SmallColl,
};
use vola_common::Span;

use crate::{
    alge::Construct,
    autodiff::{AutoDiff, AutoDiffError},
    OptEdge, OptError, OptNode, Optimizer,
};

impl Optimizer {
    ///Transforms a entrypoint with multiple wrt-args into multiple AD-entrypoints with
    /// a single wrt arg.
    ///
    /// Effectively this'll _push down_ the partial-derivative construction down to
    /// _after_ the derivative.
    pub fn linearize_ad(&mut self, entrypoint: NodeRef) -> Result<SmallColl<NodeRef>, OptError> {
        let wrt_src = self.graph[entrypoint].input_src(&self.graph, 1).unwrap();

        //FIXME: We currently rely on the constructor being there _once._.
        //       Whenever we decide to do derivatives of matrices or _other-stuff_
        //       We'd have to handle that in the linearization here.
        if !self.is_node_type::<Construct>(wrt_src.node) {
            return Err(AutoDiffError::LinearizeAdFailed.into());
        }

        //Now build a construction node, where the input is a similar AD node, but with
        //just one wrt-arg.

        let region = self.graph[entrypoint].parent.unwrap().clone();
        let autodiff_src = self.graph[entrypoint].input_src(&self.graph, 0).unwrap();
        let wrt_args = self.graph[wrt_src.node].input_srcs(&self.graph);
        let autodiff_constructor = self
            .graph
            .on_region(&region, |g| {
                let constr = g.insert_node(OptNode::new(
                    Construct::new().with_inputs(wrt_args.len()),
                    Span::empty(),
                ));

                constr
            })
            .unwrap();

        let mut new_ad_entrypoints = SmallColl::new();
        for (wrt_idx, arg) in wrt_args.into_iter().enumerate() {
            if let Some(arg) = arg {
                let new_ad_node = self
                    .graph
                    .on_region(&region, |g| {
                        let new_ad_node =
                            g.insert_node(OptNode::new(AutoDiff::default(), Span::empty()));

                        //Connect the expression
                        let _edg = g
                            .ctx_mut()
                            .connect(
                                autodiff_src,
                                new_ad_node.input(0),
                                OptEdge::value_edge_unset(),
                            )
                            .unwrap();

                        //and this node's wrt-arg
                        let _edg = g
                            .ctx_mut()
                            .connect(arg, new_ad_node.input(1), OptEdge::value_edge_unset())
                            .unwrap();

                        //finally connect the node to the new constructor
                        let _edg = g
                            .ctx_mut()
                            .connect(
                                new_ad_node.output(0),
                                autodiff_constructor.input(wrt_idx),
                                OptEdge::value_edge_unset(),
                            )
                            .unwrap();
                        new_ad_node
                    })
                    .unwrap();

                new_ad_entrypoints.push(new_ad_node);
            } else {
                return Err(AutoDiffError::EmptyWrtArg.into());
            }
        }

        //before returning, replace the current entrypoint with the constructor's
        //output
        self.graph
            .replace_node_uses(entrypoint, autodiff_constructor)?;

        Ok(new_ad_entrypoints)
    }

    ///Tries to canonicalize the AD `entrypoint` into only nodes that are differentiatable.
    pub fn canonicalize_for_ad(&mut self, entrypoint: NodeRef) -> Result<(), OptError> {
        if !self.is_node_type::<AutoDiff>(entrypoint) {
            return Err(OptError::Internal(format!(
                "AD Entrypoint was not of type AutoDiff"
            )));
        }

        Ok(())
    }

    ///Canonicalizes `node` for ad.
    ///
    /// - Guarantees that the replaced node binds to all _formerly_ connected nodes
    /// - Does delete other nodes (but might add / remove connections)
    fn handle_canon_node(&mut self, node: NodeRef) -> Result<(), OptError> {
        println!("Canon {node}");
        Ok(())
    }
}
