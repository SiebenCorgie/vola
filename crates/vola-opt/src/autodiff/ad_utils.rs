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
    attrib::AttribLocation, edge::OutportLocation, region::RegionLocation, smallvec::smallvec,
    util::abstract_node_type::AbstractNodeType, NodeRef, SmallColl,
};
use rvsdg_viewer::View;
use vola_common::Span;

use crate::{
    alge::{arithmetic::UnaryArith, buildin::Buildin, ConstantIndex, Construct},
    autodiff::{AutoDiff, AutoDiffError},
    common::Ty,
    imm::ImmScalar,
    OptEdge, OptError, OptNode, Optimizer,
};

impl Optimizer {
    ///Builds a _zero-value-node_ for the given node's output type. This means checking the output type, and
    /// building that primitive set with just zero as value.
    ///
    /// Fails if the output of this node has no type.
    pub fn emit_zero_for_node(&mut self, region: RegionLocation, node: NodeRef) -> OutportLocation {
        assert!(self.graph[node].outputs().len() == 1);
        assert!(self.graph[node].into_abstract() == AbstractNodeType::Simple);
        self.emit_zero_for_port(region, node.output(0))
    }

    ///Builds a _zero-value-node_ for the given ports output type. This means checking the output type, and
    /// building that primitive set with just zero as value.
    ///
    /// Fails if the output of this node has no type.
    pub fn emit_zero_for_port(
        &mut self,
        region: RegionLocation,
        port: OutportLocation,
    ) -> OutportLocation {
        let ty = self
            .find_type(&port.into())
            .expect("Expected port's output to be set");
        self.splat_scalar(region, ImmScalar::new(0.0), ty)
    }

    ///Transforms a entrypoint with multiple wrt-args into multiple AD-entrypoints with
    /// a single wrt arg.
    ///
    /// Effectively this'll _push down_ the partial-derivative construction down to
    /// _after_ the derivative.
    pub fn linearize_ad(&mut self, entrypoint: NodeRef) -> Result<SmallColl<NodeRef>, OptError> {
        let wrt_src = self.graph[entrypoint].input_src(&self.graph, 1).unwrap();

        let wrt_ty = self.find_type(&wrt_src.into()).unwrap();
        let region = self.graph[entrypoint].parent.unwrap().clone();
        let autodiff_src = self.graph[entrypoint].input_src(&self.graph, 0).unwrap();
        let span = self.find_span(entrypoint.into()).unwrap_or(Span::empty());

        match wrt_ty {
            //is already linear
            Ty::Scalar => Ok(smallvec![entrypoint]),
            Ty::Vector { width } => {
                let autodiff_constructor = self
                    .graph
                    .on_region(&region, |g| {
                        let constr = g.insert_node(OptNode::new(
                            Construct::new().with_inputs(width),
                            Span::empty(),
                        ));

                        constr
                    })
                    .unwrap();

                let mut new_ad_entrypoints = SmallColl::new();
                for wrt_idx in 0..width {
                    //Now build a construction node, where the input is a similar AD node, but with
                    //just one wrt-arg.
                    let new_ad_node = self
                        .graph
                        .on_region(&region, |g| {
                            let (wrt_index, _) = g
                                .connect_node(
                                    OptNode::new(ConstantIndex::new(wrt_idx), span.clone()),
                                    &[wrt_src],
                                )
                                .unwrap();

                            let (new_ad_node, _) = g
                                .connect_node(
                                    OptNode::new(AutoDiff::default(), Span::empty()),
                                    &[autodiff_src, wrt_index.output(0)],
                                )
                                .unwrap();

                            //connect to the construct node as well
                            g.ctx_mut()
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
                }

                //before returning, replace the current entrypoint with the constructor's
                //output
                self.graph
                    .replace_node_uses(entrypoint, autodiff_constructor)?;

                Ok(new_ad_entrypoints)
            }
            Ty::Matrix {
                width: _,
                height: _,
            } => {
                todo!("Implement matrix-diff linearization!")
            }
            _ => return Err(AutoDiffError::LinearizeAdFailed.into()),
        }
    }

    ///Tries to canonicalize the AD `entrypoint` into only nodes that are differentiatable.
    pub fn canonicalize_for_ad(&mut self, entrypoint: NodeRef) -> Result<(), OptError> {
        if !self.is_node_type::<AutoDiff>(entrypoint) {
            return Err(OptError::Internal(format!(
                "AD Entrypoint was not of type AutoDiff"
            )));
        }

        let active = self.activity_explorer(entrypoint)?;

        for v in active.active.flags.keys() {
            if let AttribLocation::Node(n) = v {
                self.handle_canon_node(*n)?;
            }
        }

        Ok(())
    }

    ///Canonicalizes `node` for ad.
    ///
    /// - Guarantees that the replaced node binds to all _formerly_ connected nodes
    /// - Does not delete other nodes (but might add / remove connections)
    pub(crate) fn handle_canon_node(&mut self, node: NodeRef) -> Result<(), OptError> {
        let target_region = self.graph[node].parent.unwrap().clone();

        if self.is_node_type::<Buildin>(node) {
            return self.handle_canon_buildin(&target_region, node);
        }

        if self.is_node_type::<UnaryArith>(node) {
            return self.handle_canon_unary(&target_region, node);
        }

        Ok(())
    }

    ///Handles type derive and propagation of a node that is added while canonicalizing.
    pub(crate) fn type_derive_and_propagate(&mut self, node: NodeRef) -> Result<(), OptError> {
        match self.try_node_type_derive(node)? {
            (Some(ty), outport) => {
                self.typemap.set(outport.into(), ty.clone());
                //push type on port into edges
                for edg in self.graph[outport].edges.clone() {
                    self.graph[edg].ty.set_type(ty.clone());
                }

                Ok(())
            }
            (None, _) => Err(OptError::Any {
                text: format!(
                    "Could not derive the type for canonicalized node \"{}\"",
                    self.graph[node].name()
                ),
            }),
        }
    }
}
