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
    edge::{OutportLocation, OutputType},
    region::RegionLocation,
    smallvec::smallvec,
    NodeRef, SmallColl,
};
use vola_common::Span;

use crate::{
    autodiff::{ad_forward::ForwardAd, AutoDiff, AutoDiffError},
    common::{DataType, Shape, Ty},
    imm::ImmScalar,
    typelevel::{ConstantIndex, UniformConstruct},
    OptEdge, OptError, OptNode, Optimizer,
};

impl<'opt> ForwardAd<'opt> {
    ///Transforms a entrypoint with multiple wrt-args into multiple AD-entrypoints with
    /// a single wrt arg.
    ///
    /// Effectively this'll _push down_ the partial-derivative construction down to
    /// _after_ the derivative.
    pub(crate) fn linearize_ad(
        &mut self,
        entrypoint: NodeRef,
    ) -> Result<SmallColl<NodeRef>, OptError> {
        let wrt_src = self.opt.graph[entrypoint]
            .input_src(&self.opt.graph, 1)
            .unwrap();

        let wrt_ty = self.opt.get_out_type_mut(wrt_src).unwrap();
        let region = self.opt.graph[entrypoint].parent.unwrap();
        let autodiff_src = self.opt.graph[entrypoint]
            .input_src(&self.opt.graph, 0)
            .unwrap();
        let span = self.opt.find_span(entrypoint).unwrap_or(Span::empty());

        match wrt_ty {
            //is already linear
            Ty::SCALAR_REAL => Ok(smallvec![entrypoint]),
            Ty::Shaped {
                shape: Shape::Vec { width },
                ty: DataType::Real,
            } => {
                let autodiff_constructor = self
                    .opt
                    .graph
                    .on_region(&region, |g| {
                        g.insert_node(OptNode::new(
                            UniformConstruct::new().with_inputs(width),
                            Span::empty(),
                        ))
                    })
                    .unwrap();

                let mut new_ad_entrypoints = SmallColl::new();
                for wrt_idx in 0..width {
                    //Now build a construction node, where the input is a similar AD node, but with
                    //just one wrt-arg.
                    let new_ad_node = self
                        .opt
                        .graph
                        .on_region(&region, |g| {
                            let (wrt_index, _) = g
                                .connect_node(
                                    OptNode::new(ConstantIndex::new(wrt_idx), span.clone()),
                                    [wrt_src],
                                )
                                .unwrap();

                            let (new_ad_node, _) = g
                                .connect_node(
                                    OptNode::new(AutoDiff::default(), Span::empty()),
                                    [autodiff_src, wrt_index.output(0)],
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
                self.opt
                    .graph
                    .replace_node_uses(entrypoint, autodiff_constructor)?;

                Ok(new_ad_entrypoints)
            }
            Ty::Shaped {
                shape:
                    Shape::Matrix {
                        width: _,
                        height: _,
                    },
                ty: DataType::Real,
            } => {
                todo!("Implement matrix-diff linearization!")
            }
            _ => Err(AutoDiffError::LinearizeAdFailed.into()),
        }
    }
}

impl Optimizer {
    ///Builds a _zero-value-node_ for the given ports output type. This means checking the output type, and
    /// building that primitive set with just zero as value.
    ///
    /// Fails if the output of this node has no type.
    pub fn emit_zero_for_port(
        &mut self,
        region: RegionLocation,
        port: OutportLocation,
    ) -> OutportLocation {
        let ty = self.get_out_type_mut(port).unwrap();
        self.splat_scalar(region, ImmScalar::new(0.0), ty)
    }

    ///Checks that all, or no edge is conneted in any branch of `gamma_node` for `exit_variable`
    ///Returns if any is connected, or error, if only _some_ are connected.
    pub(crate) fn all_connected(
        &self,
        gamma_node: NodeRef,
        exit_variable: OutputType,
    ) -> Result<bool, ()> {
        assert!(matches!(exit_variable, OutputType::ExitVariableOutput(_)));
        assert!(self.graph[gamma_node].node_type.is_gamma());

        let branch_count = self.graph[gamma_node].regions().len();
        let result = (0..branch_count).fold(None, |set_state, region| {
            let result_port = exit_variable
                .map_to_in_region(region)
                .unwrap()
                .to_location(gamma_node);
            match set_state {
                None => {
                    //Non set yet, just read and set as okay
                    let is_connected = self.graph[result_port].edge.is_some();
                    Some(Ok(is_connected))
                }
                Some(Ok(should_state)) => {
                    let is_connected = self.graph[result_port].edge.is_some();
                    if should_state != is_connected {
                        Some(Err(()))
                    } else {
                        //State equals
                        Some(Ok(should_state))
                    }
                }
                Some(Err(e)) => Some(Err(e)),
            }
        });

        match result {
            //some collision happened
            Some(Err(_e)) => Err(()),
            //All connected or unconnetced
            Some(Ok(all_connected)) => Ok(all_connected),
            //no branch encountered (unlikely)
            None => Ok(false),
        }
    }
}
