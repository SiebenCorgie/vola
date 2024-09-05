/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Implements loop-unrolling
//!
//! See
//!
//! - [unroll_theta](crate::Rvsdg::unroll_theta)
//! - [unroll_replace_theta](crate::Rvsdg::unroll_replace_theta)

use thiserror::Error;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::LangNode,
    region::RegionLocation,
    NodeRef, Rvsdg,
};

use super::copy::StructuralClone;

#[derive(Error, Debug, Clone)]
pub enum UnrollError {
    #[error("Node to unroll was not a theta node")]
    NotThetaNode,
    #[error("Loop variable input {0} is not connected outside of the loop, but connected inside. Therefore the loop cannot be unrolled")]
    LvInOutConnectionError(usize),
    #[error("Internal graph error while unrolling node")]
    InternalError(#[from] GraphError),
    #[error("Could not find node mapping for unrolled node")]
    RemapError,
}

impl<N: LangNode + StructuralClone + 'static, E: LangEdge + StructuralClone + 'static> Rvsdg<N, E> {
    ///A shortcut for the case where `unroll_count` is the same as the iteration count of `theta`.
    ///
    ///Takes care of eleminating the `theta` node after unrolling, and hooks up the produced values to the
    /// loop-variable users.
    pub fn unroll_replace_theta(
        &mut self,
        theta: NodeRef,
        unroll_count: usize,
    ) -> Result<(), UnrollError> {
        self.unroll_theta_head(theta, unroll_count)?;
        //now replace all lv-output uses with the lv-input connected producers

        for lv in self[theta].inport_types() {
            let lvidx = if let InputType::Input(i) = lv {
                i
            } else {
                panic!("Invalid Theta node with non-input input: {lv:?}");
            };

            let lvout = OutportLocation {
                node: theta,
                output: OutputType::Output(lvidx),
            };
            let lv_input_src = if let Some(src) = self.inport_src(InportLocation {
                node: theta,
                input: lv,
            }) {
                src
            } else {
                //if the dst is connected to any, but input is not connected, then stuff is buggy.
                if self[lvout].edges.len() > 0 {
                    return Err(UnrollError::LvInOutConnectionError(lvidx));
                } else {
                    //Otherwise we can just ignore that :)
                    continue;
                }
            };
            for edg in self[lvout].edges.clone() {
                let dst = self[edg].dst().clone();
                let value = self.disconnect(edg).unwrap();
                self.connect(lv_input_src, dst, value).unwrap();
            }
        }

        Ok(())
    }

    ///Unrolls the `theta` node `unroll_count` times, taking care of hooking up the loop-variables appropriately in-between unrolled loops.
    ///
    ///Hooks up the last unrolled loop-variable-output to the original `theta` variable loop outputs.
    ///
    /// This means given a loop with two-loop variables x,y, that _would_ iterate 3 times, if you unroll it twice,
    /// then the $x_{1}$ value would be hooked up from the loop-variable-output of theta (after one (remaining) iteration $x_{1}$) to the first unroll-use of `x`.
    ///
    /// Or in other terms, this unrolls the _tail_ of the loop 2 times, leaving the first iteration in the `theta` node.
    ///
    /// **Caution**: make sure to update you loop criteria after unrolling. This is not handeled by the function, since it can't possibly
    /// _know_ how your loop-criterion works.
    pub fn unroll_theta_tail(
        &mut self,
        theta: NodeRef,
        unroll_count: usize,
    ) -> Result<(), UnrollError> {
        if !self[theta].node_type.is_theta() {
            return Err(UnrollError::NotThetaNode);
        }
        todo!()
    }

    ///Unrolls the `theta` node `unroll_count` times, taking care of hooking up the loop-variables appropriately in-between unrolled loops.
    ///
    ///Hooks up the last unrolled loop-variable-output to the original `theta` variable loop input.
    ///
    /// This means given a loop with two-loop variables x,y, that _would_ iterate 3 times, if you unroll it twice,
    /// then the $x_{2}$ value would be hooked up to the loop-variable-input of theta (instead of $x_{0}$ which it was before).
    ///
    /// Or in other terms, this unrolls the _head_ of the loop 2 times, leaving the last iteration in the `theta` node.
    ///
    /// **Caution**: make sure to update you loop criteria after unrolling. This is not handeled by the function, since it can't possibly
    /// _know_ how your loop-criterion works.
    pub fn unroll_theta_head(
        &mut self,
        theta: NodeRef,
        unroll_count: usize,
    ) -> Result<(), UnrollError> {
        if !self[theta].node_type.is_theta() {
            return Err(UnrollError::NotThetaNode);
        }

        let theta_region = RegionLocation {
            node: theta,
            region_index: 0,
        };
        let host_region = self[theta].parent.unwrap();

        for unroll_idx in 0..unroll_count {
            //first pull out all nodes of the loop into the region.
            //TODO: One could argue, that we always pull the same nodes out, so we could probably optimize
            //      here.
            let pull_out_map = self.deep_copy_region_without_connection(theta_region, host_region);
            //disconnect all lv-inputs, and connect them to the lv-argument users.
            //then connect the same edges from the lv-result-producer equivalent to the lv-input.

            for lv in self[theta].inport_types() {
                let lvidx = if let InputType::Input(i) = lv {
                    i
                } else {
                    panic!("Invalid graph: Theta-node (which was tested before) cannot have non-input InputType");
                };

                let lv_input = InportLocation {
                    node: theta,
                    input: lv,
                };
                let lv_argument = OutportLocation {
                    node: theta,
                    output: OutputType::Output(lvidx),
                };
                let lv_result = InportLocation {
                    node: theta,
                    input: InputType::Result(lvidx),
                };

                //rewire LV
                if let Some(edg) = self[lv_input].edge.clone() {
                    let original_src = self[edg].src().clone();
                    let disconnected = self.disconnect(edg)?;
                    //since we are in head mode reconnect the
                    //lv-argument-connected equivalent _outside_ of the loop with the old src,
                    //and connect the lv-input with the lv-result-connected-equivalent.

                    //reconnect argument_equivalents
                    for arg_connected in self.outport_dsts(lv_argument) {
                        //map that ports node
                        let remap_node = *pull_out_map
                            .get(&arg_connected.node)
                            .ok_or(UnrollError::RemapError)?;
                        let equivalent_port = InportLocation {
                            node: remap_node,
                            input: arg_connected.input,
                        };
                        self.connect(
                            original_src,
                            equivalent_port,
                            disconnected.structural_copy(),
                        )?;
                    }

                    //if the result was connected, connect the equivalent result-producer to the node.
                    //The other case is not interesting, since in that case the lv-input is not in use _within_
                    //the loop, so it shouldn't be needed anyways. In the second iteration.
                    if let Some(result_connected) = self.inport_src(lv_result) {
                        let eq_node = *pull_out_map
                            .get(&result_connected.node)
                            .ok_or(UnrollError::RemapError)?;
                        let eq_port = OutportLocation {
                            node: eq_node,
                            output: result_connected.output,
                        };
                        self.connect(eq_port, lv_input, disconnected.structural_copy())?;
                    }
                } else {
                    //if lv is not connected, just make sure that it is not in use within the loop as well
                    let result_port = InportLocation {
                        node: theta,
                        input: InputType::Result(lvidx),
                    };
                    if self[result_port].edge.is_some() {
                        return Err(UnrollError::LvInOutConnectionError(lvidx));
                    }
                }
            }
        }

        Ok(())
    }
}
