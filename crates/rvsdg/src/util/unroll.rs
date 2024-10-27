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
//! - [unroll_theta](crate::Rvsdg::unroll_theta_head)
//! - [unroll_replace_theta](crate::Rvsdg::unroll_replace_theta)

use ahash::AHashMap;
use thiserror::Error;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::LangNode,
    region::RegionLocation,
    NodeRef, Rvsdg, SmallColl,
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
    #[error("Loop had non-static bounds")]
    NonStaticBounds,
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
        if unroll_count == 0 {
            return Ok(());
        }

        //Unroll fully
        for _ in 0..unroll_count {
            let _ = self.unroll_theta_head(theta)?;
        }

        //We simply connect any node that is connected to lv-input[n] to each lv-output[n] user.
        let lvcount = self[theta]
            .node_type
            .unwrap_theta_ref()
            .loop_variable_count();
        for lv in 0..lvcount {
            let lvin_src = if let Some(src) = self.inport_src(theta.input(lv)) {
                src
            } else {
                //if the input is not connected, make sure that there are also no result users
                if self[theta.output(lv)].edges.len() > 0 {
                    return Err(UnrollError::LvInOutConnectionError(lv));
                } else {
                    //otherwise, all is fine continue with next port.
                    continue;
                }
            };

            for edg in self[theta.output(lv)].edges.clone() {
                let dst = self[edg].dst().clone();
                let disconnected = self.disconnect(edg)?;
                //now reconnect for the just found src
                self.connect(lvin_src, dst, disconnected)?;
            }
        }

        Ok(())
    }

    ///Unrolls the `theta` node once, taking care of hooking up the loop-variables appropriately in-between unrolled nodes and the theta-node.
    ///
    ///Hooks up the last unrolled loop-variable-output to the original `theta` variable loop outputs.
    ///
    /// This means given a loop with a loop-variable `x`,
    /// then the $x_{1}$ value would be hooked up from the loop-variable-output of theta (after one (remaining) iteration $x_{1}$) to the first unroll-use of `x`.
    ///
    /// Or in other terms, this unrolls the _tail_ of the loop, leaving the previouse iterations in the `theta` node.
    ///
    /// **Caution**: make sure to update you loop criteria after unrolling. This is not handeled by the function, since it can't possibly
    /// _know_ how your loop-predicate works.
    ///
    ///
    /// Returns the 1:1 mapping from any _in-loop_ node to the _out-of-loop_ node.
    pub fn unroll_theta_tail(
        &mut self,
        theta: NodeRef,
        _unroll_count: usize,
    ) -> Result<AHashMap<NodeRef, NodeRef>, UnrollError> {
        if !self[theta].node_type.is_theta() {
            return Err(UnrollError::NotThetaNode);
        }
        todo!()
    }

    ///Unrolls the `theta` node, taking care of hooking up the loop-variables appropriately in-between the unrolled nodes and the remaining theta node.
    ///
    ///Hooks up the last unrolled loop-variable-output to the original `theta` variable loop input.
    ///
    /// This means given a loop with a loop variable `x`,
    /// then the $x_{1}$ value would be hooked up to the loop-variable-input of theta (instead of $x_{0}$ which it was before).
    ///
    /// Or in other terms, this unrolls the _head_ of the loop, leaving the remaining iterations in the `theta` node.
    ///
    /// **Caution**: make sure to update you loop criteria after unrolling. This is not handeled by the function, since it can't possibly
    /// _know_ how your loop-criterion works.
    ///
    ///
    /// Returns the 1:1 mapping from any _in-loop_ node to the _out-of-loop_ node.
    pub fn unroll_theta_head(
        &mut self,
        theta: NodeRef,
    ) -> Result<AHashMap<NodeRef, NodeRef>, UnrollError> {
        if !self[theta].node_type.is_theta() {
            return Err(UnrollError::NotThetaNode);
        }

        let theta_region = RegionLocation {
            node: theta,
            region_index: 0,
        };
        let host_region = self[theta].parent.unwrap();

        //first pull out all nodes of the loop into the region.
        //TODO: One could argue, that we always pull the same nodes out, so we could probably optimize
        //      here.
        let pull_out_map = self.deep_copy_region_without_connection(theta_region, host_region);

        let lvcount = self[theta]
            .node_type
            .unwrap_theta_ref()
            .loop_variable_count();
        let mut input_remapping = SmallColl::with_capacity(lvcount);

        //For each input, record where the new input should come from (and the edge type, if any)
        for lv in 0..lvcount {
            let lvresult = theta.as_inport_location(InputType::Result(lv));
            let result_edg = if let Some(edg) = &self[lvresult].edge {
                *edg
            } else {
                //Ignore lv, is result is not connected at all.
                input_remapping.push(None);
                continue;
            };

            //translate the result edge into a (new_src, edgetype) pair.
            let edg_src = self[result_edg].src().clone();
            let edg_ty = self[result_edg].ty.structural_copy();

            let new_src = if edg_src.node == theta {
                //if the src is the theta node itself, then the new input src must be the currently
                //connected value from outside the theta-node
                let input = edg_src.output.map_out_of_region().unwrap();
                let src = if let Some(src) = self.inport_src(InportLocation { node: theta, input })
                {
                    src
                } else {
                    //if that input is unconnected, then the currently checked result will be unconnected after unrolling
                    input_remapping.push(None);
                    continue;
                };
                src
            } else {
                //is not theta, so find remapped _unrolled_ node
                let remap_node = *pull_out_map
                    .get(&edg_src.node)
                    .ok_or(UnrollError::RemapError)?;
                OutportLocation {
                    node: remap_node,
                    output: edg_src.output,
                }
            };
            //now push the remapping pair
            input_remapping.push(Some((new_src, edg_ty)));
        }
        assert!(input_remapping.len() == lvcount);

        //Similarly, build the output remapping, by checking for each argument, where they are connected to, and
        //remap the outer-equivalent to the (still) currently connected input producer
        let mut output_remapping = SmallColl::with_capacity(lvcount);
        for lv in 0..lvcount {
            let mut remappings = SmallColl::new();
            let lv_input = theta.input(lv);
            let lvarg = theta.as_outport_location(OutputType::Argument(lv));
            let current_lv_input_src = if let Some(src) = self.inport_src(lv_input) {
                src
            } else {
                //if input is not set, make sure the argument is also not in use
                if self[lvarg].edges.len() > 0 {
                    return Err(UnrollError::LvInOutConnectionError(lv));
                } else {
                    //otherwise its fine to ignore that lv
                    output_remapping.push(remappings);
                    continue;
                }
            };
            for edg in self[lvarg].edges.clone() {
                let ty = self[edg].ty.structural_copy();
                let dst = self[edg].dst().clone();

                if dst.node == theta {
                    //NOTE: if we would remap a dst-input, just ignore it. That case is already handeled
                    //      by the input-remapping above.
                    continue;
                }
                let dest_remap = if let Some(remap) = pull_out_map.get(&dst.node) {
                    *remap
                } else {
                    return Err(UnrollError::RemapError);
                };

                remappings.push((
                    current_lv_input_src,
                    InportLocation {
                        node: dest_remap,
                        input: dst.input,
                    },
                    ty,
                ));
            }

            output_remapping.push(remappings);
        }
        assert!(output_remapping.len() == lvcount);

        //after building the remapping _without doing anything_, apply it to the graph
        for (lv, mapping) in input_remapping.into_iter().enumerate() {
            if let Some((new_src, edgty)) = mapping {
                //disconnect that lv-input, if it is connected, then connect with the remapping
                let lvinput = theta.input(lv);
                if let Some(edg) = self[lvinput].edge.clone() {
                    let _ = self.disconnect(edg);
                }

                self.connect(new_src, lvinput, edgty)?;
            }
        }

        //now rewire all input mappings
        for mapping in output_remapping {
            for (src, dst, edg) in mapping {
                self.connect(src, dst, edg)?;
            }
        }

        Ok(pull_out_map)
    }
}
