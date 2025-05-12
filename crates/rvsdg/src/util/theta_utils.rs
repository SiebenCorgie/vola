/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Implements loop/theta related utility functions.
use crate::{
    edge::{InputType, LangEdge, OutputType},
    nodes::LangNode,
    NodeRef, Rvsdg,
};

impl<N: LangNode, E: LangEdge> Rvsdg<N, E> {
    ///Returns true if the `lv`-loop-variable is invariant, so doesn't change with iterations.
    ///
    ///Reutrns false if `theta_node` is in fact not a theta node, the `lv`-th loop-variable is not existing, or `lv` is in fact not loop invariant.
    ///A 'lv' is also **not** invariant, if the `lv` port is internally unconnected, so not producing a result. The reason is, that it wouldn't produce a value,
    ///even, if one is supplied to the initial iteration. So the invariant _input-lv(0)=a produces output-lv(0)=a for any `n` iterations_ is not given.
    pub fn is_loop_invariant(&self, theta_node: NodeRef, lv: usize) -> bool {
        if !self[theta_node].node_type.is_theta() {
            return false;
        }

        if self[theta_node]
            .node_type
            .unwrap_theta_ref()
            .loop_variable_count()
            <= lv
        {
            return false;
        }

        //aight, can index the lv.
        let result_port = InputType::Result(lv).to_location(theta_node);
        if let Some(src) = self.inport_src(result_port) {
            if src.node != theta_node {
                //Not even the same node :(
                false
            } else {
                match src.output {
                    //Is an loop-argument, on our theta, check that its the same arg, if so we are indeed invariant
                    OutputType::Argument(n) => n == lv,
                    //Wrong output type
                    _ => false,
                }
            }
        } else {
            false
        }
    }
}
