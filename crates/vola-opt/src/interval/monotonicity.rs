/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::NodeRef;

use crate::Optimizer;

pub enum Monotonicity {
    Increasing,
    Decreasing,
    Other,
}

impl Monotonicity {
    ///Tries to find the monotonicity property of `node`. This might fail for non-alge dialect nodes, i.e. nodes
    /// where the monotonicity property makes no sense.
    ///
    /// Will return None for any none-simple node
    pub fn from_node(opt: &mut Optimizer, node: NodeRef) -> Option<Self> {
        if !opt.graph[node].node_type.is_simple() {
            return None;
        }

        //Currently bail all non-alge nodes
        if "alge" != opt.graph[node].node_type.unwrap_simple_ref().node.dialect() {
            return None;
        }

        //Rest of the horse
        todo!()
    }
}
