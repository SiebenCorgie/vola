/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use rvsdg::{
    edge::{OutportLocation, OutputType},
    util::abstract_node_type::AbstractNodeType,
    NodeRef,
};

use crate::Optimizer;

impl Optimizer {
    ///Returns the value producer port for this node,
    ///as defined by the optimizer's convention.
    ///
    /// Note that this convention is undefined for
    /// Theta, Phi and Delta nodes and Lambda.
    pub fn value_producer_port(&self, node: NodeRef) -> Option<OutportLocation> {
        match self.graph[node].into_abstract() {
            AbstractNodeType::Simple => Some(node.output(0)),
            AbstractNodeType::Gamma => {
                Some(node.as_outport_location(OutputType::ExitVariableOutput(0)))
            }
            AbstractNodeType::Theta => Some(node.output(2)),
            AbstractNodeType::Apply => Some(node.output(0)),
            _ => None,
        }
    }
}
