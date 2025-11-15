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

    ///Implements the heuristic that hints if a apply node should be inlined. Also returns false if `node` is not an Apply node.
    pub fn should_inline_apply(&self, node: NodeRef) -> bool {
        if !self.graph[node].node_type.is_apply() {
            return false;
        }

        //if that node's producer is tagged with no_inline, don't inline as well.
        let lmdsrc = self.graph.inport_src(node.input(0)).unwrap();
        let calldef = if let Some(prod) = self.graph.find_callabel_def(lmdsrc) {
            prod
        } else {
            #[cfg(feature = "log")]
            log::error!("Apply node's callable port was not defined ({node})");
            return false;
        };

        //NOTE: must be at least connected to us
        let users = self.graph.find_caller(calldef.node).unwrap();
        //try to read the inline property, otherwise assume its not tagged no_inline
        println!(
            "Searching for {} in: \n {:#?}",
            calldef.node,
            self.functions
                .values()
                .map(|f| format!("{}:{}:no_inline({})", f.lambda, f.name, f.no_inline))
                .collect::<Vec<_>>()
        );
        let is_no_inline = self
            .functions
            .values()
            .find(|f| f.lambda == calldef.node)
            .map(|f| f.no_inline)
            .unwrap_or(false);

        if is_no_inline {
            println!("Is no inline!");
        }

        //Heuristic currently fires at > 1. So basically, if there is only one user, it won't inline, if there are several, it will
        if users.len() > 1 || is_no_inline {
            false
        } else {
            true
        }
    }
}
