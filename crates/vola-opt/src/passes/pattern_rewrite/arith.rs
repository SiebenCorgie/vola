/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Collection of arithmetic rewrites.

use rvsdg::{nodes::NodeType, smallvec::SmallVec};
use rvsdg_pattern_rewrite::{PatternRewrite, Speed};

use crate::{alge::arithmetic::BinaryArith, DialectNode, OptEdge, OptNode};

///Folds
pub struct FoldBinary;
impl PatternRewrite<OptNode, OptEdge, Speed> for FoldBinary {
    fn matches(&self, graph: &rvsdg::Rvsdg<OptNode, OptEdge>, node: rvsdg::NodeRef) -> bool {
        //if the node is a binary node, and the two source nodes are in the immediate dialect,
        // matches...
        if let NodeType::Simple(s) = &graph[node].node_type {
            if s.try_downcast_ref::<BinaryArith>().is_none() {
                //Not binary arith
                return false;
            }
        } else {
            //Not simple note
            return false;
        }

        //At this point we _know_ its a binary arith, so its safe to assume two inputs
        let Some(lhs) = graph.inport_src(node.input(0)) else {
            return false;
        };
        let Some(rhs) = graph.inport_src(node.input(1)) else {
            return false;
        };
        if let (NodeType::Simple(lhs), NodeType::Simple(rhs)) =
            (&graph[lhs.node].node_type, &graph[rhs.node].node_type)
        {
            //If both are immediate values we are good to go...
            lhs.node.dialect() == "Imm" && rhs.node.dialect() == "Imm"
        } else {
            false
        }
    }

    fn benefit(&self) -> &Speed {
        &Speed(10)
    }

    fn apply(&self, graph: &mut rvsdg::Rvsdg<OptNode, OptEdge>, node: rvsdg::NodeRef) {
        //right now we can just reuse the old folding code
        let folded = {
            let source_nodes = graph
                .build_src_map(node)
                .iter()
                .map(|port| port.map(|port| &graph[port.node]))
                .collect::<SmallVec<[_; 2]>>();
            graph[node]
                .node_type
                .unwrap_simple_ref()
                .try_downcast_ref::<BinaryArith>()
                .expect("Should be binary arith, if matched before")
                .try_constant_fold(&source_nodes)
                .expect("Expected folded node")
        };
        let region = graph[node].parent.unwrap();

        log::info!("BinaryFold to {}", folded.node.name());

        let inserted = graph
            .on_region(&region, |reg| reg.insert_node(folded))
            .unwrap();

        graph.replace_node_uses(node, inserted).unwrap();
    }
}
