/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Branching related rewrites.

use rvsdg::{edge::InputType, nodes::NodeType, Rvsdg};
use rvsdg_pattern_rewrite::{benefit::Speed, PatternRewrite};

use crate::{imm::ImmBool, OptEdge, OptNode};

///If the chosen branch is statically known, specializes control flow to that branch
pub struct SpecializeStaticBranch;
impl PatternRewrite<OptNode, OptEdge, Speed> for SpecializeStaticBranch {
    fn benefit(&self) -> &Speed {
        &Speed(10)
    }

    fn matches(&self, graph: &Rvsdg<OptNode, OptEdge>, node: rvsdg::NodeRef) -> bool {
        //check if the node is a gamma node, and its decission is statically known
        if !graph[node].node_type.is_gamma() {
            return false;
        }

        let Some(predicate_src) =
            graph.inport_src(node.as_inport_location(InputType::GammaPredicate))
        else {
            return false;
        };

        //now try to downcast to a boolean value
        if let NodeType::Simple(s) = &graph[predicate_src.node].node_type {
            if let Some(_bool) = s.try_downcast_ref::<ImmBool>() {
                //downcasts to bool, therefore return true
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn apply(&self, graph: &mut rvsdg::Rvsdg<OptNode, OptEdge>, node: rvsdg::NodeRef) {
        log::info!("Apply SpecializeStaticBranch to {node}");

        let predicate = graph
            .inport_src(node.as_inport_location(InputType::GammaPredicate))
            .expect("Expected gamma-predicate producer to still exist");
        let bool_value = graph[predicate.node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<ImmBool>()
            .expect("Expected predicate producer to (still) be bool-imm")
            .lit;

        //now, based on the bool, pull out one of both branches
        //luckily we already have a helper for that
        graph.gamma_specialize_for_branch(node, if bool_value { 0 } else { 1 });
    }
}
