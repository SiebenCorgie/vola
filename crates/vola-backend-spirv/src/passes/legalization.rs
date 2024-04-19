/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Walks the current graph, and checks if the nodes and their connection statisfy the SPIR-V spec.
//!
//! Tries to repair any _recoverable_ erros, like forgetting to store some value in a var, before mutating.
//! Might error out if no known _repair_ strategy is known for some error in the graph.

use rvsdg::{nodes::NodeType, SmallColl};
use spirv_grammar_rules::GrammarRules;

use crate::{
    graph::{BackendEdge, BackendOp},
    spv::SpvType,
    BackendSpirvError, SpirvBackend,
};

impl SpirvBackend {
    pub fn legalize(&mut self) -> Result<(), BackendSpirvError> {
        //NOTE: In practice _legalize_ is a set of passes.
        //      We start out by legalizing all nodes based on the SPIR-V grammar. That catches
        //      simple misstakes like operand-count-missmatch, wrong operands etc.
        //
        //      We then use increasingly specialized passes to check for things like type-matching,
        //      and common repairable patterns.
        self.legalize_grammar()?;
        Ok(())
    }

    ///Uses the grammar to legalize all operands of a node.
    fn legalize_grammar(&mut self) -> Result<(), BackendSpirvError> {
        //load the grammar-rules of both used grammar (core and glsl).
        //TODO: Use the bundeled ones, but right now we load from file cause thats faster
        let mut core_rules = GrammarRules::load_core_grammar();
        let mut glsl_rules = GrammarRules::load_glsl_std_450_grammar();

        //NOTE: need to collect upfront, so we can modify the graph in the loop :/
        let reachable = self.graph.walk_reachable().collect::<Vec<_>>();
        for node in reachable {
            //Ignore non-simple nodes atm.
            if !self.graph.node(node).node_type.is_simple() {
                continue;
            }

            //collect the input/output signature for the legalize_pattern call.
            let mut inputs = SmallColl::new();

            for inp in self.graph.node(node).inputs().iter() {
                if let Some(inpedg) = inp.edge {
                    let ty = if let BackendEdge::Value(ty) = &self.graph.edge(inpedg).ty {
                        ty.clone()
                    } else {
                        SpvType::State
                    };
                    inputs.push(ty);
                } else {
                    return Err(BackendSpirvError::Any {
                        text: format!(
                            "Found unconnected port on SPIR-V Node {:?}",
                            self.graph.node(node).node_type.unwrap_simple_ref()
                        ),
                    });
                }
            }

            assert!(self.graph.node(node).outputs().len() == 1);
            let output = if self.graph.node(node).outputs()[0].edges.len() == 0 {
                SpvType::undefined()
            } else {
                if let BackendEdge::Value(t) = &self
                    .graph
                    .edge(self.graph.node(node).outputs()[0].edges[0])
                    .ty
                {
                    t.clone()
                } else {
                    SpvType::State
                }
            };
            //now call the legalizer for the spv op
            if let NodeType::Simple(sn) = &mut self.graph.node_mut(node).node_type {
                if let BackendOp::SpirvOp(op) = &mut sn.op {
                    op.legalize_for_pattern(&mut core_rules, &mut glsl_rules, &inputs, &output)?
                }
            }
        }

        Ok(())
    }
}
