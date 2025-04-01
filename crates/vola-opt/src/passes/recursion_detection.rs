/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::collections::VecDeque;

use ahash::AHashSet;
use rvsdg::{
    edge::{InputType, OutputType},
    nodes::StructuralNode,
    NodeRef, SmallColl,
};
use vola_common::{ariadne::Label, error_reporter, report, Span, VolaError};

use crate::{OptError, Optimizer};

impl Optimizer {
    ///Simple pass that finds recursive calls and reports them as errors.
    pub fn detect_recursive_calls(&self) -> Result<(), Vec<VolaError<OptError>>> {
        //we perform this _simply_ testing each node's CV-dependencies (and transitions, if they contain the node)
        //itself.
        //
        //luckily this is simple, we just need to find all λ-declerations that are connected to a node, and their
        //connected CV-λs etc.

        let mut errors = Vec::with_capacity(0);

        //TODO: We currently only do this for _top-level_ λs.
        //      ATM. it makes no sense to do it for nested ones, but that
        //      depends on the correct calling/building of the graph.
        for node in self.graph[self.graph.toplevel_region()].nodes.iter() {
            if self.graph[*node].node_type.is_lambda() {
                if let Err(e) = self.detect_self(*node) {
                    if let Some(span) = self.find_span(node.into()) {
                        errors.push(VolaError::error_here(e, span, "in this function"))
                    } else {
                        errors.push(VolaError::new(e));
                    }
                }
            }
        }

        //pop the last error after finishin

        if errors.len() > 0 {
            return Err(errors);
        } else {
            Ok(())
        }
    }

    fn detect_self(&self, lambda: NodeRef) -> Result<(), OptError> {
        let mut waiting = VecDeque::default();
        let mut seen = AHashSet::default();
        let get_cvlambdas = |node: NodeRef| {
            let cv_count = self.graph[node]
                .node_type
                .unwrap_lambda_ref()
                .context_variable_count();
            let mut cv_lambdas = SmallColl::default();
            for cv in 0..cv_count {
                let port = node.as_inport_location(InputType::ContextVariableInput(cv));
                if let Some(src) = self.graph.inport_src(port) {
                    if src.output == OutputType::LambdaDeclaration {
                        cv_lambdas.push(src);
                    }
                }
            }

            cv_lambdas
        };

        //init the waiting list with all λ-connected λs
        for initlmd in get_cvlambdas(lambda) {
            if initlmd.node == lambda {
                let name = self
                    .names
                    .get(&lambda.into())
                    .cloned()
                    .unwrap_or("unamed function".to_owned());
                return Err(OptError::Any {
                    text: format!("detected recursive call of \"{name}\""),
                });
            }
            waiting.push_back(initlmd);
        }
        //mark self as seen
        seen.insert(lambda);

        //now work on the waiting list, if we see our-self twice, we can bail
        while let Some(next) = waiting.pop_front() {
            //break loop if needed
            if seen.contains(&next.node) {
                continue;
            }
            //not yet seen, find all its connected λs and push them back
            for connected_lambda in get_cvlambdas(next.node) {
                //If a imported called λ imports `self`, this is recursion, which is not allowed
                if connected_lambda == lambda.as_outport_location(OutputType::LambdaDeclaration) {
                    let node_name = self
                        .names
                        .get(&next.node.into())
                        .cloned()
                        .unwrap_or("unknown-function".to_owned());
                    return Err(OptError::Any {
                        text: format!("detected recursive call of \"{node_name}\""),
                    });
                }

                waiting.push_back(connected_lambda);
            }

            seen.insert(next.node);
        }

        Ok(())
    }
}
