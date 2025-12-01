/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::{region::RegionLocation, util::abstract_node_type::AbstractNodeType, NodeRef};
use vola_common::VolaError;

use crate::{passes::lazy_type::TypeError, OptError, Optimizer};

///Tests the current optimizer state whether all registered functions (including dead λ nodes) pass the type-check.
pub struct InitialTypeCheck<'opt> {
    pub(crate) optimizer: &'opt mut Optimizer,
    errors: Vec<VolaError<OptError>>,
}

impl<'opt> InitialTypeCheck<'opt> {
    pub fn setup(optimizer: &'opt mut Optimizer) -> Self {
        InitialTypeCheck {
            optimizer,
            errors: Vec::with_capacity(0),
        }
    }

    pub fn execute(mut self) -> Result<(), Vec<VolaError<OptError>>> {
        #[cfg(feature = "log")]
        log::info!("Initial type check\n\t{} Toplevel nodes\n\t{} Concepts\n\t{} Operation / Entity Definitions\n\t{} Implementations\n\t{} Other functions", self.optimizer.graph[self.optimizer.graph.toplevel_region()].nodes.len(), self.optimizer.concepts.len(), self.optimizer.csg_node_defs.len(), self.optimizer.concept_impl.len(), self.optimizer.functions.len());

        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("DUMP_INITIAL_TYPE_CHECK").is_ok()
        {
            self.optimizer
                .push_debug_state(&format!("initial-type-check"));
        }

        // This is solely on the graph level, it'll check that
        // the existing λs do in fact pass the type check. That means that all argument-ports, and all result ports
        // are in fact typed, and that the results do in-fact derive to those set types
        for node in self.optimizer.graph[self.optimizer.graph.toplevel_region()]
            .nodes
            .clone()
        {
            match self.optimizer.graph[node].into_abstract() {
                AbstractNodeType::Lambda => self.check_lambda(node),
                _ => panic!("Invalid graph state, non-λ in top-level region"),
            }
        }

        if self.errors.len() > 0 {
            Err(self.errors)
        } else {
            Ok(())
        }
    }

    fn check_lambda(&mut self, lmd: NodeRef) {
        let fnspan = self.optimizer.find_span(lmd).unwrap();
        let region = RegionLocation {
            node: lmd,
            region_index: 0,
        };
        //check all arguments
        for (idx, arg) in self
            .optimizer
            .graph
            .argument_ports(region)
            .into_iter()
            .enumerate()
        {
            if self.optimizer.typemap.get(&arg.into()).is_none() {
                //NOTE: any argument to a function must have an associated span. At least directly after interning.
                let span = self.optimizer.find_span(arg).unwrap_or(fnspan.clone());
                self.errors.push(VolaError::error_here(
                    TypeError::LambdaArgumentUnset(idx).into(),
                    span,
                    "Not type-set",
                ));
            }
        }

        //check all results by type-deriving them
        for (idx, result) in self
            .optimizer
            .graph
            .result_ports(region)
            .into_iter()
            .enumerate()
        {
            let span = self.optimizer.find_span(result).unwrap_or(fnspan.clone());
            let Some(expected_type) = self.optimizer.typemap.get(&result.into()).cloned() else {
                self.errors.push(VolaError::error_here(
                    TypeError::LambdaResultUnset(idx).into(),
                    span,
                    "unset type for this result",
                ));
                continue;
            };

            let derived_type = match self.optimizer.get_in_type_mut(result) {
                Ok(t) => t,
                Err(e) => {
                    self.errors.push(e.to_error());
                    continue;
                }
            };
            if expected_type != derived_type {
                self.errors.push(VolaError::error_here(
                    TypeError::Collision {
                        location: result.into(),
                        was: expected_type,
                        derived: derived_type,
                    }
                    .into(),
                    span,
                    "here",
                ));
                continue;
            }
        }
    }
}
