/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::{region::RegionLocation, NodeRef};
use vola_common::{Span, VolaError};

use crate::{
    alge::buildin::{Buildin, BuildinOp},
    interval::{lower_intervals::LowerIntervals, IntervalError},
    route_new, OptError,
};

impl<'opt> LowerIntervals<'opt> {
    pub(crate) fn lower_buildin(
        &mut self,
        region: RegionLocation,
        span: Span,
        node: NodeRef,
        op: BuildinOp,
    ) -> Result<(), VolaError<OptError>> {
        let get_interval_for_input = |input: usize| {
            self.mapping
                .get(&self.optimizer.graph.inport_src(node.input(input)).unwrap())
                .cloned()
        };

        let (start, end) = match op {
            BuildinOp::Clamp => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("clamp".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::Cross => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("clamp".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::Dot => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("clamp".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::Exp => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("clamp".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::Length => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("clamp".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::Max | BuildinOp::Min => {
                //simply [op(start_a, start_b), op(end_a, end_b)]
                let (start_a, end_a) = get_interval_for_input(0).unwrap();
                let (start_b, end_b) = get_interval_for_input(1).unwrap();
                //now build the new interval
                self.optimizer
                    .graph
                    .on_region(&region, |reg| {
                        let start =
                            route_new!(reg, Buildin::new(op), span.clone(), [start_a, start_b]);
                        let end = route_new!(reg, Buildin::new(op), span.clone(), [end_a, end_b]);
                        (start.output(0), end.output(0))
                    })
                    .unwrap()
            }
            BuildinOp::Mix => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("clamp".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::Pow => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("clamp".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::SquareRoot => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("clamp".to_string()).into(),
                    span,
                    "here",
                ))
            }
        };

        //register with the mapping
        assert!(self.mapping.insert(node.output(0), (start, end)).is_none());

        Ok(())
    }
}
