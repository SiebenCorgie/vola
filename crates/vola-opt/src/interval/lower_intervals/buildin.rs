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
    imm,
    imm::ImmScalar,
    interval::{lower_intervals::LowerIntervals, IntervalError},
    route_new,
    util::Simplify,
    OptError,
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
                //This works similar to the min/max case. The clamp of an interval is just the interval-start max-ed with
                // the lower bound and the interval-end min-ed with the upper bound.
                // I.e given clamp(a, min, max) -> [max(a.start, min.start) .. (min(a.end, max.end))]
                let (start_value, end_value) = get_interval_for_input(0).unwrap();
                let (start_bound_lower, _end_bound_lower) = get_interval_for_input(1).unwrap();
                let (_start_bound_upper, end_bound_upper) = get_interval_for_input(2).unwrap();
                //now build the new interval
                self.optimizer
                    .graph
                    .on_region(&region, |reg| {
                        let start = route_new!(
                            reg,
                            BuildinOp::Max,
                            span.clone(),
                            [start_value, start_bound_lower]
                        );
                        let end = route_new!(
                            reg,
                            BuildinOp::Min,
                            span.clone(),
                            [end_value, end_bound_upper]
                        );
                        (start.output(0), end.output(0))
                    })
                    .unwrap()
            }
            BuildinOp::Cross => {
                //simplify the cross product and prepend the created nodes
                let new_expr = Simplify::new(self.optimizer, node, true)
                    .lower_cross()
                    .unwrap();
                //Now prepend the newly created nodes
                self.node_queue.prepend(new_expr);
                return Ok(());
            }
            BuildinOp::Dot => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("dot".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::Exp => {
                //as simple as [exp(start), exp(end)]
                let (start, end) = get_interval_for_input(0).unwrap();
                self.optimizer
                    .graph
                    .on_region(&region, |reg| {
                        let istart = route_new!(reg, BuildinOp::Exp, span.clone(), [start]);
                        let iend = route_new!(reg, BuildinOp::Exp, span.clone(), [end]);
                        (istart.output(0), iend.output(0))
                    })
                    .unwrap()
            }
            BuildinOp::Length => {
                //For 'length' there is an interesting implementation in
                // "Introduction to interval analysis" by Moore/ Kaerfortt / Could
                // that thinks of the interval-of a vector as a n-dimentional box.
                // I.e. the 2D vector interval is a _region_ on x and y. They use this
                // property to formulate a tight version of the euklidian
                // length / norm operator.

                //calc center / extent component wise. Then reduce into interval.
                let (start, end) = get_interval_for_input(0).unwrap();
                let span = self.optimizer.find_span(node).unwrap();
                let region = self.optimizer.graph[node].parent.unwrap();
                let interval_type = self.optimizer.get_out_type_mut(start).unwrap();
                let half = self
                    .optimizer
                    .splat_scalar(region, ImmScalar::new(0.5), interval_type);
                self.optimizer
                    .graph
                    .on_region(&region, |reg| {
                        //calculate the per-component center c
                        let start_plus_end =
                            route_new!(reg, BinaryArithOp::Add, span.clone(), [start, end]);
                        let center = route_new!(
                            reg,
                            BinaryArithOp::Mul,
                            span.clone(),
                            [start_plus_end.output(0), half]
                        );

                        //calculate the per-component radius r
                        let end_minus_start =
                            route_new!(reg, BinaryArithOp::Sub, span.clone(), [end, start]);
                        let radius = route_new!(
                            reg,
                            BinaryArithOp::Mul,
                            span.clone(),
                            [end_minus_start.output(0), half]
                        );
                        //now build the length of both
                        let norm_center =
                            route_new!(reg, BuildinOp::Length, span.clone(), [center.output(0)]);
                        let norm_radius =
                            route_new!(reg, BuildinOp::Length, span.clone(), [radius.output(0)]);

                        //and setup the new _interval_
                        //lower is max(0.0, norm_center - norm_radius)
                        let zero_scalar = imm!(reg, Real, 0.0);
                        let nc_minus_nr = route_new!(
                            reg,
                            BinaryArithOp::Sub,
                            span.clone(),
                            [norm_center.output(0), norm_radius.output(0)]
                        );
                        let istart = route_new!(
                            reg,
                            BuildinOp::Max,
                            span.clone(),
                            [zero_scalar.output(0), nc_minus_nr.output(0)]
                        );
                        //upper is norm_center + norm_radius
                        let iend = route_new!(
                            reg,
                            BinaryArithOp::Add,
                            span,
                            [norm_center.output(0), norm_radius.output(0)]
                        );
                        (istart.output(0), iend.output(0))
                    })
                    .unwrap()
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
                    IntervalError::UnsupportedOp("mix".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::Pow => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("pow".to_string()).into(),
                    span,
                    "here",
                ))
            }
            BuildinOp::SquareRoot => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp("sqrt".to_string()).into(),
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
