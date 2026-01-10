/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use std::f64;

use rvsdg::{edge::OutportLocation, region::RegionLocation, NodeRef};
use vola_common::{Span, VolaError};

use crate::imm;
use crate::{
    alge::buildin::{Buildin, BuildinOp},
    imm::ImmScalar,
    interval::{lower_intervals::LowerIntervals, IntervalError},
    route_new, OptError,
};

impl<'opt> LowerIntervals<'opt> {
    fn interval_for_input(
        &mut self,
        node: NodeRef,
        index: usize,
    ) -> (OutportLocation, OutportLocation) {
        self.get_or_build_interval(self.optimizer.graph.inport_src(node.input(index)).unwrap())
    }

    pub(crate) fn lower_buildin(
        &mut self,
        region: RegionLocation,
        span: Span,
        node: NodeRef,
        op: BuildinOp,
    ) -> Result<(), VolaError<OptError>> {
        let (start, end) = match op {
            BuildinOp::Clamp => {
                //This works similar to the min/max case. The clamp of an interval is just the interval-start max-ed with
                // the lower bound and the interval-end min-ed with the upper bound.
                // I.e given clamp(a, min, max) -> [max(a.start, min.start) .. (min(a.end, max.end))]
                let (start_value, end_value) = self.interval_for_input(node, 0);
                let (start_bound_lower, _end_bound_lower) = self.interval_for_input(node, 1);
                let (_start_bound_upper, end_bound_upper) = self.interval_for_input(node, 2);
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
            BuildinOp::Exp => {
                //as simple as [exp(start), exp(end)]
                let (start, end) = self.interval_for_input(node, 0);
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
                let (start, end) = self.interval_for_input(node, 0);
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
                let (start_a, end_a) = self.interval_for_input(node, 0);
                let (start_b, end_b) = self.interval_for_input(node, 1);
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
                //We are conservative here and just use the min/max of the mixed values
                // TODO: Inspect the alpha value, if its an interval, use that info to tighten the min/max
                //       values
                let (a_start, a_end) = self.interval_for_input(node, 0);
                let (b_start, b_end) = self.interval_for_input(node, 1);
                self.optimizer
                    .graph
                    .on_region(&region, |reg| {
                        let min = route_new!(reg, BuildinOp::Min, span.clone(), [a_start, b_start]);
                        let max = route_new!(reg, BuildinOp::Max, span.clone(), [a_end, b_end]);
                        (min.output(0), max.output(0))
                    })
                    .unwrap()
            }
            BuildinOp::Pow => {
                //For this we are lazy as fuck at the moment and return the unbound interval
                // [-INF, INF]
                // TODO: This should... at some point, inspect the exponent etc.
                let (in_low, in_up) = self.interval_for_input(node, 0);
                let in_low_ty = self.optimizer.get_out_type_mut(in_low).unwrap();
                let in_up_ty = self.optimizer.get_out_type_mut(in_up).unwrap();
                assert_eq!(in_low_ty, in_up_ty);
                let lower = self.optimizer.splat_scalar(
                    region,
                    ImmScalar::new(f64::NEG_INFINITY),
                    in_low_ty.clone(),
                );
                let upper =
                    self.optimizer
                        .splat_scalar(region, ImmScalar::new(f64::INFINITY), in_up_ty);
                (lower, upper)
            }
            BuildinOp::SquareRoot => {
                //as simple as [sqrt(start), sqrt(end)]
                // TODO: We _should_ modify this to test whether the full
                // interval is in the positive space. Right now this might introduce NANs
                // is either is negative.
                let (start, end) = self.interval_for_input(node, 0);
                self.optimizer
                    .graph
                    .on_region(&region, |reg| {
                        let istart = route_new!(reg, BuildinOp::SquareRoot, span.clone(), [start]);
                        let iend = route_new!(reg, BuildinOp::SquareRoot, span.clone(), [end]);
                        (istart.output(0), iend.output(0))
                    })
                    .unwrap()
            }
            other => {
                return Err(VolaError::error_here(
                    OptError::Interval(IntervalError::UnsupportedOp(format!("{other:?}"))),
                    span,
                    "Encountered unsupported Op.",
                ))
            }
        };

        //register with the mapping
        assert!(self.mapping.insert(node.output(0), (start, end)).is_none());

        Ok(())
    }
}
