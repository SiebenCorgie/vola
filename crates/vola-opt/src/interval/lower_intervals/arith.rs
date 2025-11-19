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
    alge::arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp},
    imm::ImmScalar,
    interval::{lower_intervals::LowerIntervals, IntervalError},
    route_new, OptError,
};
impl<'opt> LowerIntervals<'opt> {
    pub(crate) fn lower_binary_arith(
        &mut self,
        region: RegionLocation,
        span: Span,
        node: NodeRef,
        op: BinaryArithOp,
    ) -> Result<(), VolaError<OptError>> {
        assert_eq!(self.optimizer.graph[node].inputs().len(), 2);

        let (a_start, a_end) = *self
            .mapping
            .get(&self.optimizer.graph.inport_src(node.input(0)).unwrap())
            .unwrap();
        let (b_start, b_end) = *self
            .mapping
            .get(&self.optimizer.graph.inport_src(node.input(1)).unwrap())
            .unwrap();

        let (start, end) = match op {
            BinaryArithOp::Add => self
                .optimizer
                .graph
                .on_region(&region, |reg| {
                    //[a1+b1, a2+b2]
                    let add_lower =
                        route_new!(reg, BinaryArithOp::Add, span.clone(), [a_start, b_start]);
                    let add_higher =
                        route_new!(reg, BinaryArithOp::Add, span.clone(), [a_end, b_end]);

                    (add_lower.output(0), add_higher.output(0))
                })
                .unwrap(),
            BinaryArithOp::Sub => self
                .optimizer
                .graph
                .on_region(&region, |reg| {
                    //[a1-b2, a2-b1]
                    let sub_lower =
                        route_new!(reg, BinaryArithOp::Sub, span.clone(), [a_start, b_end]);
                    let sub_upper =
                        route_new!(reg, BinaryArithOp::Sub, span.clone(), [a_end, b_start]);

                    (sub_lower.output(0), sub_upper.output(0))
                })
                .unwrap(),
            BinaryArithOp::Mul | BinaryArithOp::Div => {
                self.optimizer
                    .graph
                    .on_region(&region, |reg| {
                        //Currently the general-case that find the minimum/maximum of all possibilities.
                        //
                        // For Mul this is subobtimal. For Div its actually wrong, because, if the interval contains 0, it should be NAN.
                        // TODO: Fix the DIV case (runtime shell-out via branch?).
                        let a1b1 =
                            route_new!(reg, BinaryArith::new(op), span.clone(), [a_start, b_start])
                                .output(0);
                        let a1b2 =
                            route_new!(reg, BinaryArith::new(op), span.clone(), [a_start, b_end])
                                .output(0);
                        let a2b1 =
                            route_new!(reg, BinaryArith::new(op), span.clone(), [a_end, b_start])
                                .output(0);
                        let a2b2 =
                            route_new!(reg, BinaryArith::new(op), span.clone(), [a_end, b_end])
                                .output(0);

                        let min0 =
                            route_new!(reg, BuildinOp::Min, span.clone(), [a1b1, a1b2]).output(0);
                        let min1 =
                            route_new!(reg, BuildinOp::Min, span.clone(), [min0, a2b1]).output(0);
                        let min2 =
                            route_new!(reg, BuildinOp::Min, span.clone(), [min1, a2b2]).output(0);

                        let max0 =
                            route_new!(reg, BuildinOp::Max, span.clone(), [a1b1, a1b2]).output(0);
                        let max1 =
                            route_new!(reg, BuildinOp::Max, span.clone(), [max0, a2b1]).output(0);
                        let max2 =
                            route_new!(reg, BuildinOp::Max, span.clone(), [max1, a2b2]).output(0);

                        (min2, max2)
                    })
                    .unwrap()
            }
            other => {
                return Err(VolaError::error_here(
                    IntervalError::UnsupportedOp(format!("{other:?}")).into(),
                    span,
                    "here",
                ))
            }
        };

        //register with the mapping
        assert!(self.mapping.insert(node.output(0), (start, end)).is_none());
        return Ok(());
    }

    pub(crate) fn lower_unary_arith(
        &mut self,
        region: RegionLocation,
        span: Span,
        node: NodeRef,
        op: UnaryArithOp,
    ) -> Result<(), VolaError<OptError>> {
        assert_eq!(self.optimizer.graph[node].inputs().len(), 1);
        let original_src = self.optimizer.graph.inport_src(node.input(0)).unwrap();
        let (in_start, in_end) = *self.mapping.get(&original_src).unwrap();

        let inty = self.optimizer.get_out_type_mut(original_src).unwrap();
        if !inty.is_scalar_arithmetic() {
            return Err(VolaError::error_here(
                IntervalError::UnsupportedOp(format!(
                    "{op:?}: Interval arithmetic only supported on real valued types!"
                ))
                .into(),
                span,
                "not real valued",
            ));
        }

        let (start, end) = match op {
            UnaryArithOp::Abs => {
                //Currently defaulting to the conservative 0..max(abs(start), abs(end)).
                // TODO: tighten if we can derive any knowledge about the input interval
                let start = self
                    .optimizer
                    .splat_scalar(region, ImmScalar::new(0.0), inty);
                let end = self
                    .optimizer
                    .graph
                    .on_region(&region, |reg| {
                        let abs_start = route_new!(reg, UnaryArithOp::Abs, span.clone(), in_start);
                        let abs_end = route_new!(reg, UnaryArithOp::Abs, span.clone(), in_end);
                        let max = route_new!(
                            reg,
                            BuildinOp::Max,
                            span,
                            [abs_start.output(0), abs_end.output(0)]
                        );
                        max.output(0)
                    })
                    .unwrap();
                (start, end)
            }
            UnaryArithOp::Neg => {
                //simply [-end, -start]
                self.optimizer
                    .graph
                    .on_region(&region, |reg| {
                        let neg_start = route_new!(reg, UnaryArithOp::Neg, span.clone(), in_start);
                        let neg_end = route_new!(reg, UnaryArithOp::Neg, span.clone(), in_end);
                        (neg_end.output(0), neg_start.output(0))
                    })
                    .unwrap()
            }
            UnaryArithOp::Ceil | UnaryArithOp::Floor | UnaryArithOp::Round => {
                //Simply piece-wise application
                self.optimizer
                    .graph
                    .on_region(&region, |reg| {
                        let new_start =
                            route_new!(reg, UnaryArith::new(op), span.clone(), [in_start]);
                        let new_end = route_new!(reg, UnaryArith::new(op), span.clone(), [in_end]);
                        (new_end.output(0), new_start.output(0))
                    })
                    .unwrap()
            }
            UnaryArithOp::Fract => {
                //Falling back to the [0..1] interval for now.
                let start = self
                    .optimizer
                    .splat_scalar(region, ImmScalar::new(0.0), inty.clone());
                let end = self
                    .optimizer
                    .splat_scalar(region, ImmScalar::new(1.0), inty);
                (start, end)
            }
        };

        //register with the mapping
        assert!(self.mapping.insert(node.output(0), (start, end)).is_none());

        Ok(())
    }
}
