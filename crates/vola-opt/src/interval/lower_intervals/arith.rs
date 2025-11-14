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
    alge::arithmetic::{BinaryArith, BinaryArithOp, UnaryArithOp},
    interval::lower_intervals::LowerIntervals,
    route_new, OptError, OptNode,
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
            _ => todo!(),
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

        let (in_start, in_end) = *self
            .mapping
            .get(&self.optimizer.graph.inport_src(node.input(0)).unwrap())
            .unwrap();

        let (start, end) = match op {
            _ => todo!(),
        };

        //register with the mapping
        assert!(self.mapping.insert(node.output(0), (start, end)).is_none());

        return Ok(());
    }
}
