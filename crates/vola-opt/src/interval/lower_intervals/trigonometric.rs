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
    alge::trigonometric::TrigOp,
    common::DataType,
    imm::ImmScalar,
    interval::{lower_intervals::LowerIntervals, IntervalError},
    OptError,
};

impl<'opt> LowerIntervals<'opt> {
    pub(crate) fn lower_trig(
        &mut self,
        region: RegionLocation,
        span: Span,
        node: NodeRef,
        op: TrigOp,
    ) -> Result<(), VolaError<OptError>> {
        let (in_low, in_up) = self
            .mapping
            .get(&self.optimizer.graph.inport_src(node.input(0)).unwrap())
            .unwrap();

        let in_low_ty = self.optimizer.get_or_derive_type(*in_low, false);
        let in_up_ty = self.optimizer.get_or_derive_type(*in_up, false);
        assert_eq!(in_low_ty, in_up_ty);

        //For non-real values we bail atm.
        if !in_low_ty
            .data_type()
            .map_or(false, |dt| dt == DataType::Real)
        {
            return Err(VolaError::error_here(
                IntervalError::UnsupportedOp(format!("{op:?}")).into(),
                span,
                format!("None-Real argument {} unsupported", in_low_ty),
            ));
        }

        match op {
            TrigOp::Sin | TrigOp::Cos => {
                //TODO: Try to do a constant analysis of the inputs to yield tighter bounds
                let lower =
                    self.optimizer
                        .splat_scalar(region, ImmScalar::new(-1.0), in_low_ty.clone());
                let upper = self
                    .optimizer
                    .splat_scalar(region, ImmScalar::new(1.0), in_low_ty);
                assert!(self
                    .mapping
                    .insert(node.output(0), (lower, upper))
                    .is_none());
                Ok(())
            }
            TrigOp::Tan => {
                //TODO Check whether the argument is within +/- π/2 in that case we can do tight bounds.
                let lower = self.optimizer.splat_scalar(
                    region,
                    ImmScalar::new(f64::NEG_INFINITY),
                    in_low_ty.clone(),
                );
                let upper =
                    self.optimizer
                        .splat_scalar(region, ImmScalar::new(f64::INFINITY), in_low_ty);
                assert!(self
                    .mapping
                    .insert(node.output(0), (lower, upper))
                    .is_none());
                Ok(())
            }
            other => Err(VolaError::error_here(
                IntervalError::UnsupportedOp(format!("{other:?}")).into(),
                span,
                "Not yet implemented",
            )),
        }
    }
}
