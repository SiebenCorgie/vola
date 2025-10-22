/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::edge::OutportLocation;
use vola_common::VolaError;

use crate::{OptError, Optimizer};

/// Pass that replace any interval-operation with _normal_ operations over tuple elements.
pub struct IntervalToTuple<'opt> {
    optimizer: &'opt Optimizer,
}

impl<'opt> IntervalToTuple<'opt> {
    pub fn setup(optimizer: &'opt Optimizer) -> Self {
        IntervalToTuple { optimizer }
    }

    ///Converts all live intervals to tuple.
    pub fn convert_live(&self) -> Result<(), VolaError<OptError>> {
        let live_intervals = self.collect_live_intervals();

        for live in live_intervals {
            self.handle_value(live)?;
        }

        Ok(())
    }

    fn collect_live_intervals(&self) -> Vec<OutportLocation> {
        let mut found = Vec::with_capacity(0);
        todo!();

        found
    }

    ///Replaces the interval of `value` with the tuple of `(start, end).
    ///
    /// Note that this traces the whole expression till no interval operations are found.
    pub fn handle_value(&self, value: OutportLocation) -> Result<(), VolaError<OptError>> {
        todo!();
    }
}
