/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use vola_common::VolaError;

use crate::{interval::extension::IntervalExtensionPass, OptError, Optimizer};

impl Optimizer {
    pub fn interval_extension(&mut self) -> Result<(), VolaError<OptError>> {
        #[cfg(feature = "log")]
        log::info!("interval-extension");

        IntervalExtensionPass::setup(self).extend_all()
    }

    pub fn interval_to_tupel(&mut self) -> Result<(), VolaError<OptError>> {
        #[cfg(feature = "log")]
        log::info!("interval to tupel");

        IntervalExtensionPass::setup(self).extend_all()
    }
}
