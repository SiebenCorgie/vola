/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Hosts the initial type-resolve pass. This is used
//! after lowering an AST to type-check anything that is lowered
//! for validity.

use rvsdg::SmallColl;
use vola_common::VolaError;

use crate::{OptError, Optimizer};
impl Optimizer {
    ///The initial type-check of the full graph. Considers all nodes, including dead nodes, assuming that they'd
    /// still fullfill the type-derive assumptions.
    pub fn initial_type_check(&mut self) -> Result<(), SmallColl<VolaError<OptError>>> {
        todo!()
    }
}
