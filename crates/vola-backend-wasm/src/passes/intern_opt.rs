/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use vola_opt::Optimizer;

use crate::{error::WasmError, WasmBackend};

impl WasmBackend {
    pub(crate) fn intern_optimizer(&mut self, optimizer: &Optimizer) -> Result<(), WasmError> {
        todo!()
    }
}
