/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use crate::Target;

use super::PipelineBackend;

pub struct Wasm {
    pub target: Target,
}

impl Wasm {
    pub fn new(target: Target) -> Self {
        Self { target }
    }
}

impl PipelineBackend for Wasm {
    fn opt_pre_finalize(&self, opt: &mut vola_opt::Optimizer) -> Result<(), crate::PipelineError> {
        opt.imm_scalarize()?;
        Ok(())
    }

    fn execute(&mut self, opt: vola_opt::Optimizer) -> Result<Target, crate::PipelineError> {
        let mut backend = vola_backend_wasm::WasmBackend::new();
        backend.intern_module(&opt)?;

        Ok(self.target.clone())
    }
}
