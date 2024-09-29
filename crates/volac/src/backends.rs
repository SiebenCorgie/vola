/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

mod spriv;
pub use spriv::Spirv;
mod wasm;
pub use wasm::Wasm;

use vola_opt::Optimizer;

use crate::{PipelineError, Target};

pub trait PipelineBackend {
    ///Gets executed before the optimizer is finalized.
    fn opt_pre_finalize(&self, opt: &mut Optimizer) -> Result<(), PipelineError>;

    ///Moves ownership of the (finalized) optimizer to the backend. Returns the final binary (path or buffer)
    fn execute(&mut self, opt: Optimizer) -> Result<Target, PipelineError>;
}
