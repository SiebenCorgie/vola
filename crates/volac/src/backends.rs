/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

#[cfg(feature = "spirv")]
mod spriv;
use std::panic::UnwindSafe;

#[cfg(feature = "spirv")]
pub use spriv::Spirv;

#[cfg(feature = "wasm")]
mod wasm;
#[cfg(feature = "wasm")]
pub use wasm::Wasm;

#[cfg(feature = "native")]
mod native;

#[cfg(feature = "native")]
pub use native::Native;

use vola_opt::Optimizer;

use crate::{PipelineError, Target};

///Shortcut type to refer to something that can become a boxed pipeline backend
pub type BoxableBackend = dyn PipelineBackend + Send + Sync + UnwindSafe;

///Shortcut type to refer to a boxed, `dyn` backend.
pub type BoxedBackend = Box<dyn PipelineBackend + Send + Sync + UnwindSafe>;

pub trait PipelineBackend {
    ///Gets executed before the optimizer is finalized.
    fn opt_pre_finalize(&self, opt: &mut Optimizer) -> Result<(), PipelineError>;

    ///Moves ownership of the (finalized) optimizer to the backend. Returns the final binary (path or buffer)
    fn execute(&mut self, opt: Optimizer) -> Result<Target, PipelineError>;

    ///If implemented tries to use installed tools to verify the emitted artifact.
    fn try_verify(&self) -> Result<(), String> {
        Ok(())
    }
}

pub struct StubBackend;
impl Default for StubBackend {
    fn default() -> Self {
        StubBackend
    }
}
impl PipelineBackend for StubBackend {
    fn execute(&mut self, _opt: Optimizer) -> Result<Target, PipelineError> {
        Err(PipelineError::IsStub)
    }

    fn try_verify(&self) -> Result<(), String> {
        Ok(())
    }

    fn opt_pre_finalize(&self, _opt: &mut Optimizer) -> Result<(), PipelineError> {
        Err(PipelineError::IsStub)
    }
}
