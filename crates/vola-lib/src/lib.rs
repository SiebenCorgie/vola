//! # Vola-Lib
//!
//! _Library interface to the Vola compiler._
//!
//! ## Usage
//!
//! The library makes working with Vola's [Optimizer](vola_opt::Optimizer) _simpler_.
//! There are known [passes] that hide details of the actual optimizer pass.
//!
//! Its also possible to access the [Optimizer](vola_opt::Optimizer) directly to run _custom_ passes as well.
//!
//! ## Emitting Code
//!
//! Once finished optimizing you can use either the [spirv](spirv) or [wasm](wasm) modules to
//! further optimize (backend-specific), or emit code for those platforms.

use vola_opt::Optimizer;

use crate::passes::{Cnf, Dne, Pass, PassError, PatterRewriteAll, SpecializeAll};

///Collection of known passes
pub mod passes;
#[cfg(feature = "spirv")]
pub mod spirv;
#[cfg(feature = "wasm")]
pub mod wasm;

///A _optimization compilation context_.
pub struct OptModule {
    pub opt: Optimizer,
}

impl OptModule {
    pub fn apply_pass(&mut self, pass: impl Pass) -> Result<(), PassError> {
        pass.execute(&mut self.opt)
    }

    ///Specializes all CSG nodes in the graph
    pub fn specialize_csg(&mut self) -> Result<(), PassError> {
        //First part ends after specialization
        self.apply_pass(PatterRewriteAll)?;
        self.apply_pass(Cnf)?;
        self.apply_pass(SpecializeAll)?;
        self.apply_pass(Dne)?;
        self.apply_pass(Cnf)?;
        Ok(())
    }
}
