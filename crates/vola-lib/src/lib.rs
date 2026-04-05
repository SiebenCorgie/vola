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
//!
//!
//! ## Example
//!
//! A simple pipeline for WASM byte-code emission looks like this:
//!
//! ```rust, ignore
//! let ast = passes::LowerAst::from_file(my_file).unwrap();
//! let mut module = OptModule::new();
//! module.apply_pass(ast).unwrap();
//! //This runst a standard pipeline (similar to volac) that arrives at
//! //a full lowering of the code.
//! module.standard_pipeline().unwrap();
//!
//! //now load that code into the WASM backend and emit the bytecode
//! let mut wasm = wasm::WasmModule::new();
//! wasm.lower_opt(module).unwrap();
//! //NOTE: if wasm-tools is available, you could run the verification
//! //by flagging this true.
//! let bytes = wasm.build(false);
//! ```
//!
//! The `module` allows you to run use-case specific passes, and even allows you direct access to the underlying IR.
//! You could also lower several ASTs into the same `module`, or run multiple backends on the same optimized IR.
//!
use vola_opt::Optimizer;

use crate::passes::{
    Cleanup, Cnf, Dne, InlineExports, LowerAutodiff, LowerIntervals, Pass, PassError,
    PatterRewriteAll, SpecializeAll,
};

mod interface;
pub use interface::InterfaceDescriptor;
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
    pub fn new() -> Self {
        Self {
            opt: Optimizer::new(),
        }
    }

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

    ///Applies the volac-based standard pipeline that lowers _everything_
    /// to the alge-dialect.
    pub fn standard_pipeline(&mut self) -> Result<(), PassError> {
        self.specialize_csg()?;
        self.apply_pass(InlineExports)?;
        self.apply_pass(LowerIntervals)?;
        self.apply_pass(LowerAutodiff)?;
        self.apply_pass(Cleanup)?;
        Ok(())
    }

    ///Generates the _current_ description of all exported interfaces.
    pub fn build_interface_descriptor(&self) -> InterfaceDescriptor {}
}
