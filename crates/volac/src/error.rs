use rvsdg::{
    err::GraphError,
    util::{cne::CneError, cnf::CnfError},
};
use vola_common::thiserror::{self, Error};
use vola_opt::OptError;

///Error type collection that can happen at compile-time.
///Mostly transparent errors
#[derive(Error, Debug)]
pub enum PipelineError {
    #[error(transparent)]
    IoErr(#[from] std::io::Error),
    #[error(transparent)]
    OptError(#[from] OptError),
    #[error(transparent)]
    RVSDGError(#[from] GraphError),
    #[cfg(feature = "spirv")]
    #[error(transparent)]
    SpirvError(#[from] vola_backend_spirv::BackendSpirvError),
    #[cfg(feature = "wasm")]
    #[error(transparent)]
    WasmError(#[from] vola_backend_wasm::WasmError),
    #[cfg(feature = "native")]
    #[error("Could not find native ISA: {0}")]
    NativeIsaError(String),

    #[error(transparent)]
    AstError(#[from] vola_ast::AstError),
    #[error(transparent)]
    CnfError(#[from] CnfError),
    #[error(transparent)]
    CneError(#[from] CneError),
    #[error("Failed to validate: {0}")]
    ValidationFailed(String),
    #[error("No backend configured!")]
    IsStub,
}
