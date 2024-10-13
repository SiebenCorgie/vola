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
    #[error("Parser failed with: {0}")]
    ParserError(String),
    #[error(transparent)]
    RVSDGError(#[from] GraphError),
    #[error(transparent)]
    SpirvError(#[from] vola_backend_spirv::BackendSpirvError),
    #[error(transparent)]
    WasmError(#[from] vola_backend_wasm::WasmError),
    #[error(transparent)]
    AstError(#[from] vola_ast::AstError),
    #[error(transparent)]
    CnfError(#[from] CnfError),
    #[error(transparent)]
    CneError(#[from] CneError),
    #[error("Failed to validate: {0}")]
    ValidationFailed(String),
}
