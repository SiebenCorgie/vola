use rvsdg::err::GraphError;
use vola_common::{
    thiserror::{self, Error},
    Reportable,
};
use vola_opt::OptError;

impl Reportable for PipelineError {}

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
    AstError(#[from] vola_ast::AstError),
}
