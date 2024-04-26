use rvsdg::err::GraphError;
use vola_ast::ParserError;
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
    ParserError(#[from] ParserError),
    #[error(transparent)]
    RVSDGError(#[from] GraphError),
    #[error(transparent)]
    SpirvError(#[from] vola_backend_spirv::BackendSpirvError),
    #[error(transparent)]
    AstError(#[from] vola_ast::AstError),
}
