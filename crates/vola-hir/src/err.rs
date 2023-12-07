use rvsdg::err::{BuilderError, GraphError};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum HirErr {
    #[error("Any error occured")]
    Any,
    #[error("RVSDG error: {0}")]
    GraphError(#[from] GraphError),
    #[error("RVSDG builder error: {0}")]
    RvsdgBuilderError(#[from] BuilderError),
}

impl Default for HirErr {
    fn default() -> Self {
        HirErr::Any
    }
}
