use std::sync::Mutex;

use rvsdg::err::{BuilderError, GraphError};
use thiserror::Error;

use lazy_static::lazy_static;
use tinyvec::TinyVec;
use vola_common::{CommonError, ErrorReporter, Span};

lazy_static! {
    static ref REPORTER: Mutex<ErrorReporter<HirErr>> = Mutex::new(ErrorReporter::new());
}

///Sets the default file for reported errors.
pub fn set_reporter_file_path(file_path: &str) {
    REPORTER.lock().unwrap().set_default_file(file_path)
}

///Reports an error to the static reporter. All reported errors can be printed via
/// [print_errors].
pub fn report_error(error: CommonError<HirErr>) {
    REPORTER.lock().unwrap().push_error(error)
}

pub fn print_errors() -> TinyVec<[CommonError<HirErr>; 10]> {
    REPORTER.lock().unwrap().report_all()
}

///A pass result can signal, the HIR state, after it is applied. This allows us to decide
/// if we want to continue passes, if we aboard, or if we emit warnings
pub enum PassResult<T> {
    Ok(T),
    ///If we need to abort because of the given error
    Abort(HirErr),
    ///Something happened, but we can continue at least.
    Continue {
        result: T,
        error: Option<HirErr>,
    },
}

impl<T> PassResult<T> {
    pub fn can_continue(&self) -> bool {
        if let PassResult::Abort(_) = &self {
            false
        } else {
            true
        }
    }
    ///Unwraps the result into t, panics if it was Abort.
    /// Resolves to T, but reports the error, if it was `Continue`
    pub fn unwrap(self) -> T {
        match self {
            Self::Abort(err) => panic!("failed to unwrap: {}", err),
            Self::Continue { result, error } => {
                if let Some(err) = error {
                    report_error(CommonError::new(Span::empty(), err));
                }
                result
            }
            Self::Ok(ok) => ok,
        }
    }

    pub fn try_unwrap(mut self) -> Option<T> {
        match self {
            Self::Abort(_e) => None,
            Self::Continue { result, .. } | Self::Ok(result) => Some(result),
        }
    }
}

#[derive(Error, Debug, Clone)]
pub enum HirErr {
    #[error("Any error occured")]
    Any,
    #[error("Could not find lambda decleration with identifier {0}")]
    UnknownLambda(String),
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
