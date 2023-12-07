//! AST diagnosis helper. This is mostly Span of nodes, as well as AST errors and their reporting.

use std::{num::ParseIntError, sync::Mutex};

use lazy_static::lazy_static;
use thiserror::Error;

use tinyvec::TinyVec;
use vola_common::{CommonError, ErrorReporter, Span};

lazy_static! {
    static ref REPORTER: Mutex<ErrorReporter<AstErrorTy>> = Mutex::new(ErrorReporter::new());
}

///Sets the default file for reported errors.
pub fn set_reporter_file_path(file_path: &str) {
    REPORTER.lock().unwrap().set_default_file(file_path)
}

///Reports an error to the static reporter. All reported errors can be printed via
/// [print_errors].
pub fn report_error(error: CommonError<AstErrorTy>) {
    REPORTER.lock().unwrap().push_error(error)
}

pub fn print_errors() -> TinyVec<[CommonError<AstErrorTy>; 10]> {
    REPORTER.lock().unwrap().report_all()
}

#[derive(Debug)]
pub struct AstError(pub CommonError<AstErrorTy>);

impl AstError {
    pub fn at_node(node: &tree_sitter::Node, source: AstErrorTy) -> Self {
        let span = Span::from(node);
        AstError(CommonError::new(span, source))
    }

    ///Helper that creates an error if `node` is not of `expect_kind`.
    pub fn kind_expected(node: &tree_sitter::Node, expect_kind: &str) -> Result<(), Self> {
        if node.kind() == expect_kind {
            return Ok(());
        }

        Err(Self::at_node(
            node,
            AstErrorTy::UnexpectedNodeKind {
                actual: node.kind().to_owned(),
                expected: expect_kind.to_owned(),
            },
        ))
    }

    ///Helper that checks if the child iterator has ended. Reports an error if not.
    pub fn expect_end<'a>(
        iter: &mut dyn Iterator<Item = tree_sitter::Node<'a>>,
    ) -> Result<(), Self> {
        if let Some(node) = iter.next() {
            Err(AstError::at_node(&node, AstErrorTy::ExpectEndOfStatement))
        } else {
            Ok(())
        }
    }

    pub fn uncaught_error(node: &tree_sitter::Node) -> Result<(), Self> {
        if node.has_error() {
            Err(AstError::at_node(node, AstErrorTy::UncaughtError))
        } else {
            Ok(())
        }
    }
}

impl From<AstErrorTy> for AstError {
    fn from(value: AstErrorTy) -> Self {
        AstError(CommonError {
            span: Span::empty(),
            source: value,
            backtrace: None,
        })
    }
}

#[derive(Debug, Error)]
pub enum AstErrorTy {
    #[error("Failed to load vola parser: {0}")]
    LanguageError(#[from] tree_sitter::LanguageError),
    #[error("{0}")]
    IoError(#[from] std::io::Error),
    #[error("Failed to parse from file")]
    ParseError,
    #[error("Failed to parse utf8 string in source code: {0}")]
    Utf8Err(#[from] core::str::Utf8Error),
    #[error("{ty} with name {ident} already existed")]
    IdentifierAlreadyExists { ty: String, ident: String },
    #[error("Could not parse digit: {0}")]
    ParseDigitError(#[from] ParseIntError),
    #[error("Block did not end with a primitive statement")]
    BlockEndNoPrim,
    #[error("Scoped algebra expression ended with a none algebraic expression")]
    ScopedEndNoAlge,
    #[error("Unexpected token {token} while parsing {unit}")]
    UnexpectedToken { token: String, unit: String },
    #[error("Unexpected node {actual} in AST, expected {expected}")]
    UnexpectedNodeKind { actual: String, expected: String },
    #[error("Expected end of statement")]
    ExpectEndOfStatement,
    #[error("Expected end of block, because return operation was set before.")]
    ExpectedEndAfterRet,
    #[error("Expected {0}")]
    Expected(String),
    #[error("Invalid node in tree, but error was not caught correctly")]
    UncaughtError,
}

impl Default for AstErrorTy {
    fn default() -> Self {
        AstErrorTy::UncaughtError
    }
}
