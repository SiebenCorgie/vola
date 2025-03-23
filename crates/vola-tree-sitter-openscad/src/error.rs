use std::str::Utf8Error;

#[derive(Debug, thiserror::Error, Clone, PartialEq, Eq)]
pub enum ParserError {
    #[error("Unknown Error: {0}")]
    UnknownError(String),
    #[error("Could not run tree-sitter")]
    TreeSitterFailed,
    #[error("File Error occured: {0}")]
    FSError(String),
    #[error("UTF8 parser error: {0}")]
    Utf8ParseError(#[from] Utf8Error),
    #[error("Malformed node: {0}")]
    MalformedNode(String),
    #[error("Unexpected AST element: {0}")]
    Unexpected(String),
    #[error("Use of unsupported OpenScad feature: {0}")]
    UnsupportedScadFeature(String),
    #[error(transparent)]
    ParseIntError(#[from] std::num::ParseIntError),
    #[error(transparent)]
    ParseFloatError(#[from] std::num::ParseFloatError),
    #[error("Is not an expression")]
    NoExpr,
    #[error("Expression will be ignored")]
    Ignored,
}
