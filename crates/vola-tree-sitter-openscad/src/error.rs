use std::str::Utf8Error;

#[derive(Debug, thiserror::Error, Clone)]
pub enum ParserError {
    #[error("Unknown Error: {0}")]
    UnknownError(String),
    #[error("Could not run tree-sitter")]
    TreeSitterFailed,
    #[error("File Error occured: {0}")]
    FSError(String),
    #[error("UTF8 parser error: {0}")]
    Utf8ParseError(#[from] Utf8Error),
}
