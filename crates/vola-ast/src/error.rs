use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Unknown Error: {0}")]
    UnknownError(String),
    #[error("Could not run tree-sitter")]
    TreeSitterFailed,
    #[error("File Error occured: {0}")]
    FSError(std::io::Error),
    #[error("Unknown AstNode: {0}")]
    UnknownAstNode(String),
    #[error("Unexpected AstNode: {kind}, expected {expected}")]
    UnexpectedAstNode { kind: String, expected: String },
}
