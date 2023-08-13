use std::path::Path;

use thiserror::Error;

mod parser;
pub use parser::parser;

#[derive(Debug, Error)]
pub enum AstError {
    #[error("Failed to load vola parser: {0}")]
    LanguageError(#[from] tree_sitter::LanguageError),
    #[error("{0}")]
    IoError(#[from] std::io::Error),
    #[error("Failed to parse from file")]
    ParseError,
}

///All node types
pub enum Node {}

pub struct Ast {}

impl Ast {
    ///Creates an empty Ast with the given root node
    fn empty(root: Node) -> Self {
        Ast {}
    }

    ///Tries to parse `file` using `tree-sitter-vola` into an [Ast].
    fn from_file(file: impl AsRef<Path>) -> Result<Self, AstError> {
        let mut parser = parser()?;
        let text = std::fs::read_to_string(file)?;
        let syn_tree = parser.parse(text, None).ok_or(AstError::ParseError)?;

        //TODO transform to ast

        let ast = Ast {};

        Ok(ast)
    }
}
