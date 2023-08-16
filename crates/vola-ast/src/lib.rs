//! # Vola-AST
//!
//! Vola's *A*bstract *S*yntax *T*ree.
//!
//!
//! The AST is designed to be easily transformed into SSA 3-address code. This allows us to transform it into
//! MLIR Code, or directly into SPIR-V, depending on the choosen back/middle end.

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
pub enum Node {
    EntryPoint,
}

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
