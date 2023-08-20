

use tree_sitter::{Parser};

use crate::{AstError};

pub trait FromSitter {
    ///Parses `Self` from some node
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized;
}

///Loads the Vola language and returns the tree-sitter parser.
pub fn parser() -> Result<Parser, tree_sitter::LanguageError> {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_vola::language())?;
    Ok(parser)
}
