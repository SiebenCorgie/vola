use tree_sitter::{Language, Parser};

use crate::error::VolaErr;

///Loads the Vola language and returns the tree-sitter parser.
pub fn parser() -> Result<Parser, VolaErr> {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_vola::language())?;
    Ok(parser)
}
