use tree_sitter::{Language, Parser};

///Loads the Vola language and returns the tree-sitter parser.
pub fn parser() -> Result<Parser, tree_sitter::LanguageError> {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_vola::language())?;
    Ok(parser)
}
