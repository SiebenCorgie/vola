use std::path::Path;

use vola_ast::VolaAst;

///Small helper that parses some file into a AST, or panics if that doesn't work
pub fn parse_file(file: &dyn AsRef<Path>) -> VolaAst {
    let parser = vola_tree_sitter_parser::VolaTreeSitterParser;
    vola_ast::VolaAst::new_from_file(file, &parser).expect("Failed to parse file via tree-sitter")
}
