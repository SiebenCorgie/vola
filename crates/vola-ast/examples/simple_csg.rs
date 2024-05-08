use vola_ast::dot::ast_to_svg;

pub fn main() {
    let parser = vola_tree_sitter_parser::VolaTreeSitterParser;
    let ast = match vola_ast::VolaAst::new_from_file(&"crates/vola-ast/examples/csg.vola", &parser)
    {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("{err:?}");
            return;
        }
    };

    ast_to_svg(&ast, "simple_csg.svg");
}
