use vola_ast::{dot::ast_to_svg, VolaAst};

pub fn main() {
    let ast = match vola_ast::parse_file("crates/vola-ast/examples/csg.vola") {
        Ok(ast) => ast,
        Err((partial_ast, mut reporter)) => {
            reporter.report_all();
            println!("\nGot {} entries in toplevel!", partial_ast.entries.len());
            return;
        }
    };

    ast_to_svg(&ast, "simple_csg");
}
