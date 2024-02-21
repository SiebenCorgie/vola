use vola_opt::Optimizer;

pub fn main() {
    let ast = match vola_ast::parse_file("crates/vola-ast/examples/csg.vola") {
        Ok(ast) => ast,
        Err((partial_ast, _errors_reporter)) => {
            println!("\nGot {} entries in toplevel!", partial_ast.entries.len());
            return;
        }
    };

    let mut opt = Optimizer::new();
    if opt.add_ast(ast).is_err() {
        println!("Opt did not parse AST successfully :(");
    }
}
