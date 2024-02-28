use vola_opt::Optimizer;

pub fn main() {
    pretty_env_logger::init();

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

    opt.dump_svg("before_type_resolution.svg");

    if let Err(e) = opt.type_derive() {
        println!("Type derive failed: {e}");
    }

    opt.dump_svg("after_type_resolution.svg");
}
