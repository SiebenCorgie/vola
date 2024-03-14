use std::time::Instant;

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

    let tyder_start = Instant::now();
    if let Err(e) = opt.type_derive() {
        println!("Type derive failed: {e}");
    }
    println!(
        "Type derive pass took {}ms / {}ns",
        tyder_start.elapsed().as_millis(),
        tyder_start.elapsed().as_nanos()
    );

    opt.dump_svg("after_type_resolution.svg");

    let mut disp_start = Instant::now();
    if let Err(e) = opt.dispatch_all_exports() {
        println!("Dispatching exports failed!");
    }
    println!(
        "Dispatch pass took {}ms / {}ns",
        disp_start.elapsed().as_millis(),
        disp_start.elapsed().as_nanos()
    );

    opt.dump_svg("after_dispatch.svg");
}
