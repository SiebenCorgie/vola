use graphviz_rust::{
    cmd::{CommandArg, Format},
    exec,
    printer::{DotPrinter, PrinterContext},
};
use vola_ast::{Ast, AstError};
use vola_hir::Module;

fn main() -> Result<(), AstError> {
    let file = std::env::args().nth(1).unwrap_or({
        println!("No file found, using default.vola!");
        String::from("examples/dry-run/default.vola")
    });

    let ast = Ast::from_file(file)?;
    let hir = Module::from_ast(ast);

    let graph = hir.dot_graph();
    let string = graph.print(&mut PrinterContext::default());
    println!("{}", string);

    exec(
        graph,
        &mut PrinterContext::default(),
        vec![
            Format::Svg.into(),
            CommandArg::Output("dryrun-hir.svg".to_string()),
        ],
    )
    .unwrap();

    Ok(())
}
