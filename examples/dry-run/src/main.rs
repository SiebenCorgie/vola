use graphviz_rust::{
    cmd::{CommandArg, Format},
    exec,
    printer::PrinterContext,
};
use vola_ast::{diag::AstError, Ast};

fn main() -> Result<(), AstError> {
    let file = std::env::args().nth(1).unwrap_or({
        println!("No file found, using default.vola!");
        String::from("examples/dry-run/default.vola")
    });

    let ast = Ast::from_file(file)?;
    let ast_graph = ast.dot_graph();

    exec(
        ast_graph,
        &mut PrinterContext::default(),
        vec![
            Format::Svg.into(),
            CommandArg::Output("dryrun-ast.svg".to_string()),
        ],
    )
    .unwrap();

    Ok(())
}
