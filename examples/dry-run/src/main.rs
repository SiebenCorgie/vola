use vola_ast::{Ast, AstError};

fn main() -> Result<(), AstError> {
    let file = std::env::args().nth(1).unwrap_or({
        println!("No file found, using default.vola!");
        String::from("examples/dry-run/default.vola")
    });

    let ast = Ast::from_file(file)?;

    println!("{:#?}", ast);

    Ok(())
}
