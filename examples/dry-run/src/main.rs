use vola_ast::AstError;

fn main() -> Result<(), AstError> {
    let file = std::env::args().nth(1).unwrap_or({
        println!("No file found, using default.vola!");
        String::from("examples/dry-run/default.vola")
    });

    let mut parser = vola_ast::parser()?;
    let file_str = std::fs::read_to_string(&file).expect(&format!("Failed to read {}", file));

    let tree = parser
        .parse(&file_str, None)
        .expect("Failed to parse file in time");

    Ok(())
}
