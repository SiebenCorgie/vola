//The example reads in a test file (from the test corpus)
//and outputs it reformatted.

fn main() {
    let ast = vola_tree_sitter_parser::parse_file("tests/ui/std.vola").unwrap();
    let formated = vola_fmt::Formater::format_ast(&ast);

    println!("{}", formated);
}
