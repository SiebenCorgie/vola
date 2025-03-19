use vola_tree_sitter_openscad::{OpenScadTreeSitterParser, parse_file};

#[test]
fn car() {
    let mut parser = OpenScadTreeSitterParser;
    let ast = parse_file("tests/car.scad").expect("Expected no errors!");
    assert!(ast.entries.len() == 31);
}
