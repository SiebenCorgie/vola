//! Runs the parser on selected examples from the SCAD repo

use vola_tree_sitter_openscad::parse_file;

#[test]
fn car() {
    let ast = parse_file("tests/car.scad").expect("Expected no errors!");
    assert_eq!(ast.entries.len(), 3);
}

#[test]
fn cad() {
    let ast = parse_file("tests/CSG.scad").expect("Expected no errors!");
    assert_eq!(ast.entries.len(), 3);
}

#[test]
fn cad_modules() {
    let ast = parse_file("tests/CSG-modules.scad").expect("Expected no errors!");
    assert_eq!(ast.entries.len(), 13);
}
