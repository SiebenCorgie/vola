//! Runs the parser on selected examples from the SCAD repo

use vola_tree_sitter_openscad::{OpenScadTreeSitterParser, parse_file};

#[test]
fn car() {
    let mut parser = OpenScadTreeSitterParser;
    let ast = parse_file("tests/car.scad").expect("Expected no errors!");
    assert!(ast.entries.len() == 31);
}

#[test]
fn cad() {
    let mut parser = OpenScadTreeSitterParser;
    let ast = parse_file("tests/CSG.scad").expect("Expected no errors!");
    assert!(ast.entries.len() == 31);
}

#[test]
fn cad_modules() {
    let mut parser = OpenScadTreeSitterParser;
    let ast = parse_file("tests/CSG-modules.scad").expect("Expected no errors!");
    assert!(ast.entries.len() == 31);
}
