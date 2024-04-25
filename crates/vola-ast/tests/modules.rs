#[test]
fn module_simple_import() {
    let mut ast = vola_ast::parse_file("tests/src_module.vola").unwrap();
    assert!(ast.entries[0].entry.is_module_import());
    ast.resolve_modules(&"tests/src_module.vola").unwrap();
    assert!(ast.entries[0].entry.is_def_node());
    assert!(ast.entries[4].entry.is_exportfn());
}

#[test]
fn module_cascading_import() {
    let mut ast = vola_ast::parse_file("tests/sub_sub_module.vola").unwrap();
    assert!(ast.entries[0].entry.is_module_import());
    ast.resolve_modules(&"tests/sub_sub_module.vola").unwrap();
    assert!(ast.entries[0].entry.is_def_node());
    assert!(ast.entries[4].entry.is_exportfn());
    assert!(ast.entries[5].entry.is_def_node());
}

#[test]
fn module_cyclic_import() {
    let mut ast = vola_ast::parse_file("tests/cyclic.vola").unwrap();
    ast.resolve_modules(&"tests/cyclic.vola").unwrap();
    assert!(ast.entries.len() == 2, "{} != 2", ast.entries.len());
    assert!(ast.entries[0].entry.is_def_node());
    assert!(ast.entries[1].entry.is_exportfn());
}

#[test]
fn toplevel_cyclic_import() {
    let mut ast = vola_ast::parse_file("tests/self_cyclic.vola").unwrap();
    ast.resolve_modules(&"tests/self_cyclic.vola").unwrap();
    assert!(ast.entries.len() == 1, "{} != 1", ast.entries.len());
    assert!(ast.entries[0].entry.is_exportfn());
}
