mod simple_parser;
use simple_parser::parse_file;

#[test]
fn module_simple_import() {
    let ast = parse_file(&"tests/src_module.vola");
    assert!(ast.entries[0].entry.is_def_node());
    assert!(ast.entries[4].entry.is_export_fn());
}

#[test]
fn module_cascading_import() {
    let ast = parse_file(&"tests/sub_sub_module.vola");
    assert!(ast.entries[0].entry.is_def_node());
    assert!(ast.entries[4].entry.is_export_fn());
    assert!(ast.entries[5].entry.is_def_node());
}

#[test]
fn module_cyclic_import() {
    let ast = parse_file(&"tests/cyclic.vola");
    assert!(ast.entries.len() == 2, "{} != 2", ast.entries.len());
    assert!(ast.entries[0].entry.is_def_node());
    assert!(ast.entries[1].entry.is_export_fn());
}

#[test]
fn toplevel_cyclic_import() {
    let ast = parse_file(&"tests/self_cyclic.vola");
    assert!(ast.entries.len() == 1, "{} != 1", ast.entries.len());
    assert!(ast.entries[0].entry.is_export_fn());
}
