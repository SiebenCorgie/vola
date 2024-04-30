use vola_ast::{common::Ty, AstEntry, TopLevelNode};

#[test]
fn test_parse_fn() {
    let ast = vola_ast::parse_file("tests/testfn.vola").unwrap();

    assert!(ast.entries.len() == 1);
    if let TopLevelNode {
        span: _,
        ct_args: _,
        entry: AstEntry::AlgeFunc(f),
    } = &ast.entries[0]
    {
        assert!(f.args[0].ty == Ty::Vec { width: 3 });
        assert!(
            f.args[1].ty
                == Ty::Matrix {
                    width: 3,
                    height: 3
                }
        );

        assert!(f.return_type == Ty::Scalar)
    }
}
