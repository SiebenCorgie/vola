use vola_ast::{
    common::{DataTy, Shape, Ty},
    AstEntry, TopLevelNode,
};

mod simple_parser;
use simple_parser::parse_file;

#[test]
fn test_parse_fn() {
    let ast = parse_file(&"tests/testfn.vola");

    assert!(ast.entries.len() == 1);
    if let TopLevelNode {
        span: _,
        ct_args: _,
        entry: AstEntry::Func(f),
    } = &ast.entries[0]
    {
        assert!(
            f.args[0].ty
                == Ty::Shaped {
                    ty: DataTy::Real,
                    shape: Shape::Vec { width: 3 }
                }
        );
        assert!(
            f.args[1].ty
                == Ty::Shaped {
                    ty: DataTy::Real,
                    shape: Shape::Matrix {
                        width: 3,
                        height: 3
                    }
                }
        );

        assert!(f.return_type == Ty::Simple(DataTy::Real));
        assert!(f.block.stmts.len() == 1);
    } else {
        panic!("Expected algefn");
    }
}
