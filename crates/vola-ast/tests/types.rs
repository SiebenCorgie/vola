mod simple_parser;
use simple_parser::parse_file;
use smallvec::smallvec;
use vola_ast::{
    common::Ty,
    csg::{CSGNodeDef, CSGNodeTy},
    AstEntry, TopLevelNode,
};

#[test]
fn construct_all_types() {
    //Constructs all types in the file.
    let ast = parse_file(&"tests/type_constructor.vola");

    assert!(ast.entries.len() == 8);

    for entry in ast.entries {
        if let TopLevelNode {
            span: _,
            ct_args: _,
            entry:
                AstEntry::CSGNodeDef(CSGNodeDef {
                    span: _,
                    ty: CSGNodeTy::Entity,
                    name,
                    args,
                }),
        } = entry
        {
            match name.0.as_str() {
                "SomeScalar" => {
                    assert!(args.len() == 1);
                    assert!(args[0].ty == Ty::Scalar);
                }
                "SomeNat" => {
                    assert!(args.len() == 1);
                    assert!(args[0].ty == Ty::Nat);
                }
                "SomeVector" => {
                    assert!(args.len() == 1);
                    assert!(args[0].ty == Ty::Vec { width: 3 });
                }
                "SomeBigVector" => {
                    assert!(args.len() == 1);
                    assert!(args[0].ty == Ty::Vec { width: 2342 });
                }
                "SomeMatrix" => {
                    assert!(args.len() == 1);
                    assert!(
                        args[0].ty
                            == Ty::Matrix {
                                width: 3,
                                height: 3
                            }
                    );
                }
                "SomeBigMatrix" => {
                    assert!(args.len() == 1);
                    assert!(
                        args[0].ty
                            == Ty::Matrix {
                                width: 123,
                                height: 456
                            }
                    );
                }
                "SomeTensor" => {
                    assert!(args.len() == 1);
                    assert!(
                        args[0].ty
                            == Ty::Tensor {
                                dim: smallvec![3, 3, 3]
                            }
                    );
                }
                "SomeBigTensor" => {
                    assert!(args.len() == 1);
                    assert!(
                        args[0].ty
                            == Ty::Tensor {
                                dim: smallvec![123, 123, 2987, 9879]
                            }
                    );
                }
                _ => panic!("Unexpected entity type!"),
            }
        } else {
            panic!("Unexpected node type!");
        }
    }
}
