use vola_ast::{
    common::{Field, Identifier, PrimBlock},
    Ast,
};
use vola_common::Span;

#[test]
pub fn complex_field() {
    /*
        let ast = Ast::from_string(
            "
    field my_param(){
        def mybox = box([1, 2, 3]);
        def my_sphere = sphere(1.0);

        union<translate<mybox>([1.0, 0.0, 0.0]), round<my_sphere>(0.3)>()
    }",
        )
        .unwrap();

        let expected = Field {
            src: Span::empty(),
            ident: Identifier{String::from("my_param")},
            args: Vec::new(),
            block: PrimBlock {
                stmt_list: Vec::new(),
                op_tree: vola_ast::comb::OpNode::PrimIdent(Identifier(String::new())),
            },
        };

        assert!(ast.fields.len() == 1);
        let parsed_field = ast.fields.get(&expected.ident).unwrap();
        //TODO use sexpr to test `parsed_field`
        */
}
