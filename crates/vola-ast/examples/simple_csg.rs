use vola_ast::{dot::ast_to_svg, VolaAst};

pub fn main() {
    let string = "
//Export interface of the SDF
export myField(p: vec3, translation: vec3){
    let some_formula = translation.x * 2.0;
    csg all_field = SmoothUnion(1.0){
        subField(translation)
    }{
        Rotate([30.0, 30.0, some_formula]){
            Round(1.0){
                Box([1.0, 0.5, 3.0])
            }
        }
    };

    (all_field.SDF3D(p), all_field.Color(p))
}
    "
    .to_owned();

    let ast = match vola_ast::parse_string(string) {
        Ok(ast) => ast,
        Err((partial_ast, mut reporter)) => {
            reporter.report_all();
            println!("\nGot {} entries in toplevel!", partial_ast.entries.len());
            return;
        }
    };

    ast_to_svg(&ast, "simple_csg");
}
