==================
README def block
==================


#[derive(LocalLipshitz)]
entity Sphere(radius: vec3);

concept SDF2D: vec2 -> s;
concept SDF3D: vec3 -> s;
concept UGF: vec3 -> s;
concept LocalLipshitz: vec3 -> s;
concept Color: -> vec3;

---
(source_file
      (ct_attrib
        (fn_call
          (identifier)
          (alge_expr
            (identifier))))
      (def_entity
        (identifier)
        (typed_arg
          (identifier)
          (alge_type
            (t_vec
              (digit)))))
      (def_concept
        (identifier)
        (alge_type
          (t_vec
            (digit)))
        (alge_type
          (t_scalar)))
      (def_concept
        (identifier)
        (alge_type
          (t_vec
            (digit)))
        (alge_type
          (t_scalar)))
      (def_concept
        (identifier)
        (alge_type
          (t_vec
            (digit)))
        (alge_type
          (t_scalar)))
      (def_concept
        (identifier)
        (alge_type
          (t_vec
            (digit)))
        (alge_type
          (t_scalar)))
      (def_concept
        (identifier)
        (alge_type
          (t_vec
            (digit)))))

==================
README Sphere
==================
//Declares that we need the parameter `@` to define SDF3D of Sphere
impl Sphere for SDF3D(at){
	length(at) - radius
}

impl Sphere for UGF(at){
     length(at)
}

---
(source_file
      (comment)
      (impl_block
        (identifier)
        (identifier)
        (identifier)
        (block
          (alge_expr
            (binary_expr
              (alge_expr
                (fn_call
                  (identifier)
                  (alge_expr
                    (identifier))))
              (alge_expr
                (identifier))))))
      (impl_block
        (identifier)
        (identifier)
        (identifier)
        (block
          (alge_expr
            (fn_call
              (identifier)
              (alge_expr
                (identifier)))))))

==================
README op Translate
==================

#[define(no_identity)]
operation Translate(trans: vec3);

//declares that `translate` takes a sub-expression and the
//field
impl Translate<sub> for SDF3D(at){
	at = at - trans;
	eval sub(at)
}

---

(source_file
      (ct_attrib
        (fn_call
          (identifier)
          (alge_expr
            (identifier))))
      (def_operation
        (identifier)
        (typed_arg
          (identifier)
          (alge_type
            (t_vec
              (digit)))))
      (comment)
      (comment)
      (impl_block
        (identifier)
        (identifier)
        (identifier)
        (identifier)
        (block
          (assign_stmt
            (identifier)
            (alge_expr
              (binary_expr
                (alge_expr
                  (identifier))
                (alge_expr
                  (identifier)))))
          (alge_expr
            (eval_expr
              (identifier)
              (alge_expr
                (identifier)))))))

==================
README Op Union
==================

operation Union();
impl Union<left, right> for SDF3D(at) {
	let left = eval l(at);
	let right = eval r(at);
	let new_result = min(left, right);
	new_result
}

---
(source_file
      (def_operation
        (identifier))
      (impl_block
        (identifier)
        (identifier)
        (identifier)
        (identifier)
        (identifier)
        (block
          (let_stmt
            (identifier)
            (alge_expr
              (eval_expr
                (identifier)
                (alge_expr
                  (identifier)))))
          (let_stmt
            (identifier)
            (alge_expr
              (eval_expr
                (identifier)
                (alge_expr
                  (identifier)))))
          (let_stmt
            (identifier)
            (alge_expr
              (fn_call
                (identifier)
                (alge_expr
                  (identifier))
                (alge_expr
                  (identifier)))))
          (alge_expr
            (identifier)))))

==================
README Op SmoothUnion
==================

operation SmoothUnion(suradius: s);
impl SmoothUnion<left, right> for SDF3D(at){
	let l = eval left(at);
	let r = eval right(at);
	let h = clamp( 0.5 + 0.5 * (r-l) / suradius, 0.0, 1.0 );
	let dist = mix(l, r, h) * - suradius * h * (1.0 - h);
	dist
}

---
(source_file
      (def_operation
        (identifier)
        (typed_arg
          (identifier)
          (alge_type
            (t_scalar))))
      (impl_block
        (identifier)
        (identifier)
        (identifier)
        (identifier)
        (identifier)
        (block
          (let_stmt
            (identifier)
            (alge_expr
              (eval_expr
                (identifier)
                (alge_expr
                  (identifier)))))
          (let_stmt
            (identifier)
            (alge_expr
              (eval_expr
                (identifier)
                (alge_expr
                  (identifier)))))
          (let_stmt
            (identifier)
            (alge_expr
              (fn_call
                (identifier)
                (alge_expr
                  (binary_expr
                    (alge_expr
                      (float_literal
                        (digit)
                        (digit)))
                    (alge_expr
                      (binary_expr
                        (alge_expr
                          (binary_expr
                            (alge_expr
                              (float_literal
                                (digit)
                                (digit)))
                            (alge_expr
                              (alge_expr
                                (binary_expr
                                  (alge_expr
                                    (identifier))
                                  (alge_expr
                                    (identifier)))))))
                        (alge_expr
                          (identifier))))))
                (alge_expr
                  (float_literal
                    (digit)
                    (digit)))
                (alge_expr
                  (float_literal
                    (digit)
                    (digit))))))
          (let_stmt
            (identifier)
            (alge_expr
              (binary_expr
                (alge_expr
                  (binary_expr
                    (alge_expr
                      (binary_expr
                        (alge_expr
                          (fn_call
                            (identifier)
                            (alge_expr
                              (identifier))
                            (alge_expr
                              (identifier))
                            (alge_expr
                              (identifier))))
                        (alge_expr
                          (unary_expr
                            (alge_expr
                              (identifier))))))
                    (alge_expr
                      (identifier))))
                (alge_expr
                  (alge_expr
                    (binary_expr
                      (alge_expr
                        (float_literal
                          (digit)
                          (digit)))
                      (alge_expr
                        (identifier))))))))
          (alge_expr
            (identifier)))))

==================
README Op ColorOW
==================

operation ColorOw(owcol: vec3);
impl ColorOw<sub> for Color{
	eval sub();
	owcol
}

---
(source_file
      (def_operation
        (identifier)
        (typed_arg
          (identifier)
          (alge_type
            (t_vec
              (digit)))))
      (impl_block
        (identifier)
        (identifier)
        (identifier)
        (block
          (dead_eval_stmt
            (eval_expr
              (identifier)))
          (alge_expr
            (identifier)))))

==================
README subField define
==================

define subField(trans: vec3){
    csg sphere = Sphere(3.0);

    Union(){
        Translate(trans){
            Sphere(2.0)
        }
    }{
        Box([1.0, 0.0, 3.0])
    }
}

---
(source_file
      (field_decl
        (identifier)
        (typed_arg
          (identifier)
          (alge_type
            (t_vec
              (digit))))
        (csg_binding
          (identifier)
          (fn_call
            (identifier)
            (alge_expr
              (float_literal
                (digit)
                (digit)))))
        (csg_binary
          (identifier)
          (csg_unary
            (identifier)
            (alge_expr
              (identifier))
            (fn_call
              (identifier)
              (alge_expr
                (float_literal
                  (digit)
                  (digit)))))
          (fn_call
            (identifier)
            (alge_expr
              (list
                (alge_expr
                  (float_literal
                    (digit)
                    (digit)))
                (alge_expr
                  (float_literal
                    (digit)
                    (digit)))
                (alge_expr
                  (float_literal
                    (digit)
                    (digit)))))))))

==================
README export myField
==================

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

---
(source_file
      (comment)
      (field_export
        (identifier)
        (typed_arg
          (identifier)
          (alge_type
            (t_vec
              (digit))))
        (typed_arg
          (identifier)
          (alge_type
            (t_vec
              (digit))))
        (let_stmt
          (identifier)
          (alge_expr
            (binary_expr
              (alge_expr
                (identifier))
              (ERROR)
              (alge_expr
                (float_literal
                  (digit)
                  (digit))))))
        (csg_binding
          (identifier)
          (csg_binary
            (identifier)
            (alge_expr
              (float_literal
                (digit)
                (digit)))
            (fn_call
              (identifier)
              (alge_expr
                (identifier)))
            (csg_unary
              (identifier)
              (alge_expr
                (list
                  (alge_expr
                    (float_literal
                      (digit)
                      (digit)))
                  (alge_expr
                    (float_literal
                      (digit)
                      (digit)))
                  (alge_expr
                    (identifier))))
              (csg_unary
                (identifier)
                (alge_expr
                  (float_literal
                    (digit)
                    (digit)))
                (fn_call
                  (identifier)
                  (alge_expr
                    (list
                      (alge_expr
                        (float_literal
                          (digit)
                          (digit)))
                      (alge_expr
                        (float_literal
                          (digit)
                          (digit)))
                      (alge_expr
                        (float_literal
                          (digit)
                          (digit))))))))))
        (access_decl
          (identifier)
          (fn_call
            (identifier)
            (alge_expr
              (identifier))))
        (access_decl
          (identifier)
          (fn_call
            (identifier)
            (alge_expr
              (identifier))))))
