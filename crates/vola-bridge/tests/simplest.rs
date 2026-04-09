#[test]
fn teddy() {
    let mut module = vola_bridge::load_code(
        "
       export fn myfn(a: real) -> real{
           a + 2.0
       }
    ",
        [],
    )
    .unwrap();

    let result = module
        .call("myfn")
        .unwrap()
        .with_arg("a", 2.0)
        .unwrap()
        .call_once()
        .unwrap();

    assert!(result.len() == 1);
    assert_eq!(result[0].unwrap_f32(), 4.0f32);
}

///Loads the standard library and compiles it, along with a custom function.
#[test]
fn with_std() {
    let code = "
module stdlib::prelude;
export fn mytest(at: vec3) -> real{
    csg c = Sphere(1.0);
    eval c.Sdf3d(at)
}
    ";
    let mut module = vola_bridge::Module::from_code(
        code.as_bytes(),
        "./",
        [(
            "stdlib".to_string(),
            "../../vola-stdlib/vola-sdf-stdlib/".into(),
        )]
        .into_iter(),
    )
    .unwrap();

    //NOTE: the sd at this point should be 1.0, because the sphere is 1.0 in radius around ZERO.
    let result = module
        .call("mytest")
        .unwrap()
        .with_arg("at", [0.0, 2.0, 0.0])
        .unwrap()
        .call_once()
        .unwrap();

    assert!(result.len() == 1);
    assert_eq!(result[0].unwrap_f32(), 1.0f32);
}
