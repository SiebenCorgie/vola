///Simplest autodiff test that just applies AD once for a sphere definition
#[test]
fn ad_sphere() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_sphere.vola")
        .unwrap();
    target.try_verify().unwrap();
}
