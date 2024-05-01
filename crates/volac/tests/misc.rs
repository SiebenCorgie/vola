#[test]
fn use_external_function() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/custom_fn.vola")
        .unwrap();
    target.try_verify().unwrap();
}
