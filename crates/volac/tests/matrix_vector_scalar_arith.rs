#[test]
fn vector_scalar_operations() {
    let mut pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/vector_scalar.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn matrix_scalar_operations() {
    let mut pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/matrix_scalar.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn vector_matrix_operations() {
    let mut pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/vector_matrix.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn matrix_vector_operations() {
    let mut pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/matrix_vector.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn matrix_vector_broken_operations() {
    let mut pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline.execute_on_file(&"tests/vola_src/matrix_vector_broken.vola");
    assert!(target.is_err())
}

#[test]
fn matrix_matrix_operations() {
    let mut pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/matrix_matrix.vola")
        .unwrap();
    target.try_verify().unwrap();
}
