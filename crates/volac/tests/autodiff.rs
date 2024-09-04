///Simplest autodiff test that just applies AD once for a sphere definition
#[test]
fn ad_sphere() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_sphere.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn ad_sphere_vec() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_sphere_vec.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn ad_sphere_with_external() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_sphere_with_external.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn ad_sphere_with_external_vec() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_sphere_with_external_vec.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn ad_box() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_box.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn ad_box_vec() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_box.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn ad_nontrivial_activity() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_non_trivial_activity_trace.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn ad_with_gamma() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_with_gamma.vola")
        .unwrap();
    target.try_verify().unwrap();
}
