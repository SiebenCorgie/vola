///Simplest autodiff test that just applies AD once for a sphere definition
#[test]
fn ad_sphere() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_sphere.vola")
        .unwrap();
}

#[test]
fn ad_sphere_vec() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_sphere_vec.vola")
        .unwrap();
}

#[test]
fn ad_sphere_with_external() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_sphere_with_external.vola")
        .unwrap();
}

#[test]
fn ad_sphere_with_external_vec() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_sphere_with_external_vec.vola")
        .unwrap();
}

#[test]
fn ad_box() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_box.vola")
        .unwrap();
}

#[test]
fn ad_box_vec() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_box.vola")
        .unwrap();
}

#[test]
fn ad_nontrivial_activity() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_non_trivial_activity_trace.vola")
        .unwrap();
}

#[test]
fn ad_with_gamma() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_with_gamma.vola")
        .unwrap();
}

#[test]
fn ad_with_theta() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_with_theta.vola")
        .unwrap();
}

#[test]
fn ad_with_theta_fractal() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/autodiff_with_theta_fract.vola")
        .unwrap();
}

#[test]
fn ad_higher_order_derivative() {
    let mut pipeline = volac::Pipeline::new_in_memory().with_validation();
    let _target = pipeline
        .execute_on_file(&"tests/vola_src/second_order_diff.vola")
        .unwrap();
}
