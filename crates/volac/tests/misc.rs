use volac::PipelineError;

#[test]
fn use_external_function() {
    let pipeline = volac::Pipeline::new_in_memory();
    let target = pipeline
        .execute_on_file(&"tests/vola_src/custom_fn.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn eval_in_entity() {
    let pipeline = volac::Pipeline::new_in_memory();

    let expected_error = PipelineError::OptError(vola_opt::OptError::Any {
        text: "Could not find implementation of \"SDF3D\" for \"InnerEntity\"".to_owned(),
    });
    match pipeline.execute_on_file(&"tests/vola_src/undefined_impl.vola") {
        Err(e) => {
            //NOTE: kinda dirty, but working :D
            if e.to_string() != expected_error.to_string() {
                panic!("Wrong kind of error, expected ErrorsOccurred(1), got {e:?}")
            }
        }
        Ok(_) => panic!("Expected 1 error"),
    }
}
#[test]
fn recursive_function_simple() {
    let pipeline = volac::Pipeline::new_in_memory();

    let expected_error = PipelineError::OptError(vola_opt::OptError::ErrorsOccurred(2));
    match pipeline.execute_on_file(&"tests/vola_src/simple_recursive_call.vola") {
        Err(e) => {
            //NOTE: kinda dirty, but working :D
            if e.to_string() != expected_error.to_string() {
                panic!("Wrong kind of error, expected ErrorsOccurred(2), got {e:?}")
            }
        }
        Ok(_) => panic!("Expected 1 error"),
    }
}
#[test]
fn recursive_function_cascading() {
    let pipeline = volac::Pipeline::new_in_memory();

    let expected_error = PipelineError::OptError(vola_opt::OptError::ErrorsOccurred(3));
    match pipeline.execute_on_file(&"tests/vola_src/cascaded_recursive_call.vola") {
        Err(e) => {
            //NOTE: kinda dirty, but working :D
            if e.to_string() != expected_error.to_string() {
                panic!("Wrong kind of error, expected ErrorsOccurred(3), got {e:?}")
            }
        }
        Ok(_) => panic!("Expected 1 error"),
    }
}
#[test]
fn unknown_subtree_reference() {
    let pipeline = volac::Pipeline::new_in_memory();

    let expected_error = PipelineError::OptError(vola_opt::OptError::ErrorsOccurred(1));
    match pipeline.execute_on_file(&"tests/vola_src/unknown_subtree_reference.vola") {
        Err(e) => {
            //NOTE: kinda dirty, but working :D
            if e.to_string() != expected_error.to_string() {
                panic!("Wrong kind of error, expected ErrorsOccurred(1), got {e:?}")
            }
        }
        Ok(_) => panic!("Expected 1 error"),
    }
}

#[test]
fn simple_gamma() {
    let pipeline = volac::Pipeline::new_in_memory();

    let target = pipeline
        .execute_on_file(&"tests/vola_src/ifthenelse.vola")
        .unwrap();
    target.try_verify().unwrap()
}
#[test]
fn gamma_local_variable() {
    let pipeline = volac::Pipeline::new_in_memory();

    let target = pipeline
        .execute_on_file(&"tests/vola_src/gamma_local_variable.vola")
        .unwrap();
    target.try_verify().unwrap()
}

#[test]
fn simple_theta() {
    let pipeline = volac::Pipeline::new_in_memory();

    let target = pipeline
        .execute_on_file(&"tests/vola_src/theta_assignment.vola")
        .unwrap();
    target.try_verify().unwrap();
}
#[test]
fn theta_nested() {
    let pipeline = volac::Pipeline::new_in_memory();

    let target = pipeline
        .execute_on_file(&"tests/vola_src/theta_nested.vola")
        .unwrap();
    target.try_verify().unwrap();
}
#[test]
fn theta_nested_simple() {
    let pipeline = volac::Pipeline::new_in_memory();

    let target = pipeline
        .execute_on_file(&"tests/vola_src/theta_nested_simple.vola")
        .unwrap();
    target.try_verify().unwrap();
}
#[test]
fn theta_simple_local_variable() {
    let pipeline = volac::Pipeline::new_in_memory();

    let target = pipeline
        .execute_on_file(&"tests/vola_src/theta_local_variable.vola")
        .unwrap();
    target.try_verify().unwrap();
}

#[test]
fn in_loop_eval() {
    let pipeline = volac::Pipeline::new_in_memory();
    //NOTE: This tests both, in-gamma-eval, and in-loop-eval
    let target = pipeline
        .execute_on_file(&"tests/vola_src/in_loop_eval.vola")
        .unwrap();
    target.try_verify().unwrap();
}
#[test]
fn multi_concept_eval_in_loop_eval() {
    let pipeline = volac::Pipeline::new_in_memory();
    //NOTE: This tests both, in-gamma-eval, and in-loop-eval
    let target = pipeline
        .execute_on_file(&"tests/vola_src/subtree_multi_concept_eval.vola")
        .unwrap();
    target.try_verify().unwrap();
}
