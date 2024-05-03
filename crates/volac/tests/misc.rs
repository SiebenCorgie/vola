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
        text: "Failed to dispatch all fields with 1 errors".to_owned(),
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
