use std::{
    error::Error,
    fmt::Display,
    path::PathBuf,
    thread::JoinHandle,
    time::{Duration, Instant},
};

use smallvec::{smallvec, SmallVec};
use vola_common::VolaError;
use volac::{backends::BoxedBackend, Pipeline, PipelineError, Target};
use yansi::Paint;

use crate::config::Config;

#[derive(Debug, Clone, PartialEq)]
pub enum ResultType {
    Success,
    Error(Vec<String>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Backend {
    Spirv,
    Wasm,
    None,
}

#[derive(Debug, PartialEq)]
pub enum TestState {
    Success,
    Error(String),
}

pub struct TestRun {
    backend: Backend,
    time: Duration,
    state: TestState,
    path: PathBuf,
}

pub struct TestResult {
    pub runs: SmallVec<[TestRun; 3]>,
}

impl TestResult {
    pub fn only_successes(&self) -> bool {
        let mut is_success = true;
        for r in &self.runs {
            if r.state != TestState::Success {
                is_success = false;
            }
        }

        is_success
    }

    pub fn partial_success(&self) -> bool {
        let mut any_success = false;
        for r in &self.runs {
            if r.state == TestState::Success {
                any_success = true
            }
        }

        any_success
    }
}

impl Display for TestRun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.state {
            TestState::Success => {
                write!(
                    f,
                    "{}: [{} @ {}ms] {}",
                    "Passed".bold().green(),
                    format!("{:?}", self.backend).bold(),
                    format!("{}", (self.time.as_secs_f32() * 1000.0).round()),
                    self.path.to_str().unwrap_or("Could not parse path!"),
                )
            }
            TestState::Error(error) => {
                write!(
                    f,
                    "{}: [{}] {}: {}",
                    "Failed".bold().red(),
                    format!("{:?}", self.backend).bold(),
                    self.path.to_str().unwrap_or("Colud not parse path!"),
                    error.bold()
                )
            }
        }
    }
}

fn pipeline_res_to_testrun(
    res: Result<Target, Vec<VolaError<PipelineError>>>,
    config: &Config,
    backend: Backend,
    time: Duration,
    path: PathBuf,
) -> TestRun {
    //check _if the right thing_ happened and push that result into the runs-result list
    match res {
        Ok(t) => {
            if config.expected_result == ResultType::Success {
                //If the result was actually expected to be successful, check if we have exec-test-data for that run.
                //if so, try it out.

                let test_run_result = match backend {
                    Backend::Wasm => {
                        let result = crate::wasm_executor::try_execute(t, config);
                        //blindly overwrite the test result
                        result
                    }
                    //for all others, don't run and keep it at
                    //success
                    _ => TestState::Success,
                };

                //is alright
                TestRun {
                    backend,
                    time,
                    state: test_run_result,
                    path: path.clone(),
                }
            } else {
                //expected error, but had none
                TestRun {
                    backend,
                    time,
                    state: TestState::Error(format!("Expected Error, but was success")),
                    path: path.clone(),
                }
            }
        }
        Err(e) => {
            match &config.expected_result {
                ResultType::Error(expected_error) => {
                    if expected_error.len() != e.len() {
                        return TestRun {
                            backend,
                            time,
                            state: TestState::Error(format!(
                                "Expected {} errors, but had {}",
                                expected_error.len(),
                                e.len()
                            )),
                            path: path.clone(),
                        };
                    }

                    for pipeline_error in e.iter().map(|e| e.error.to_string()) {
                        if !expected_error.contains(&pipeline_error) {
                            return TestRun {
                                backend,
                                time,
                                state: TestState::Error(format!(
                                    "Had error {}, but expected one of\n\t{:?}",
                                    pipeline_error, expected_error
                                )),
                                path: path.clone(),
                            };
                        }
                    }

                    //all errors match
                    TestRun {
                        backend,
                        time,
                        state: TestState::Success,
                        path: path.clone(),
                    }
                }
                ResultType::Success => {
                    //expected success, but was error
                    TestRun {
                        backend,
                        time,
                        state: TestState::Error(format!("Expected errors, but was success")),
                        path: path.clone(),
                    }
                }
            }
        }
    }
}

pub fn run_file(path: PathBuf) -> Result<JoinHandle<TestResult>, Box<dyn Error>> {
    let hdl = std::thread::Builder::new()
        .name(format!("TestRun: {:?}", path.as_path()))
        .spawn(move || {
            let config = match Config::parse_file(&path) {
                Ok(conf) => conf,
                Err(e) => {
                    //just emit that as error,
                    let dummyrun = TestRun {
                        backend: Backend::None,
                        time: Duration::ZERO,
                        state: TestState::Error(e.to_string()),
                        path,
                    };

                    return TestResult {
                        runs: smallvec![dummyrun],
                    };
                }
            };

            //Depending on the config, launch volac
            //and checkout the results each time. Then push the run
            let mut results = TestResult {
                runs: SmallVec::new(),
            };

            let mut backend_runs: SmallVec<[Backend; 2]> = SmallVec::new();
            if config.spirv {
                backend_runs.push(Backend::Spirv);
            }
            if config.wasm {
                backend_runs.push(Backend::Wasm);
            }

            for backend in backend_runs {
                //Run that thing and check if the output matches
                let start = Instant::now();
                let execfile = path.clone();
                let pipeline_result = std::panic::catch_unwind(|| {
                    let pipeline_backend: BoxedBackend = match backend {
                        Backend::None => panic!("No pipeline"),
                        Backend::Wasm => Box::new(volac::backends::Wasm::new(Target::buffer())),
                        Backend::Spirv => Box::new(volac::backends::Spirv::new(Target::buffer())),
                    };
                    let mut pipeline = Pipeline::new().with_backend(pipeline_backend);
                    /* NOTE: uncomment any of these if tests are failing.
                        pipeline.early_cnf = false;
                        pipeline.late_cne = false;
                        pipeline.late_cnf = false;
                    */
                    if config.validate {
                        pipeline = pipeline.with_validation();
                    }

                    pipeline.execute_on_file(&execfile)
                });
                let time = start.elapsed();

                let pipeline_run = match pipeline_result {
                    Err(_e) => {
                        //if this happened, then the run crashed.
                        TestRun {
                            backend,
                            time,
                            state: TestState::Error(format!("Paniced")),
                            path: path.clone(),
                        }
                    }
                    Ok(res) => pipeline_res_to_testrun(res, &config, backend, time, path.clone()),
                };

                results.runs.push(pipeline_run);
            }

            results
        })?;

    Ok(hdl)
}
