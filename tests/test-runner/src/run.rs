use std::{
    error::Error,
    fmt::Display,
    io::{BufRead, Read},
    path::{Path, PathBuf},
    sync::mpsc::Receiver,
    thread::JoinHandle,
    time::{Duration, Instant},
};

use smallvec::{smallvec, SmallVec};
use volac::{backends::PipelineBackend, Pipeline, Target};
use yansi::Paint;

#[derive(Debug, Clone, PartialEq)]
pub enum ResultType {
    Success,
    Error(String),
}

///Test run config.
///
/// A test file can add the following header:
/// ```ignore
/// //BEGIN-CONFIG
/// //ERROR: Some output string to set an expected error
/// //STDOUT: Some exepected iout string
/// //NO-VALIDATE
/// //NO-WASM
/// //NO-SPIRV
/// //END-CONFIG
/// ```
/// Each line (except for begin and end) is optional. Otherwise the default config is used.
pub struct Config {
    spirv: bool,
    wasm: bool,
    validate: bool,

    //The expected correct behavior, including the error string, if an error is expected.
    expected_result: ResultType,
    //If Stdout output is expected, the content of it.
    //if set to None, stdout is not checked at all.
    expected_io_output: Option<String>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            spirv: true,
            wasm: true,
            validate: true,
            expected_result: ResultType::Success,
            expected_io_output: None,
        }
    }
}

impl Config {
    ///tries to parse the header of a file into this config
    pub fn parse_file(path: impl AsRef<Path>) -> Result<Self, Box<dyn Error>> {
        let mut config = Config::default();

        let file = std::fs::File::open(path.as_ref())?;
        let reader = std::io::BufReader::new(file);

        let mut lines = reader.lines();

        if let Some(first_line) = lines.next() {
            let line = first_line?;
            if !line.starts_with("//BEGIN-CONFIG") {
                //if no begin, just return
                return Ok(config);
            }
        } else {
            //if no first line
            return Ok(config);
        }

        //Now read the rest of the lines till we find an end, or a malformed line
        for line in lines {
            let line = line?;

            //the correct break
            if line.starts_with("//END-CONFIG") {
                break;
            }

            if line.starts_with("//NO-VALIDATE") {
                config.validate = false;
                continue;
            }

            if line.starts_with("//NO-WASM") {
                config.wasm = false;
                continue;
            }
            if line.starts_with("//NO-SPIRV") {
                config.spirv = false;
                continue;
            }

            if line.starts_with("//ERROR:") {
                //strip the error part and push the string
                if let Some(suffix) = line.strip_prefix("//ERROR:") {
                    config.expected_result = ResultType::Error(suffix.to_string());
                } else {
                    return Err(format!(
                        "{} \"ERROR\" string for {:?}",
                        "Malformed".bold(),
                        path.as_ref()
                    )
                    .into());
                }
                continue;
            }

            if line.starts_with("//STDOUT:") {
                //strip the error part and push the string
                if let Some(suffix) = line.strip_prefix("//STDOUT:") {
                    config.expected_io_output = Some(suffix.to_string());
                } else {
                    return Err(format!(
                        "{} \"STDOUT\" string for {:?}",
                        "Malformed".bold(),
                        path.as_ref()
                    )
                    .into());
                }
                continue;
            }

            return Err(format!(
                "Unexpected line\n{}\n{:?}\n\nDid you forget the END-CONFIG?",
                line.bold(),
                path.as_ref(),
            )
            .into());
        }

        Ok(config)
    }
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

            let mut runs: SmallVec<[Backend; 2]> = SmallVec::new();
            if config.spirv {
                runs.push(Backend::Spirv);
            }
            if config.wasm {
                runs.push(Backend::Wasm);
            }

            for backend in runs {
                //Run that thing and check if the output matches
                let start = Instant::now();
                let execfile = path.clone();
                let pipeline_result = std::panic::catch_unwind(|| {
                    let pipeline_backend: Box<dyn PipelineBackend + 'static> = match backend {
                        Backend::None => panic!("No pipeline"),
                        Backend::Wasm => Box::new(volac::backends::Wasm::new(Target::buffer())),
                        Backend::Spirv => Box::new(volac::backends::Spirv::new(Target::buffer())),
                    };
                    let mut pipeline = Pipeline::new_in_memory().with_backend(pipeline_backend);
                    pipeline.execute_on_file(&execfile)
                });
                let time = start.elapsed();

                let pipeline_run = match pipeline_result {
                    Err(e) => {
                        //if this happened, then the run crashed.
                        TestRun {
                            backend,
                            time,
                            state: TestState::Error(format!("Paniced")),
                            path: path.clone(),
                        }
                    }
                    Ok(res) => {
                        //Otherwise check _if the right thing_ happened and push that result into the runs-result list
                        match res {
                            Ok(_t) => {
                                if config.expected_result == ResultType::Success {
                                    //is alright
                                    TestRun {
                                        backend,
                                        time,
                                        state: TestState::Success,
                                        path: path.clone(),
                                    }
                                } else {
                                    //expected error, but had none
                                    TestRun {
                                        backend,
                                        time,
                                        state: TestState::Error(format!(
                                            "Expected Error, but was success"
                                        )),
                                        path: path.clone(),
                                    }
                                }
                            }
                            Err(e) => {
                                let pipeline_error = e.to_string();
                                match &config.expected_result {
                                    ResultType::Error(expected_error) => {
                                        //make sure the error matches
                                        if expected_error == &pipeline_error {
                                            TestRun {
                                                backend,
                                                time,
                                                state: TestState::Success,
                                                path: path.clone(),
                                            }
                                        } else {
                                            TestRun {
                                                backend,
                                                time,
                                                state: TestState::Error(format!(
                                                    "Expected error:\n    {}\ngot\n    {}",
                                                    expected_error, pipeline_error
                                                )),
                                                path: path.clone(),
                                            }
                                        }
                                    }
                                    ResultType::Success => {
                                        //expected success, but was error
                                        TestRun {
                                            backend,
                                            time,
                                            state: TestState::Error(pipeline_error),
                                            path: path.clone(),
                                        }
                                    }
                                }
                            }
                        }
                    }
                };

                results.runs.push(pipeline_run);
            }

            results
        })?;

    Ok(hdl)
}
