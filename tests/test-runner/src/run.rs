use std::{
    error::Error,
    fmt::Display,
    path::PathBuf,
    thread::JoinHandle,
    time::{Duration, Instant},
};

use vola_lib::spirv::vola_backend_spirv::SpirvConfig;
use yansi::Paint;

use crate::config::Config;

#[derive(Debug, Clone, PartialEq)]
pub enum ResultType {
    Success,
    Error(Vec<String>),
}

pub enum PartialResult {
    Success(Duration),
    Error(Vec<String>),
    None,
}

impl PartialResult {
    pub fn is_success(&self) -> bool {
        matches!(self, PartialResult::Success(_))
    }

    pub fn error(error: impl Into<String>) -> Self {
        PartialResult::Error(vec![error.into()])
    }

    pub fn push_error(&mut self, error: impl Into<String>) {
        match self {
            PartialResult::Success(_) | PartialResult::None => {
                *self = PartialResult::Error(vec![error.into()]);
            }
            PartialResult::Error(errs) => errs.push(error.into()),
        }
    }
}

impl Display for PartialResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error(e) => {
                if e.len() > 1 {
                    write!(f, "{}({}): ", "Errors".red().bold(), e.len())?;
                    for e in e.iter() {
                        write!(f, "'{}' ", e)?;
                    }
                    Ok(())
                } else {
                    write!(f, "{}: {}", "Error".red().bold(), e[0])
                }
            }
            Self::Success(duration) => write!(f, "{}ms", (duration.as_secs_f32() * 1000.0).round()),
            Self::None => write!(f, "none"),
        }
    }
}

pub struct TestResult {
    parsing: PartialResult,
    optimizing: PartialResult,
    wasm_code: PartialResult,
    pub(crate) wasm_execute: PartialResult,
    spirv_code: PartialResult,
    path: PathBuf,
}

impl TestResult {
    pub fn empty(path: PathBuf) -> Self {
        TestResult {
            parsing: PartialResult::None,
            optimizing: PartialResult::None,
            wasm_code: PartialResult::None,
            wasm_execute: PartialResult::None,
            spirv_code: PartialResult::None,
            path,
        }
    }

    pub fn only_successes(&self) -> bool {
        self.parsing.is_success()
            && self.optimizing.is_success()
            && self.wasm_code.is_success()
            && self.wasm_execute.is_success()
            && self.spirv_code.is_success()
    }

    pub fn partial_success(&self) -> bool {
        self.parsing.is_success()
            || self.optimizing.is_success()
            || self.wasm_code.is_success()
            || self.wasm_execute.is_success()
            || self.spirv_code.is_success()
    }

    fn all_errors(&self) -> Vec<String> {
        let append_errors = |partial: &PartialResult, appenders: &mut Vec<String>| {
            if let PartialResult::Error(errors) = partial {
                for e in errors {
                    appenders.push(e.clone())
                }
            }
        };

        let mut appender = Vec::new();
        append_errors(&self.parsing, &mut appender);
        append_errors(&self.optimizing, &mut appender);
        append_errors(&self.wasm_code, &mut appender);
        append_errors(&self.wasm_execute, &mut appender);
        append_errors(&self.spirv_code, &mut appender);

        appender
    }

    pub fn matches_expectation(&self, expectation: &Config) -> Result<(), String> {
        match &expectation.expected_result {
            ResultType::Success => {
                //If we expect success, there should only be success... lol
                if self.only_successes() {
                    Ok(())
                } else {
                    let all_errors = self.all_errors();
                    Err(format!(
                        "Expected success, had {} errors: {all_errors:#?}",
                        all_errors.len()
                    ))
                }
            }
            ResultType::Error(errors) => {
                //If we expect errors, test each error against all errors we _possibly_ gathered.
                //Early out, if there are none

                if self.only_successes() {
                    return Err(format!("Expected Errors, but had none"));
                }

                let all_errors = self.all_errors();

                for expected_error in errors {
                    if !all_errors.contains(expected_error) {
                        return Err(format!(
                            "Expected error '{expected_error}' in\n{all_errors:#?}"
                        ));
                    }
                }

                //Found all expected errors, i.e. _correct_
                Ok(())
            }
        }
    }
}

impl Display for TestResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn format_ident(ident: &str, result: &PartialResult) -> String {
            format!("{}: {}", ident.bold(), result)
        }
        write!(
            f,
            "[{}, {}, {}, {}, {}]: {:?}",
            format_ident("AST", &self.parsing),
            format_ident("Opt", &self.optimizing),
            format_ident("SPIR-V", &self.spirv_code),
            format_ident("WASM", &self.wasm_code),
            format_ident("Execusion", &self.wasm_execute),
            self.path
        )
    }
}

pub fn run_file(path: PathBuf) -> Result<JoinHandle<(TestResult, Config)>, Box<dyn Error>> {
    let hdl = std::thread::Builder::new()
        .name(format!("TestRun: {:?}", path.as_path()))
        .spawn(move || {
            let mut result = TestResult::empty(path.clone());

            let config = match Config::parse_file(&path) {
                Ok(conf) => conf,
                Err(e) => {
                    result
                        .parsing
                        .push_error(format!("Could not parse test-run config: {}", e));
                    return (result, Config::default());
                }
            };

            let mut module = vola_lib::OptModule::new();
            let parse_start = Instant::now();
            let ast_lowering = match vola_lib::passes::LowerAst::from_file(&path) {
                Ok(ast) => ast,
                Err(e) => {
                    for e in e {
                        result
                            .parsing
                            .push_error(format!("{}", e.error.to_string()));
                    }

                    return (result, config);
                }
            };
            match module.apply_pass(ast_lowering) {
                Ok(_) => {}
                Err(e) => {
                    for e in e.errors {
                        result
                            .parsing
                            .push_error(format!("{}", e.error.to_string()));
                    }
                    return (result, config);
                }
            }

            let ast_timing = parse_start.elapsed();
            result.parsing = PartialResult::Success(ast_timing);

            //Run the standard pipeline
            let opt_start = Instant::now();
            if let Err(e) = module.standard_pipeline() {
                for e in e.errors {
                    result
                        .optimizing
                        .push_error(format!("{}", e.error.to_string()));
                }
                return (result, config);
            }
            let opt_timing = opt_start.elapsed();
            result.optimizing = PartialResult::Success(opt_timing);

            let copy = module.clone();

            if config.wasm {
                let start = Instant::now();
                let mut backend = vola_lib::wasm::WasmModule::new();
                if let Err(e) = backend.lower_opt(copy) {
                    result
                        .wasm_code
                        .push_error(format!("Lowering to WASM backend failed: {e:?}"));
                } else {
                    //successful lowerd opt into backend
                    match backend.build(config.validate) {
                        Ok(wasm_code) => {
                            result.wasm_code = PartialResult::Success(start.elapsed());
                            crate::wasm_executor::try_execute(wasm_code, &config, &mut result);
                        }
                        Err(e) => {
                            result
                                .wasm_code
                                .push_error(format!("WASM code generation failed: {e:?}"));
                        }
                    }
                }
            }

            if config.spirv {
                let start = Instant::now();
                let mut backend = vola_lib::spirv::SpirvModule::new(SpirvConfig::default());
                if let Err(e) = backend.lower_opt(module) {
                    result
                        .spirv_code
                        .push_error(format!("Lowering to SPIRV backend failed: {e:?}"));
                } else {
                    match backend.build(config.validate) {
                        Ok(_) => result.spirv_code = PartialResult::Success(start.elapsed()),
                        Err(e) => {
                            result
                                .wasm_code
                                .push_error(format!("SPIRV code generation failed: {e:?}"));
                        }
                    }
                }
            }

            (result, config)
        })?;

    Ok(hdl)
}
