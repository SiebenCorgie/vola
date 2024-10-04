use core::f32;
use std::{error::Error, io::BufRead, path::Path};

use smallvec::SmallVec;
use yansi::Paint;

use crate::run::ResultType;

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
/// //EXEC-FN:some_function_name
/// //EXEC-ARGS:1.0,2.0,3.0
/// //EXEC-RES:1.0
/// //EXEC-EPS:0.0001
/// //END-CONFIG
/// ```
/// Each line (except for begin and end) is optional. Otherwise the default config is used.
pub struct Config {
    pub spirv: bool,
    pub wasm: bool,
    pub validate: bool,

    //The expected correct behavior, including the error string, if an error is expected.
    pub expected_result: ResultType,
    //If Stdout output is expected, the content of it.
    //if set to None, stdout is not checked at all.
    pub expected_io_output: Option<String>,
    pub execution_fn: Option<String>,
    pub execution_args: Option<SmallVec<[f32; 3]>>,
    pub execution_res: Option<SmallVec<[f32; 3]>>,
    pub exec_eps: f32,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            spirv: true,
            wasm: true,
            validate: true,
            expected_result: ResultType::Success,
            expected_io_output: None,
            execution_fn: None,
            execution_res: None,
            execution_args: None,
            exec_eps: f32::EPSILON,
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

            if line.starts_with("//EXEC-FN:") {
                //strip the first part and push the string
                if let Some(suffix) = line.strip_prefix("//EXEC-FN:") {
                    config.execution_fn = Some(suffix.to_string());
                } else {
                    return Err(format!(
                        "{} \"EXEC-FN\" string for {:?}",
                        "Malformed".bold(),
                        path.as_ref()
                    )
                    .into());
                }
                continue;
            }

            if line.starts_with("//EXEC-EPS:") {
                //strip the first part and push the string
                if let Some(suffix) = line.strip_prefix("//EXEC-EPS:") {
                    if let Ok(f) = suffix.parse::<f32>() {
                        config.exec_eps = f;
                    } else {
                        return Err(format!(
                            "{} \"EXEC-EPS\" float: {:?}",
                            "Malformed".bold(),
                            suffix
                        )
                        .into());
                    }
                } else {
                    return Err(format!(
                        "{} \"EXEC-EPS\" string for {:?}",
                        "Malformed".bold(),
                        path.as_ref()
                    )
                    .into());
                }
                continue;
            }

            if line.starts_with("//EXEC-ARGS:") {
                //strip the first part and push the string
                if let Some(suffix) = line.strip_prefix("//EXEC-ARGS:") {
                    //now try to parse the args into a list of floats
                    //TODO: support ints as well?
                    let mut args = SmallVec::new();
                    for item in suffix.split(",") {
                        if item.is_empty() {
                            continue;
                        }
                        if let Ok(f) = item.parse::<f32>() {
                            args.push(f);
                        } else {
                            return Err(format!(
                                "{} in \"EXEC-ARGS\" : {}",
                                "Malformed float".bold(),
                                item
                            )
                            .into());
                        }
                    }

                    config.execution_args = Some(args);
                } else {
                    return Err(format!(
                        "{} \"EXEC-ARGS\" string for {:?}",
                        "Malformed".bold(),
                        path.as_ref()
                    )
                    .into());
                }
                continue;
            }
            if line.starts_with("//EXEC-RES:") {
                //strip the first part and push the string
                if let Some(suffix) = line.strip_prefix("//EXEC-RES:") {
                    //now try to parse the args into a list of floats
                    //TODO: support ints as well?
                    let mut res = SmallVec::new();
                    for item in suffix.split(",") {
                        if let Ok(f) = item.parse::<f32>() {
                            res.push(f);
                        } else {
                            return Err(format!(
                                "{} in \"EXEC-RES\" : {}",
                                "Malformed float".bold(),
                                item
                            )
                            .into());
                        }
                    }

                    config.execution_res = Some(res);
                } else {
                    return Err(format!(
                        "{} \"EXEC-RES\" string for {:?}",
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
