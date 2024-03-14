/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use ahash::AHashMap;
use backtrace::Backtrace;
use miette::Diagnostic;

use lazy_static::lazy_static;
use std::{
    error::Error,
    sync::{Arc, Mutex},
};

pub trait Reportable: Error + Diagnostic + Send + Sync + 'static {}

lazy_static! {
    static ref REPORTER: Mutex<ErrorReporter> = Mutex::new(ErrorReporter::new());
}

///Helper utility that collects [CommonError](crate::CommonError)s, and reports them when asked.
pub struct ErrorReporter {
    had_error: bool,
    ///Map of cached files we already _know_.
    cached_files: AHashMap<String, Arc<String>>,
}

impl ErrorReporter {
    pub fn new() -> Self {
        ErrorReporter {
            had_error: false,
            cached_files: AHashMap::default(),
        }
    }

    pub fn has_errors(&self) -> bool {
        self.had_error
    }

    pub fn load_src_file<'a>(&'a mut self, file: &str) -> Option<&'a str> {
        self.cached_files.get(file).map(|s| s.as_str())
    }

    ///Tries to ensure that "file" is loaded. Returns false if it couldn't.
    pub fn ensure_file(&mut self, file: &str) -> Option<Arc<String>> {
        if !self.cached_files.contains_key(file) {
            match std::fs::read_to_string(file) {
                Ok(file_string) => {
                    let payload = Arc::new(file_string);
                    let _ = self.cached_files.insert(file.to_owned(), payload.clone());
                    Some(payload)
                }
                Err(e) => {
                    println!("failed to read file {} : {}", file, e);
                    None
                }
            }
        } else {
            self.cached_files.get(file).cloned()
        }
    }

    pub fn report_on_file(&mut self, rep: impl Reportable, src_file: &str) {
        if let Some(srccode) = self.ensure_file(src_file) {
            let report = miette::ErrReport::new(rep).with_source_code(srccode);
            println!("{:?}", report);

            if std::env::var("VOLA_BACKTRACE").is_ok() {
                println!("Backtrace: {:#?}", Backtrace::new())
            }
        } else {
            self.report(rep);
        }
    }

    pub fn report(&mut self, rep: impl Reportable) {
        let report = miette::ErrReport::new(rep);
        println!("{:?}", report);
        if std::env::var("VOLA_BACKTRACE").is_ok() {
            println!("Backtrace: {:#?}", Backtrace::new())
        }
    }
}

///Sends a `err` reportable and reports the error, possibly based on some source-`file`.
pub fn report(err: impl Reportable, file: Option<&str>) {
    if let Some(f) = file {
        match REPORTER.lock() {
            Ok(mut lck) => lck.report_on_file(err, f),
            Err(e) => println!("Failed to report error: {e}"),
        }
    } else {
        match REPORTER.lock() {
            Ok(mut lck) => lck.report(err),
            Err(e) => println!("Failed to report error: {e}"),
        }
    }
}
