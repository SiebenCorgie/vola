use std::error::Error;

use ahash::AHashMap;
use tinyvec::TinyVec;

use crate::{CommonError, ErrorPrintBundle, Span};

///Helper utility that collects [CommonError](crate::CommonError)s, and reports them when asked.
pub struct ErrorReporter<E: Error + Default> {
    errors: TinyVec<[CommonError<E>; 10]>,
    ///Map of cached files we already _know_.
    cached_files: AHashMap<String, Vec<String>>,
}

impl<E: Error + Default> ErrorReporter<E> {
    pub fn new() -> Self {
        ErrorReporter {
            errors: TinyVec::default(),
            cached_files: AHashMap::default(),
        }
    }

    pub fn has_errors(&self) -> bool {
        self.errors.len() > 0
    }

    pub fn push_error(&mut self, error: CommonError<E>) {
        self.errors.push(error);
    }

    ///Loads the sub slice for the span. Returns an error string, if the file can not be read.
    pub fn load_span_line<'a>(&'a mut self, span: &Span) -> &'a [String] {
        if !self.cached_files.contains_key(span.file.as_str()) {
            //try to load the file, if not possible, ignore
            match std::fs::read_to_string(span.file.as_str()) {
                Ok(file) => {
                    let _ = self.cached_files.insert(
                        span.file.as_str().to_owned(),
                        file.lines().map(|line| line.to_owned()).collect(),
                    );
                }
                Err(e) => {
                    println!("failed to read file {} : {}", span.file.as_str(), e);
                    let _ = self.cached_files.insert(
                        span.file.as_str().to_owned(),
                        vec![format!("Could not read file: {}", span.file.as_str())],
                    );
                }
            }
        }

        if let Some(file_content) = self.cached_files.get(span.file.as_str()) {
            //try to get the lines range
            file_content
        } else {
            &[]
        }
    }

    ///Prints all errors to stdout
    pub fn report_all(&mut self) {
        println!("reporting {} errors", self.errors.len());
        for error in &self.errors {
            if let Some(lines) = self.cached_files.get(error.span.file.as_str()) {
                let bundle = ErrorPrintBundle {
                    error: &error,
                    src_lines: &lines[error.span.from.0..error.span.to.0],
                };
                println!("{}", bundle);
            } else {
                println!("could not get source file {}", error.span.file);
            }
        }
    }

    pub fn take_errors(self) -> TinyVec<[CommonError<E>; 10]> {
        self.errors
    }
}
