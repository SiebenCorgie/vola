/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::{error::Error, path::PathBuf};

use thiserror::Error;
use vola_common::VolaError;

///Errors that happen while working on the AST.
#[derive(Debug, Error, Clone, PartialEq)]
pub enum AstError {
    #[error("Failed to parse file: {0}")]
    ParsingError(String),

    #[error("Not parsing from file. Therfore no sub-module can be used.")]
    NoRootFile,
    #[error("Module file {path:?} does not exist")]
    NoModuleFile { path: PathBuf },
    #[error("IO Error: {0}")]
    IoError(String),
}

impl AstError {
    pub fn from_parser_error<E: Error>(error: VolaError<E>) -> VolaError<Self> {
        let internal = Self::ParsingError(format!("{}", error.error.to_string()));
        VolaError {
            error: internal,
            source_span: error.source_span,
            labels: error.labels,
        }
    }
}
