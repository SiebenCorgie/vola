/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::path::PathBuf;

use thiserror::Error;

///Errors that happen while working on the AST.
#[derive(Debug, Error, Clone, PartialEq)]
pub enum AstError {
    #[error("Failed to parse file {path:?}: {err}")]
    ParsingError { path: String, err: String },

    #[error("Not parsing from file. Therfore no sub-module can be used.")]
    NoRootFile,
    #[error("Module file {path:?} does not exist")]
    NoModuleFile { path: PathBuf },
    #[error("IO Error: {0}")]
    IoError(String),
}
