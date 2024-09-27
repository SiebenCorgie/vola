/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::error::Error;

use rvsdg::util::graph_type_transform::GraphTypeTransformerError;
use vola_common::{thiserror::Error, Reportable};
use vola_opt::common::Ty;

impl Reportable for WasmError {}

#[derive(Debug, Error)]
pub enum WasmError {
    #[error(transparent)]
    Any(Box<dyn Error + Sync + Send + 'static>),
    #[error(transparent)]
    WalrusError(walrus::ErrorKind),
    #[error("Failed to intern optimizer state")]
    InterningFailed,
    #[error(transparent)]
    GraphTransError(#[from] GraphTypeTransformerError),
    #[error("Encountered unsupported optimizer node {0}")]
    UnsupportedNode(String),
    #[error("Encountered composite immediate value. Was \"ImmScalarize\" applied?")]
    UnexpectedComposite,
    #[error("Unexpected type {0:?}")]
    UnexpectedType(Ty),
}
