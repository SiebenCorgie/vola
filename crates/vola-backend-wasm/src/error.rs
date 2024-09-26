/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::error::Error;

use vola_common::{thiserror::Error, Reportable};

impl Reportable for WasmError {}

#[derive(Debug, Error)]
pub enum WasmError {
    #[error(transparent)]
    Any(Box<dyn Error + Sync + Send + 'static>),
    #[error(transparent)]
    WalrusError(walrus::ErrorKind),
}
