/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Handles loading of the wasm runtime

use crate::error::WasmError;

const RUNTIME_CODE: &'static [u8] = include_bytes!(env!("RUNTIME_DIR"));

pub fn load_runtime_module() -> Result<walrus::Module, WasmError> {
    let module = walrus::Module::from_buffer(RUNTIME_CODE).map_err(|e| WasmError::Any(e.into()))?;

    Ok(module)
}
