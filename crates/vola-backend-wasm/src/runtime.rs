/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Handles loading of the wasm runtime

use rvsdg::SmallColl;
use walrus::{FunctionId, FunctionKind, LocalId, ModuleFunctions, ModuleLocals, ValType};

use crate::error::WasmError;

const RUNTIME_CODE: &'static [u8] = include_bytes!(env!("RUNTIME_DIR"));

pub fn load_runtime_module() -> Result<walrus::Module, WasmError> {
    let module = walrus::Module::from_buffer(RUNTIME_CODE).map_err(|e| WasmError::Any(e.into()))?;

    Ok(module)
}

pub struct FnSymbol {
    pub id: FunctionId,
    pub arguments: SmallColl<(LocalId, ValType)>,
}

pub fn lookup_function_symbol(
    local_tabel: &ModuleLocals,
    function_tabel: &ModuleFunctions,
    name: &str,
) -> FnSymbol {
    let symbol = function_tabel
        .by_name(name)
        .expect(&format!("Expected function {} to be known!", name));

    match &function_tabel.get(symbol).kind {
        FunctionKind::Local(l) => {
            let mut args = SmallColl::new();
            for arg in l.args.iter() {
                let local = local_tabel.get(*arg);
                let ty = local.ty().clone();
                args.push((*arg, ty));
            }

            FnSymbol {
                id: symbol,
                arguments: args,
            }
        }
        other => panic!("Unexpected function kind: {other:?}"),
    }
}
