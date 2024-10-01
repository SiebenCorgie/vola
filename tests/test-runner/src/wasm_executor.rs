use std::u32;

use smallvec::{smallvec, SmallVec};
use volac::Target;
use wasmtime::{Module, Store};
use yansi::Paint;

use crate::{config::Config, run::TestState};

pub fn try_execute(target: Target, config: &Config) -> TestState {
    //if no function is set, just return success,
    let target_fn = if let Some(f) = &config.execution_fn {
        f.clone()
    } else {
        return TestState::Success;
    };

    let args = config
        .execution_args
        .as_ref()
        .unwrap_or(&SmallVec::new())
        .clone();
    let results = config
        .execution_res
        .as_ref()
        .unwrap_or(&SmallVec::new())
        .clone();

    let target_bytes = match target {
        Target::Buffer(b) => b,
        Target::File(f) => match std::fs::read(&f) {
            Ok(b) => b,
            Err(e) => return TestState::Error(format!("Failed to read file {f:?}: {e:?}")),
        },
    };

    let engine = wasmtime::Engine::default();

    let module = match Module::new(&engine, target_bytes) {
        Ok(module) => module,
        Err(e) => {
            return TestState::Error(format!("Failed to load buffer as WASM module: {e:?}"));
        }
    };

    let linker = wasmtime::Linker::new(&engine);
    let mut store = Store::new(&engine, ());
    let instance = match linker.instantiate(&mut store, &module) {
        Ok(inst) => inst,
        Err(e) => return TestState::Error(format!("Failed to instatiate WASMTime: {e:?}")),
    };

    let entrypoint = match instance.get_func(&mut store, &target_fn) {
        Some(f) => f,
        None => {
            return TestState::Error(format!(
                "Could not find function {} in WasmTime",
                target_fn.bold()
            ))
        }
    };

    let mut result_store: SmallVec<[wasmtime::Val; 32]> =
        smallvec![wasmtime::Val::F32(f32::NAN.to_bits()); results.len()];
    let param_store: SmallVec<[wasmtime::Val; 32]> = args
        .iter()
        .map(|param| wasmtime::Val::F32(param.to_bits()))
        .collect();

    match entrypoint.call(&mut store, &param_store, &mut result_store) {
        Ok(_) => {}
        Err(e) => {
            return TestState::Error(format!(
                "Failed to call {}: {e:?}\nDid you set the right amount of arguments and results?",
                target_fn.bold()
            ))
        }
    }

    //returned successfully, check that the result is indeed correct
    for (idx, (calculated, expected)) in result_store.iter().zip(results.iter()).enumerate() {
        let calcf = if let wasmtime::Val::F32(f) = calculated {
            f32::from_bits(*f)
        } else {
            return TestState::Error(format!("Calculated value was not F32"));
        };

        if (calcf - *expected).abs() > config.exec_eps {
            return TestState::Error(format!(
                "Unexpected result[{}]: {} != {}",
                idx, calcf, expected
            ));
        }
    }

    TestState::Success
}
