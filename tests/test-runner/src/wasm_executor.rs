use smallvec::{smallvec, SmallVec};
use volac::Target;
use wasmtime::{Module, Store, Trap};
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

    let mut wasm_config = wasmtime::Config::new();
    wasm_config.wasm_backtrace(true);
    wasm_config.wasm_backtrace_details(wasmtime::WasmBacktraceDetails::Enable);

    let engine = match wasmtime::Engine::new(&wasm_config)
        .map_err(|e| TestState::Error(format!("Failed to start wasmtime Engine: {e:?}")))
    {
        Ok(c) => c,
        Err(e) => return e,
    };

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
        Err(e) => {
            //check if we can unwrap a trap:

            if let Some(trap) = e.downcast_ref::<Trap>() {
                return TestState::Error(format!(
                    "Failed to instatiate WASM-Time: {trap:?}\n\nBacktrace:\n{}",
                    e.backtrace()
                ));
            } else {
                return TestState::Error(format!("Failed to instatiate WASM-Time: {e:?}"));
            }
        }
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
    let mut any_wrong = false;
    let mut diffs: SmallVec<[f32; 3]> = smallvec![0.0; results.len()];
    let wasm_f32_results: SmallVec<[f32; 3]> = result_store
        .iter()
        .map(|i| {
            if let wasmtime::Val::F32(f) = i {
                f32::from_bits(*f)
            } else {
                panic!("Had none-float return type");
            }
        })
        .collect();

    for (idx, (calculated, expected)) in wasm_f32_results.iter().zip(results.iter()).enumerate() {
        let diff = (calculated - *expected).abs();
        diffs[idx] = diff;
        if diff > config.exec_eps {
            any_wrong = true;
        }
    }

    if any_wrong {
        return TestState::Error(format!(
            "Unexpected result {:?} != {:?}: (Diff: {:?})",
            results, wasm_f32_results, diffs
        ));
    } else {
        TestState::Success
    }
}
