use std::time::{Duration, Instant};

use smallvec::{SmallVec, smallvec};
use wasmtime::{Module, Store, Trap};
use yansi::Paint;

use crate::{
    config::Config,
    run::{PartialResult, TestResult},
};

pub fn try_execute(code: Vec<u8>, config: &Config, test_results: &mut TestResult) {
    //if no function is set, just return success,
    let target_fn = if let Some(f) = &config.execution_fn {
        f.clone()
    } else {
        //Early return as no run
        test_results.wasm_execute = PartialResult::Success(Duration::ZERO);
        return;
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

    let mut wasm_config = wasmtime::Config::new();
    wasm_config.wasm_backtrace(true);
    wasm_config.wasm_backtrace_details(wasmtime::WasmBacktraceDetails::Enable);

    let engine = match wasmtime::Engine::new(&wasm_config) {
        Ok(c) => c,
        Err(e) => {
            test_results
                .wasm_execute
                .push_error(format!("Could not init WASM engine: {e:?}"));
            return;
        }
    };

    let module = match Module::new(&engine, code) {
        Ok(module) => module,
        Err(e) => {
            test_results
                .wasm_execute
                .push_error(format!("Could not init module engine: {e:?}"));
            return;
        }
    };

    let linker = wasmtime::Linker::new(&engine);
    let mut store = Store::new(&engine, ());
    let instance = match linker.instantiate(&mut store, &module) {
        Ok(inst) => inst,
        Err(e) => {
            //check if we can unwrap a trap:

            if let Some(trap) = e.downcast_ref::<Trap>() {
                test_results.wasm_execute = PartialResult::error(format!(
                    "Failed to instatiate WASM-Time: {trap:?}\n\nBacktrace:\n{}",
                    e.backtrace()
                ));
                return;
            } else {
                test_results
                    .wasm_execute
                    .push_error(format!("Failed to instatiate WASM-Time: {e:?}"));
                return;
            }
        }
    };

    let entrypoint = match instance.get_func(&mut store, &target_fn) {
        Some(f) => f,
        None => {
            test_results.wasm_execute.push_error(format!(
                "Could not find function {} in WasmTime",
                target_fn.bold()
            ));
            return;
        }
    };

    let mut result_store: SmallVec<[wasmtime::Val; 32]> =
        smallvec![wasmtime::Val::F32(f32::NAN.to_bits()); results.len()];
    let param_store: SmallVec<[wasmtime::Val; 32]> = args
        .iter()
        .map(|param| wasmtime::Val::F32(param.to_bits()))
        .collect();

    let start = Instant::now();
    match entrypoint.call(&mut store, &param_store, &mut result_store) {
        Ok(_) => {}
        Err(e) => {
            test_results.wasm_execute.push_error(format!(
                "Failed to call {}: {e:?}\nDid you set the right amount of arguments and results?",
                target_fn.bold()
            ));
            return;
        }
    }
    let elapsed = start.elapsed();

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
        test_results.wasm_execute.push_error(format!(
            "Unexpected result {:?} != {:?}: (Diff: {:?})",
            results, wasm_f32_results, diffs
        ));
    } else {
        test_results.wasm_execute = PartialResult::Success(elapsed);
    }
}
