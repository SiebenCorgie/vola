/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use super::PipelineBackend;
use crate::Target;
use std::fs;
use wasmtime::CodeBuilder;

pub struct Native {
    //The wasm backend we actually use to create a WASM module
    wasm_backend: super::Wasm,
    target: Target,
}

impl Native {
    pub fn new(target: Target) -> Self {
        Self {
            //NOTE: The wasm backend will always be _in-memory_
            wasm_backend: super::Wasm::new(Target::Buffer(Vec::new())),
            target,
        }
    }
}

impl PipelineBackend for Native {
    fn execute(&mut self, opt: vola_opt::Optimizer) -> Result<crate::Target, crate::PipelineError> {
        let wasmcode = self.wasm_backend.execute(opt)?.unwrap_buffer();

        let config = wasmtime::Config::new();
        let engine = wasmtime::Engine::new(&config).unwrap();
        let mut code = CodeBuilder::new(&engine);
        code.wasm_binary(wasmcode, None).unwrap();

        let native_artifact = code.compile_module_serialized().unwrap();
        self.target.update_from_buffer(&native_artifact);

        //if the target is a file, write to file
        if self.target.is_file() {
            let target_file = self.target.target_file_name("dylib").unwrap();
            if target_file.exists() {
                fs::remove_file(&target_file).unwrap();
            }
            fs::write(target_file, native_artifact).unwrap();
        }

        Ok(self.target.clone())
    }

    fn opt_pre_finalize(&self, opt: &mut vola_opt::Optimizer) -> Result<(), crate::PipelineError> {
        self.wasm_backend.opt_pre_finalize(opt)
    }

    fn try_verify(&self) -> Result<(), String> {
        self.wasm_backend.try_verify()
    }
}

#[cfg(test)]
mod test {
    use static_assertions::assert_impl_all;

    use crate::backends::Native;

    #[test]
    fn impl_send() {
        assert_impl_all!(Native: Send);
    }

    #[test]
    fn impl_sync() {
        assert_impl_all!(Native: Sync);
    }
}
