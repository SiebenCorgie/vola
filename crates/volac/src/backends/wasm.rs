/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::{io::Write, process::Stdio};

use crate::Target;

use super::PipelineBackend;

pub struct Wasm {
    pub target: Target,
}

impl Wasm {
    pub fn new(target: Target) -> Self {
        Self { target }
    }
}

impl PipelineBackend for Wasm {
    fn opt_pre_finalize(&self, opt: &mut vola_opt::Optimizer) -> Result<(), crate::PipelineError> {
        opt.imm_scalarize()?;
        opt.remove_unused_edges()?;
        opt.cleanup_export_lmd();
        Ok(())
    }

    fn execute(&mut self, opt: vola_opt::Optimizer) -> Result<Target, crate::PipelineError> {
        let mut backend = vola_backend_wasm::WasmBackend::new();
        backend.intern_module(&opt)?;

        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("VOLA_DUMP_WASM_BEFORE_CFG").is_ok()
        {
            backend.push_debug_state("WASM before CFG");
        }

        //This transforms the RVSDG into a WASM module
        //By loading the runtime, and then adding all exported functions to that.
        let mut module = backend.into_wasm_module()?;

        //Run garbage collection
        vola_backend_wasm::walrus::passes::gc::run(&mut module);

        //Depending on the target either write to file, or to buffer
        match &mut self.target {
            Target::Buffer(buffer) => {
                let buf = module.emit_wasm();
                *buffer = buf;
            }
            Target::File(path) => module.emit_wasm_file(path).unwrap(),
        }

        Ok(self.target.clone())
    }

    fn try_verify(&self) -> Result<(), String> {
        //Try to use wasm-tools to verify the emitted file
        match &self.target {
            //start the validator on the src
            Target::File(path) => {
                let output = std::process::Command::new("wasm-tools")
                    .arg("validate")
                    .arg(path)
                    .output()
                    .map_err(|e| e.to_string())?;
                //push output stream
                std::io::stdout().write_all(&output.stdout).unwrap();
                std::io::stderr().write_all(&output.stderr).unwrap();
                if !output.status.success() {
                    Err(format!("Failed to validate module with: {}", output.status))
                } else {
                    Ok(())
                }
            }
            //stream the buffer on stdin.
            Target::Buffer(b) => {
                let mut command = std::process::Command::new("wasm-tools")
                    .arg("validate")
                    .stdin(Stdio::piped())
                    .spawn()
                    .unwrap();
                let buffcpy = b.clone();
                let mut command_in = command.stdin.take().unwrap();
                std::thread::spawn(move || {
                    command_in
                        .write_all(&buffcpy)
                        .expect("Failed to write to stdin");
                    command_in.flush().unwrap();
                });
                //now wait for it to end and output
                let output = command.wait_with_output().map_err(|e| e.to_string())?;

                std::io::stdout().write_all(&output.stdout).unwrap();
                std::io::stderr().write_all(&output.stderr).unwrap();
                if !output.status.success() {
                    Err(format!("Failed to validate module with: {}", output.status))
                } else {
                    Ok(())
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use std::panic::UnwindSafe;

    use static_assertions::assert_impl_all;

    use crate::backends::Wasm;

    #[test]
    fn impl_send() {
        assert_impl_all!(Wasm: Send);
    }

    #[test]
    fn impl_sync() {
        assert_impl_all!(Wasm: Sync);
    }

    #[test]
    fn impl_unwind_safe() {
        assert_impl_all!(Wasm: UnwindSafe);
    }
}
