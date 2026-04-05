use std::{io::Write, process::Stdio};

pub use vola_backend_wasm;
use vola_backend_wasm::{WasmBackend, WasmError};
use vola_common::VolaError;
use vola_opt::{
    OptError,
    passes::{Cleanup, ImmScalarize, InlineAll},
};

use crate::{
    OptModule,
    passes::{Pass, PassError},
};

struct WasmCleanupPass;
impl Pass for WasmCleanupPass {
    fn execute(self, opt: &mut vola_opt::Optimizer) -> Result<(), PassError> {
        //NOTE: Our WASM backend currently does not support calls, so we gotta inline all :/
        InlineAll::setup(opt)
            .execute()
            .map_err(|e| e.to_error::<OptError>())?;
        ImmScalarize::setup(opt)
            .scalarize_all()
            .map_err(|e| VolaError::new(e.into()))?;
        Cleanup::setup(opt)
            .remove_unused_edges()
            .map_err(|e| VolaError::new(e.into()))?
            .cleanup_export_lmd();
        Ok(())
    }
}

pub struct WasmModule {
    pub backend: WasmBackend,
}

impl WasmModule {
    pub fn new() -> Self {
        //NOTE: for the SPIRV backend we currently _need_ to do some
        // cleanup. Mainly typing edges and scalarizing all the code

        let backend = WasmBackend::new();

        WasmModule { backend }
    }

    pub fn lower_opt(&mut self, mut opt: OptModule) -> Result<(), WasmError> {
        opt.apply_pass(WasmCleanupPass).unwrap();
        self.backend.intern_module(&opt.opt)
    }

    ///Generates the WASM bytecode of the current module. If `try_verify` is set, tries to execute `wasm-tools`
    /// on the command line, and emits an error, if validation failed (or the command was not found).
    pub fn build(&mut self, try_verify: bool) -> Result<Vec<u8>, WasmError> {
        let mut wasm_module = self.backend.into_wasm_module()?;
        let code = wasm_module.emit_wasm();
        if try_verify {
            Self::try_verify(&code)?;
        }

        Ok(code)
    }

    ///tries to use the `wasm-tools` command (if installed) to verify the code
    fn try_verify(bytes: &[u8]) -> Result<(), WasmError> {
        let mut command = std::process::Command::new("wasm-tools")
            .arg("validate")
            .stdin(Stdio::piped())
            .spawn()
            .unwrap();
        let buffcpy = bytes.to_vec();
        let mut command_in = command.stdin.take().unwrap();
        std::thread::spawn(move || {
            command_in
                .write_all(&buffcpy)
                .expect("Failed to write to stdin");
            command_in.flush().unwrap();
        });
        //now wait for it to end and output
        let output = command
            .wait_with_output()
            .map_err(|e| WasmError::ValidationError(e.to_string()))?;

        std::io::stdout().write_all(&output.stdout).unwrap();
        std::io::stderr().write_all(&output.stderr).unwrap();
        if !output.status.success() {
            Err(WasmError::ValidationError(format!(
                "Failed to validate module with: {}",
                output.status
            )))
        } else {
            Ok(())
        }
    }
}
