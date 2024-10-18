/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::{io::Write, process::Stdio};

use vola_backend_spirv::{rspirv::binary::Assemble, SpirvConfig};

use crate::{PipelineError, Target};

use super::PipelineBackend;

pub struct Spirv {
    pub target: Target,
    pub config: SpirvConfig,
}

impl Spirv {
    pub fn new(target: Target) -> Self {
        Spirv {
            target,
            config: SpirvConfig::default(),
        }
    }
}

impl PipelineBackend for Spirv {
    fn opt_pre_finalize(&self, opt: &mut vola_opt::Optimizer) -> Result<(), PipelineError> {
        opt.imm_scalarize()?;
        opt.cne_exports()?;

        Ok(())
    }

    fn execute(&mut self, opt: vola_opt::Optimizer) -> Result<Target, PipelineError> {
        let mut backend = vola_backend_spirv::SpirvBackend::new(self.config.clone());

        backend.intern_module(&opt)?;
        backend.hl_to_spv_nodes()?;
        backend.legalize()?;

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_SPIRV_FINAL").is_ok() {
            backend.push_debug_state("Final SPIR-V Graph");
        }

        let spvmodule = backend
            .build()
            .expect("Failed to build SPIR-V module from backend graph.");

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_SPIRV_FINAL").is_ok() {
            backend.push_debug_state("Emitted SPIR-V Graph");
        }
        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("VOLA_DUMP_SPIRV_STATE").is_ok()
            || std::env::var("VOLA_DUMP_VIEWER").is_ok()
        {
            backend.dump_debug_state(&"SpirvState.bin");
        }
        let words = spvmodule.assemble();
        let bytes = bytemuck::cast_slice(&words);

        if let Some(fname) = self.target.target_file_name("spv") {
            if let Target::File(f) = &mut self.target {
                //Rewrite the file name
                *f = fname;
                println!("Emitting SPIR-V as {f:?}");
            }
        }
        //now update the target with the just generated buffer
        self.target.update_from_buffer(bytes);

        Ok(self.target.clone())
    }

    ///tries to use the `spirv-val` command (if installed) to verify the code
    fn try_verify(&self) -> Result<(), String> {
        match &self.target {
            //start the validator on the src
            Target::File(path) => {
                let output = std::process::Command::new("spirv-val")
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
                let mut command = std::process::Command::new("spirv-val")
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

    use crate::backends::Spirv;

    #[test]
    fn impl_send() {
        assert_impl_all!(Spirv: Send);
    }

    #[test]
    fn impl_sync() {
        assert_impl_all!(Spirv: Sync);
    }
    #[test]
    fn impl_unwind_safe() {
        assert_impl_all!(Spirv: UnwindSafe);
    }
}
