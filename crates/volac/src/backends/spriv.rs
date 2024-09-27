/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

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

        let mut target = self.target.clone();
        if let Target::File(f) = &mut self.target {
            //Rewrite the file name
            if let Some(new_path) = target.target_file_name("spv") {
                *f = new_path;
            }
            println!("Emitting SPIR-V as {f:?}");
        }
        target.update_from_buffer(bytes);

        Ok(target)
    }
}
