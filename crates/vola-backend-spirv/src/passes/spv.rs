use rspirv::dr::Builder;

use crate::{SpirvBackend, SpirvConfig, SpirvModule};

impl SpirvBackend {
    pub fn into_spv_module(&self, config: &SpirvConfig) -> SpirvModule {
        //Build the initial _empty_ module with the header specified by config
        let mut b = Builder::new();
        b.set_version(config.version_major, config.version_minor);
        b.memory_model(
            rspirv::spirv::AddressingModel::Logical,
            rspirv::spirv::MemoryModel::Vulkan,
        );
        for ext in &config.extensions {
            b.extension(ext.clone());
        }

        for ext_inst in &config.ext_inst {
            b.ext_inst_import(ext_inst.clone());
        }

        todo!("implement module emission");

        b.module()
    }
}
