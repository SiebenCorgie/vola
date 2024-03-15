use crate::{error::BackendSpirvError, SpirvBackend, SpirvConfig, SpirvModule};

impl SpirvBackend {
    pub fn into_spv_module(&self, config: &SpirvConfig) -> Result<SpirvModule, BackendSpirvError> {
        todo!()
    }
}
