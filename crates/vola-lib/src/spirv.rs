use std::{io::Write, process::Stdio};

use vola_backend_spirv::{BackendSpirvError, SpirvBackend, SpirvConfig, rspirv::binary::Assemble};
use vola_common::VolaError;
use vola_opt::passes::{Cleanup, ImmScalarize, TypeEdges};

use crate::{
    OptModule,
    passes::{Pass, PassError},
};

struct SpirvCleanupPass;
impl Pass for SpirvCleanupPass {
    fn execute(self, opt: &mut vola_opt::Optimizer) -> Result<(), PassError> {
        TypeEdges::setup(opt).execute()?;
        ImmScalarize::setup(opt)
            .scalarize_all()
            .map_err(|e| VolaError::new(e))?;
        Cleanup::setup(opt)
            .cne_exports()
            .map_err(|e| VolaError::new(e))?;

        Ok(())
    }
}

pub struct SpirvModule {
    #[allow(unused)]
    config: SpirvConfig,
    pub backend: SpirvBackend,
}

impl SpirvModule {
    pub fn new(config: SpirvConfig) -> Self {
        //NOTE: for the SPIRV backend we currently _need_ to do some
        // cleanup. Mainly typing edges and scalarizing all the code

        let backend = SpirvBackend::new(config.clone());

        SpirvModule { backend, config }
    }

    pub fn lower_opt(&mut self, mut opt: OptModule) -> Result<(), BackendSpirvError> {
        opt.apply_pass(SpirvCleanupPass).unwrap();
        self.backend.intern_module(&opt.opt)
    }

    pub fn legalize(&mut self) -> Result<(), BackendSpirvError> {
        self.backend.hl_to_spv_nodes()?;
        self.backend.legalize()?;

        Ok(())
    }

    ///Generates the SPIRV bytecode of the current module. If `try_verify` is set, tries to execute [spirv-val]
    /// on the command line, and emits an error, if validation failed (or the command was not found).
    pub fn build(&mut self, try_verify: bool) -> Result<Vec<u8>, BackendSpirvError> {
        let spvmodule = self.backend.build()?;

        let words = spvmodule.assemble();
        let bytes = bytemuck::cast_slice(&words);

        if try_verify {
            Self::try_verify(bytes)?;
        }

        Ok(bytes.to_vec())
    }

    ///tries to use the `spirv-val` command (if installed) to verify the code
    fn try_verify(bytes: &[u8]) -> Result<(), BackendSpirvError> {
        let mut command = std::process::Command::new("spirv-val")
            .stdin(Stdio::piped())
            .spawn()
            .unwrap();
        let mut command_in = command.stdin.take().unwrap();

        //TODO: don't copy?
        let buffcpy = bytes.to_vec();
        std::thread::spawn(move || {
            command_in
                .write_all(&buffcpy)
                .expect("Failed to write to stdin");
            command_in.flush().unwrap();
        });
        //now wait for it to end and output
        let output = command
            .wait_with_output()
            .map_err(|e| BackendSpirvError::ValidationFailed(e.to_string()))?;

        std::io::stdout().write_all(&output.stdout).unwrap();
        std::io::stderr().write_all(&output.stderr).unwrap();
        if !output.status.success() {
            Err(BackendSpirvError::ValidationFailed(format!(
                "Failed to validate module with: {}",
                output.status
            )))
        } else {
            Ok(())
        }
    }
}
