/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # Vola's SPIR-V backend
//!
//! Implements the transformation from the RVSDG in vola-opti into a valid SPIR-V module.
//!
//! With this transformation we also need to respect all quirks of the target format. This manyl
//! is the __recursion-less-ness__ of SPIR-V atm. So no ϕ-Nodes at all at the moment.
//!
//! Apart from that the transformation is pretty easy, since SPIR-V itself is an SCF-Graph, so
//! translation from RVSDG is pretty straight forward.
//!

use ahash::AHashSet;
use error::BackendSpirvError;
use graph::{SpvEdg, SpvOp};
use rvsdg::Rvsdg;
use vola_opt::Optimizer;

mod error;
mod graph;
mod passes;

pub type SpirvModule = rspirv::dr::Module;

///Configures auxilary data for the spirv module, like the targeting SPIRV
/// version, and expected extensions.
#[derive(Clone, Debug)]
pub struct SpirvConfig {
    version_major: u8,
    version_minor: u8,
    ///Extensions that are required for this module. This might be patched
    /// by the backend with additional extensions.
    extensions: AHashSet<String>,
    ///Extended instruction sets that are required. This might be patched by the
    //backend with additional instruction-set extensions.
    ext_inst: AHashSet<String>,
}

impl Default for SpirvConfig {
    fn default() -> Self {
        SpirvConfig {
            version_major: 1,
            version_minor: 6,
            extensions: AHashSet::default(),
            ext_inst: AHashSet::default(),
        }
    }
}

///The backend borrows the source graph for its lifetime.
pub struct SpirvBackend {
    graph: Rvsdg<SpvOp, SpvEdg>,
    config: SpirvConfig,
}

impl SpirvBackend {
    pub fn new() -> Self {
        SpirvBackend {
            graph: Rvsdg::new(),
            config: SpirvConfig::default(),
        }
    }

    ///Interns the `opt` module into this backend builder. All as _export_ declared
    /// λ-Nodes will be exported in the [SpirvModule] as well.
    ///
    /// assumes that all nodes that are live/reachabl (from any ω-result) are in the "alge" dialect.
    pub fn intern_module(&mut self, opt: &Optimizer) -> Result<(), BackendSpirvError> {
        self.intern(opt)
    }

    ///Builds the module based on the current configuration.
    pub fn build(&self) -> Result<SpirvModule, BackendSpirvError> {
        let config = self.config.clone();
        self.into_spv_module(&config)
    }
}
