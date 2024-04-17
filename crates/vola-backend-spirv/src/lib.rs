/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # Vola's SPIR-V backend
//!
//! Implements the transformation from the RVSDG in vola-opt into a valid SPIR-V module.
//!
//! With this transformation we also need to respect all quirks of the target format. This manyl
//! is the __recursion-less-ness__ of SPIR-V atm. So no ϕ-Nodes at all at the moment.
//!
//! Apart from that the transformation is pretty easy, since SPIR-V itself is an SCF-Graph, so
//! translation from RVSDG is pretty straight forward.
//!
//!
//! Right now we exploid the fact, that vola-opt only outputs a ddg within a λ-Node. So constructing the
//! SCF is pretty easy.
//!
//! Later on we should implement the RVSDG-Destruction algorithm described in the RVSDG-paper.

use ahash::AHashSet;
use graph::{BackendEdge, BackendNode};
use rvsdg::{attrib::FlagStore, Rvsdg};
use spv::SpvType;
use vola_common::Span;
use vola_opt::Optimizer;

pub use rspirv;

mod error;
pub use error::BackendSpirvError;
mod passes;

//Defines the SPIR-V dialect used in the backend's RVSDG.
mod graph;
mod spv;

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
        let mut ext_inst = AHashSet::default();
        ext_inst.insert("GLSL.std.450".to_owned());

        SpirvConfig {
            version_major: 1,
            version_minor: 5,
            extensions: AHashSet::default(),
            ext_inst,
        }
    }
}

pub struct SpirvBackend {
    //NOTE: option allows us to temporarly take
    //      the builder
    graph: Rvsdg<BackendNode, BackendEdge>,
    ///Can be used to tag things with their identifiers
    idents: FlagStore<String>,
    ///Can be used to tag things with a source-span, if there is such a thing.
    spans: FlagStore<Span>,
    ///Used to declare types on _none-edge_ things. Mostly ports to derive a type for an unused
    ///attribute later on.
    typemap: FlagStore<SpvType>,
    config: SpirvConfig,
}

impl SpirvBackend {
    pub fn new(config: SpirvConfig) -> Self {
        SpirvBackend {
            graph: Rvsdg::new(),
            idents: FlagStore::new(),
            spans: FlagStore::new(),
            typemap: FlagStore::new(),
            config,
        }
    }

    ///Interns the `opt` module into this backend builder. All as _export_ declared
    /// λ-Nodes will be exported in the [SpirvModule] as well.
    ///
    /// Assumes that all nodes that are live/reachable (from any ω-result) are in the "alge" dialect.
    pub fn intern_module(&mut self, opt: &Optimizer) -> Result<(), BackendSpirvError> {
        self.intern_opt_graph(opt)
            .map_err(|e| BackendSpirvError::InterningError(e))
    }

    ///Builds the module based on the current configuration and graph.
    pub fn build(&self) -> Result<SpirvModule, BackendSpirvError> {
        self.into_spv_module(&self.config)
    }

    pub fn dump_svg(&self, name: &str, ignore_dead_node: bool) {
        let conf = rvsdg_viewer::layout::LayoutConfig {
            grid_padding: 30,
            grid_empty_spacing: 15,
            ignore_dead_node,
            ..Default::default()
        };
        rvsdg_viewer::into_svg_with_config(&self.graph, name, &conf)
    }
}
