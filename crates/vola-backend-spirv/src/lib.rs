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

#![doc(html_logo_url = "https://gitlab.com/tendsinmende/vola/-/raw/main/resources/vola_icon.svg")]

use ahash::AHashSet;
use graph::{BackendEdge, BackendNode};
use rvsdg::{attrib::FlagStore, Rvsdg};
#[cfg(feature = "viewer")]
use rvsdg_viewer::ViewerState;
use spv::SpvType;
use vola_common::Span;
use vola_opt::Optimizer;

pub use rspirv;

mod error;
pub use error::BackendSpirvError;
mod passes;

//Defines the SPIR-V dialect used in the backend's RVSDG.
mod graph;
pub(crate) mod hl;
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
    pub(crate) idents: FlagStore<String>,
    ///Can be used to tag things with a source-span, if there is such a thing.
    spans: FlagStore<Span>,
    ///Used to declare types on _none-edge_ things. Mostly ports to derive a type for an unused
    ///attribute later on.
    pub(crate) typemap: FlagStore<SpvType>,
    pub(crate) spirv_id_map: FlagStore<u32>,
    config: SpirvConfig,

    #[cfg(feature = "viewer")]
    viewer: ViewerState,
}

impl SpirvBackend {
    pub fn new(config: SpirvConfig) -> Self {
        SpirvBackend {
            graph: Rvsdg::new(),
            idents: FlagStore::new(),
            spans: FlagStore::new(),
            typemap: FlagStore::new(),
            spirv_id_map: FlagStore::new(),
            config,
            #[cfg(feature = "viewer")]
            viewer: ViewerState::new(),
        }
    }

    ///Interns the `opt` module into this backend builder. All as _export_ declared
    /// λ-Nodes will be exported in the [SpirvModule] as well.
    ///
    /// Assumes that all nodes that are live/reachable (from any ω-result) are in the "alge" dialect.
    pub fn intern_module(&mut self, opt: &Optimizer) -> Result<(), BackendSpirvError> {
        let res = self
            .intern_opt_graph(opt)
            .map_err(|e| BackendSpirvError::InterningError(e));

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_SPV_INTERNED").is_ok()
        {
            self.push_debug_state("Opt interned into SPIR-V");
        }

        res
    }

    ///Builds the module based on the current configuration and graph.
    pub fn build(&mut self) -> Result<SpirvModule, BackendSpirvError> {
        self.into_spv_module()
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

    #[cfg(not(feature = "viewer"))]
    pub fn push_debug_state(&mut self, name: &str) {}

    #[cfg(feature = "viewer")]
    pub fn push_debug_state(&mut self, name: &str) {
        use rvsdg_viewer::layout::LayoutConfig;

        let mut local_typemap = self.typemap.clone();
        for edg in self.graph.edges() {
            if let Some(ty) = self.graph.edge(edg).ty.get_type() {
                local_typemap.set(edg.into(), ty.clone());
            }
        }

        let layout_config = LayoutConfig {
            ignore_dead_node: false,
            ..Default::default()
        };

        self.viewer
            .new_state_builder(name, &self.graph, &layout_config)
            .with_flags("Name", &self.idents)
            .with_flags("Span", &self.spans)
            .with_flags("Type", &local_typemap)
            .with_flags("SPIRV-ID", &self.spirv_id_map)
            .build();

        if std::env::var("VOLA_ALWAYS_WRITE_DUMP").is_ok() {
            self.dump_debug_state(&format!("{name}.bin"));
        }
    }

    #[cfg(not(feature = "viewer"))]
    pub fn dump_debug_state(&self, path: &dyn AsRef<std::path::Path>) {}
    #[cfg(feature = "viewer")]
    pub fn dump_debug_state(&self, path: &dyn AsRef<std::path::Path>) {
        println!("Dumping {:?}", path.as_ref());
        self.viewer.write_to_file(path)
    }
}
