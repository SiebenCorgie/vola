/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # Vola's WASM backend
//!
//! The backend takes care of transforming a [Optimizer](vola_opt::Optimizer) into a WASM module. Any exported field is also exported under
//! its name into the module. You can then use the WASM code (or file) to load the generated code. For instance via [Wasmi](https://github.com/wasmi-labs/wasmi) or [Cranelift-Wasm] (https://docs.rs/cranelift-wasm/0.112.1/cranelift_wasm/).
//!
//!
//! Under the hood the module loads [vola-wasm-runtime]() crate's wasm module as a runtime. The crate implements special functions like `length`, `cross` etc, and loads functions like `sqrt`, `sin` etc.

use rvsdg::{
    attrib::{AttribLocation, FlagStore},
    Rvsdg,
};
#[cfg(feature = "viewer")]
use rvsdg_viewer::ViewerState;
use vola_common::Span;
use vola_opt::Optimizer;
mod error;
pub use error::WasmError;
mod graph;
mod passes;
mod runtime;
mod wasm;

pub use walrus;

pub struct WasmBackend {
    graph: Rvsdg<graph::WasmNode, graph::WasmEdge>,

    spans: FlagStore<Span>,
    names: FlagStore<String>,

    #[cfg(feature = "viewer")]
    viewer: ViewerState,
}

impl WasmBackend {
    pub fn new() -> Self {
        WasmBackend {
            graph: Rvsdg::new(),

            spans: FlagStore::new(),
            names: FlagStore::new(),

            #[cfg(feature = "viewer")]
            viewer: ViewerState::new(),
        }
    }

    pub fn intern_module(&mut self, optimizer: &Optimizer) -> Result<(), WasmError> {
        self.intern_optimizer(optimizer)?;

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_WASM_INTERN").is_ok()
        {
            self.push_debug_state("Optimizer interned into WASM");
        }

        Ok(())
    }

    #[cfg(not(feature = "viewer"))]
    pub fn push_debug_state(&mut self, name: &str) {}

    #[cfg(feature = "viewer")]
    pub fn push_debug_state_with<F>(&mut self, name: &str, with: F)
    where
        F: FnOnce(rvsdg_viewer::GraphStateBuilder) -> rvsdg_viewer::GraphStateBuilder,
    {
        use rvsdg_viewer::{layout::LayoutConfig, View};

        let mut local_typemap = FlagStore::new();
        for edg in self.graph.edges() {
            let ident = self.graph.edge(edg).ty.name();
            local_typemap.set(edg.into(), ident);
        }

        let layout_config = LayoutConfig {
            ignore_dead_node: false,
            ..Default::default()
        };

        let builder = self
            .viewer
            .new_state_builder(name, &self.graph, &layout_config)
            .with_flags("Name", &self.names)
            .with_flags("Span", &self.spans)
            .with_flags("Type", &local_typemap);
        with(builder).build();

        if std::env::var("VOLA_ALWAYS_WRITE_DUMP").is_ok() {
            self.dump_debug_state(&format!("{name}.bin"));
        }
    }

    #[cfg(feature = "viewer")]
    pub fn push_debug_state(&mut self, name: &str) {
        self.push_debug_state_with(name, |t| t)
    }

    #[cfg(not(feature = "viewer"))]
    pub fn dump_debug_state(&self, path: &dyn AsRef<std::path::Path>) {}

    #[cfg(feature = "viewer")]
    pub fn dump_debug_state(&self, path: &dyn AsRef<std::path::Path>) {
        println!("Dumping {:?}", path.as_ref());
        self.viewer.write_to_file(path)
    }

    pub fn span_or_empty(&self, attrib: impl Into<AttribLocation>) -> Span {
        self.spans
            .get(&attrib.into())
            .cloned()
            .unwrap_or(Span::empty())
    }
}
