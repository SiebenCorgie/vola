/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # Vola-Opt
//!
//! The vola optimizer.
//!
//! The optimizer employs different _dialects_ to handle aspects of the input-language. Those dialects are
//! lowered at some point to the [_alge_](alge) dialect. This graph can then be handed off to backends to generate
//! code.
//!
//! ## Dialects
//! ### [CSG](csg)
//!
//! Models the CSG Trees that are defined in the language and ultimately exported. Takes care
//! of building the final tree, by resolving sub trees, and uses the access descriptors to build the
//! tree's data flow.
//!
//! ### [AutoDiff](autodiff)
//!
//! Represents differentiation in the graph. Lowering performs auto-differentiation of the input expression with respect to any other expression(s).
//! Is only defined on algebraic expressions.
//!
//! ### [Alge](alge)
//!
//! Used to represent algebraic, logic and boolean expressions.
//!
//! ### [Imm](imm)
//!
//! Immediate, so usually constant values in the graph. Also forms the basis for many static code optimizations.
//!
//! ### [TypeLevel](typelevel)
//!
//! Models type-level nodes, constructing and destructing composite-types at the moment.
//!
//! ## Passes
//!
//! Lowering, checks and transformation passes are defined as functions on [Optimizer]. See [Optimizer::full_graph_cnf] or [Optimizer::specialize_all_exports] for examples.

//NOTE: We need that trait for the OptNode, so we can Upcast `DialectNode: Any` to `Any`.
#![doc(html_logo_url = "https://gitlab.com/tendsinmende/vola/-/raw/main/resources/vola_icon.svg")]

use ahash::AHashMap;
use common::Ty;
use config::Config;
use graph::{
    auxiliary::{Function, Impl, ImplKey},
    CsgConcept, CsgDef,
};
use rvsdg::{attrib::FlagStore, Rvsdg};

use rvsdg_viewer::layout::LayoutConfig;
use vola_common::Span;

pub mod alge;
pub mod common;
pub mod csg;
pub mod error;
pub use error::OptError;
pub mod autodiff;
pub mod config;
pub mod graph;
pub mod imm;
pub mod passes;
pub mod typelevel;
pub mod util;

//Re-Export all of these, since they basically form the basis of _everything_.
pub use graph::{DialectNode, OptEdge, OptNode, TypeState};

pub type OptGraph = Rvsdg<OptNode, OptEdge>;

///The _whole_ optimizer. Mostly ties together the RVSDG and some auxiliary structures that
/// make wiring the the correct nodes together possible.
pub struct Optimizer {
    pub graph: OptGraph,

    ///All known concept definitions keyed by their name
    //NOTE: using the name, since thats how we reference them all the time.
    pub concepts: AHashMap<String, CsgConcept>,

    ///All known entity and operation definitions.
    pub csg_node_defs: AHashMap<String, CsgDef>,

    ///lookup table for the λ-Nodes of entity implementation of concepts
    pub concept_impl: AHashMap<ImplKey, Impl>,

    ///Lookup table for all functions that are not part of an implementation.
    pub functions: AHashMap<String, Function>,

    ///All known type tags of ports and nodes. Can be used to do type checking, or infer edge types.
    pub typemap: FlagStore<Ty>,

    ///Can be used to tag source Spans for nodes that are no OptNodes
    pub span_tags: FlagStore<Span>,

    ///Can be used to name nodes, which might be interesting for passes that
    ///do not have knowledge of the original AST/Source, but still need to emit human
    /// readable names.
    pub names: FlagStore<String>,

    /// Flags nodes or outputs as producing a named variable. Is used when rerouting
    /// variables into controll-flow notes.
    ///
    /// Automatically setup by let and assign bindings as well as when importing
    /// into intra-procedural nodes.
    pub var_producer: FlagStore<String>,

    #[cfg(feature = "viewer")]
    pub viewer_state: rvsdg_viewer::ViewerState,

    pub config: Config,
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            graph: Rvsdg::new(),
            concepts: AHashMap::default(),
            csg_node_defs: AHashMap::default(),
            concept_impl: AHashMap::default(),
            functions: AHashMap::default(),
            typemap: FlagStore::new(),
            span_tags: FlagStore::new(),
            names: FlagStore::new(),
            var_producer: FlagStore::new(),
            #[cfg(feature = "viewer")]
            viewer_state: rvsdg_viewer::ViewerState::new(),
            config: Config::default(),
        }
    }

    pub fn dump_svg(&self, name: &str, ignore_dead_node: bool) {
        let conf = LayoutConfig {
            grid_padding: 30,
            grid_empty_spacing: 15,
            ignore_dead_node,
            ..Default::default()
        };
        rvsdg_viewer::into_svg_with_config(&self.graph, name, &conf)
    }

    ///Pushes the current graph state under the given name.
    #[cfg(feature = "viewer")]
    pub fn push_debug_state(&mut self, name: &str) {
        self.push_debug_state_with(name, |t| t)
    }

    #[cfg(feature = "viewer")]
    pub fn push_debug_state_with<F>(&mut self, name: &str, with: F)
    where
        F: FnOnce(rvsdg_viewer::GraphStateBuilder) -> rvsdg_viewer::GraphStateBuilder,
    {
        //NOTE propbably do not rebuild this each time?
        let mut typemap = self.typemap.clone();
        for edge in self.graph.edges() {
            if let Some(ty) = self.graph.edge(edge).ty.get_type() {
                typemap.set(rvsdg::attrib::AttribLocation::Edge(edge).into(), ty.clone());
            }
        }

        let layout_config = LayoutConfig {
            ignore_dead_node: false,
            ..Default::default()
        };

        {
            let builder = self
                .viewer_state
                .new_state_builder(name, &self.graph, &layout_config)
                .with_flags("Type", &typemap)
                .with_flags("Span", &self.span_tags)
                .with_flags("Name", &self.names)
                .with_flags("Variable Producer", &self.var_producer);

            with(builder).build();
        }
        if std::env::var("VOLA_ALWAYS_WRITE_DUMP").is_ok() {
            self.dump_debug_state(&format!("{name}.bin"));
        }
    }

    ///Shortcut to immediatly write debug state, without pushing it to the chain.
    #[cfg(feature = "viewer")]
    #[allow(unused)]
    pub(crate) fn write_debug_state<F>(&self, name: &str, with: F)
    where
        F: FnOnce(rvsdg_viewer::GraphStateBuilder) -> rvsdg_viewer::GraphStateBuilder,
    {
        //NOTE propbably do not rebuild this each time?
        let mut typemap = self.typemap.clone();
        for edge in self.graph.edges() {
            if let Some(ty) = self.graph.edge(edge).ty.get_type() {
                typemap.set(rvsdg::attrib::AttribLocation::Edge(edge).into(), ty.clone());
            }
        }

        let layout_config = LayoutConfig {
            ignore_dead_node: true,
            ..Default::default()
        };

        let mut viewer_state = rvsdg_viewer::ViewerState::new();
        {
            let builder = viewer_state
                .new_state_builder(name, &self.graph, &layout_config)
                .with_flags("Type", &typemap)
                .with_flags("Span", &self.span_tags)
                .with_flags("Name", &self.names)
                .with_flags("Variable Producer", &self.var_producer);

            with(builder).build();
        }

        let path = format!("{name}.bin");
        println!("Writing debug state to {:?}", path);
        viewer_state.write_to_file(&path);
    }

    #[cfg(feature = "viewer")]
    pub fn dump_debug_state(&self, path: impl AsRef<std::path::Path>) {
        println!("Writing debug state to {:?}", path.as_ref());
        if std::env::var("VOLA_DUMP_SVG").is_ok() {
            let mut svg_path = path.as_ref().to_path_buf();
            svg_path.set_extension("svg");
            self.dump_svg(svg_path.to_str().unwrap(), false);
        }
        self.viewer_state.write_to_file(&path)
    }
}
