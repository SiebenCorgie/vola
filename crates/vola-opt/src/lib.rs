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
//! Currently is based on two high-level dialects with a shared type system. As well as one low-level dialect
//! that is SPIR-V like.
//!
//! ### CSG-Dialect
//!
//! Models the CSG Trees that are defined in the language and ultimately exported. Takes care
//! of building the final tree, by resolving sub trees, and uses the access descriptors to build the
//! tree's data flow.
//!
//! ### Alge-Dialect
//!
//! Used to represent algebraic expressions.

//NOTE: We need that trait for the OptNode, so we can Upcast `DialectNode: Any` to `Any`.
#![feature(trait_upcasting)]

use ahash::AHashMap;
use alge::{
    algefn::AlgeFn,
    implblock::{ConceptImpl, ConceptImplKey},
};
use common::Ty;
use config::Config;
use csg::{exportfn::ExportFn, fielddef::FieldDef};
use rvsdg::{attrib::FlagStore, Rvsdg};

use rvsdg_viewer::layout::LayoutConfig;
use vola_ast::{
    csg::{CSGConcept, CSGNodeDef},
    VolaAst,
};
use vola_common::Span;

pub mod alge;
mod ast;
pub mod common;
pub mod csg;
mod error;
pub use error::OptError;
mod autodiff;
pub mod config;
mod graph;
pub mod imm;
mod passes;
mod util;

//Re-Export all of these, since they basically form the basis of _everything_.
pub use graph::{DialectNode, OptEdge, OptNode, TypeState};

pub type OptGraph = Rvsdg<OptNode, OptEdge>;

///The _whole_ optimizer. Mostly ties together the RVSDG and some auxiliary structures that
/// make wiring the the correct nodes together possible.
pub struct Optimizer {
    pub graph: OptGraph,

    ///All known concept definitions keyed by their name
    //NOTE: using the name, since thats how we reference them all the time.
    pub(crate) concepts: AHashMap<String, CSGConcept>,
    ///All known entity and operation defs
    pub(crate) csg_node_defs: AHashMap<String, CSGNodeDef>,

    ///lookup table for the λ-Nodes of entity implementation of concepts
    pub(crate) concept_impl: AHashMap<ConceptImplKey, ConceptImpl>,

    ///Lookup table for all λ-Nodes that are export_fn.
    pub(crate) export_fn: AHashMap<String, ExportFn>,

    ///Lookup table for all field-defs
    pub(crate) field_def: AHashMap<String, FieldDef>,

    ///Lookup table for all alge functions.
    pub(crate) alge_fn: AHashMap<String, AlgeFn>,

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
            export_fn: AHashMap::default(),
            field_def: AHashMap::default(),
            alge_fn: AHashMap::default(),
            typemap: FlagStore::new(),
            span_tags: FlagStore::new(),
            names: FlagStore::new(),
            var_producer: FlagStore::new(),
            #[cfg(feature = "viewer")]
            viewer_state: rvsdg_viewer::ViewerState::new(),
            config: Config::default(),
        }
    }

    ///Adds a [VolaAst] to the optimizer. Might emit errors if the
    /// semantic analysis fails immediately while adding.
    pub fn add_ast(&mut self, ast: VolaAst) -> Result<(), OptError> {
        //NOTE we first add all def nodes, since those don't depend on anything else, and without those
        // some of the other nodes might not build, even though they could.
        //
        // After that we add all impl-blocks, since they only depend on the defs,
        // then all field-def, since they need the impl-blocks, and last are all exportfn.

        #[cfg(feature = "profile")]
        let ast_add_start = std::time::Instant::now();

        #[cfg(feature = "log")]
        log::info!("Adding Ast to Optimizer");

        let mut errors = Vec::with_capacity(0);

        //NOTE yes collecting into a big'ol Vec all the time is kinda wasteful, but since
        // we use `self` in the filter, we can't just connect multiple filter_maps :O .

        //concept loop
        let sans_defs = ast
            .entries
            .into_iter()
            .filter_map(|ast_entry| {
                if ast_entry.entry.is_def_node() {
                    //Early add def
                    if let Err(e) = self.add_tl_node(ast_entry) {
                        errors.push(e);
                    }
                    None
                } else {
                    Some(ast_entry)
                }
            })
            .collect::<Vec<_>>();

        //algefunction loop
        let sans_alge_fn = sans_defs
            .into_iter()
            .filter_map(|ast_entry| {
                if ast_entry.entry.is_alge_fn() {
                    //Early add def
                    if let Err(e) = self.add_tl_node(ast_entry) {
                        errors.push(e)
                    }
                    None
                } else {
                    Some(ast_entry)
                }
            })
            .collect::<Vec<_>>();

        //implblock loop
        let sans_impl_block = sans_alge_fn
            .into_iter()
            .filter_map(|ast_entry| {
                if ast_entry.entry.is_impl_block() {
                    //Early add def
                    if let Err(e) = self.add_tl_node(ast_entry) {
                        errors.push(e);
                    }
                    None
                } else {
                    Some(ast_entry)
                }
            })
            .collect::<Vec<_>>();

        //fielddefs
        let sans_fielddef = sans_impl_block
            .into_iter()
            .filter_map(|ast_entry| {
                if ast_entry.entry.is_field_def() {
                    //Early add def
                    if let Err(e) = self.add_tl_node(ast_entry) {
                        errors.push(e);
                    }
                    None
                } else {
                    Some(ast_entry)
                }
            })
            .collect::<Vec<_>>();

        //now _the_rest_ which _should_ be only export_fn

        for tl in sans_fielddef {
            if !tl.entry.is_exportfn() {
                println!("warning: found non-field-def in last top-level-node iterator");
            }
            if let Err(e) = self.add_tl_node(tl) {
                errors.push(e);
            }
        }

        #[cfg(feature = "profile")]
        println!(
            "Adding AST took {}ms / {}ns",
            ast_add_start.elapsed().as_millis(),
            ast_add_start.elapsed().as_nanos()
        );

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_AST").is_ok() {
            self.push_debug_state("AST to Opt");
        }

        if errors.len() > 0 {
            Err(OptError::ErrorsOccurred(errors.len()))
        } else {
            Ok(())
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
            ignore_dead_node: true,
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

    #[cfg(feature = "viewer")]
    pub fn dump_debug_state(&self, path: &dyn AsRef<std::path::Path>) {
        println!("Writing debug state to {:?}", path.as_ref());

        if std::env::var("VOLA_DUMP_SVG").is_ok() {
            let mut svg_path = path.as_ref().to_path_buf();
            svg_path.set_extension("svg");
            self.dump_svg(svg_path.to_str().unwrap(), false);
        }
        self.viewer_state.write_to_file(path)
    }
}
