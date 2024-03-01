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

use std::{any::Any, fmt::Debug, ops::Deref};

use ahash::AHashMap;
use alge::implblock::{ConceptImpl, ConceptImplKey};
use common::Ty;
use csg::{exportfn::ExportFn, fielddef::FieldDef};
use error::OptError;
use rvsdg::{
    attrib::AttribStore, edge::LangEdge, nodes::LangNode, rvsdg_derive_lang::LangNode,
    util::copy::StructuralClone, Rvsdg,
};

use rvsdg_viewer::{layout::LayoutConfig, View};
use vola_ast::{
    csg::{CSGConcept, CSGNodeDef},
    VolaAst,
};
use vola_common::Span;

mod alge;
mod ast;
mod common;
mod csg;
mod error;
mod passes;

pub type OptGraph = Rvsdg<OptNode, OptEdge>;

///A node of some dialect
pub trait DialectNode: LangNode + Any + View {
    ///Dialect identifier of this node.
    fn dialect(&self) -> &'static str;

    ///When presented with the given type map and graph, lets the implementation choose a type, if possible.
    ///
    /// Can return an error if an invalid configuration is detected. If the configuration is just incomplete, should return
    /// Ok(None). In that case the type resolution will try again later.
    fn try_derive_type(
        &self,
        _typemap: &AttribStore<Ty>,
        _graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        Err(OptError::Any {
            text: format!("Type resolution not implemented for {}", self.name()),
        })
    }

    ///Builds a structural copy of this node, where no inputs/outputs are connected.
    /// Needed to break up the `dyn` indirection in OptNode.
    fn structural_copy(&self, span: Span) -> OptNode;
}

///Single optimizer node of some dialect.4
#[derive(LangNode)]
pub struct OptNode {
    ///The source span this node originated from
    pub span: Span,
    ///The inner node that is being represented
    #[expose]
    pub node: Box<dyn DialectNode + Send + Sync + 'static>,
}

impl OptNode {
    pub fn new(node: impl DialectNode + Send + Sync + 'static, span: Span) -> Self {
        OptNode {
            span,
            node: Box::new(node),
        }
    }
}

impl StructuralClone for OptNode {
    fn structural_copy(&self) -> Self {
        self.node.structural_copy(self.span.clone())
    }
}

impl StructuralClone for OptEdge {
    fn structural_copy(&self) -> Self {
        self.clone()
    }
}

impl View for OptNode {
    fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
        self.node.color()
    }
    fn name(&self) -> String {
        self.node.name()
    }
    fn stroke(&self) -> rvsdg_viewer::Stroke {
        self.node.stroke()
    }
}

impl Debug for OptNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} . {}", self.node.dialect(), self.node.name())
    }
}

#[derive(Clone, Debug)]
pub enum TypeState {
    Set(Ty),
    Derived(Ty),
    Unset,
}

#[derive(Clone, Debug)]
pub enum OptEdge {
    State,
    Value { ty: TypeState },
}

impl OptEdge {
    ///Sets an unset edge to an derived edge. Does nothing if the state is already
    /// `Set`.
    ///
    /// Panics if the state is already derived.
    //TODO: Make that nicer. Shouldn't happen thought. However, better panic then creating invalid state.
    pub fn set_derived_state(&mut self, ts: Ty) {
        match self {
            OptEdge::State => panic!("Cannot set type on state edge"),
            OptEdge::Value { ty } => match ty {
                TypeState::Unset => *ty = TypeState::Derived(ts),
                TypeState::Derived(t) => {
                    panic!("Type state was already derived as {t:?}, cannot overwrite as {ts:?}")
                }
                TypeState::Set(_) => {}
            },
        }
    }

    ///Returns the type of this edge, if possible. Is none for State edges and untyped value edges.
    pub fn get_type(&self) -> Option<&Ty> {
        match &self {
            OptEdge::State
            | OptEdge::Value {
                ty: TypeState::Unset,
            } => None,
            OptEdge::Value {
                ty: TypeState::Derived(t) | TypeState::Set(t),
            } => Some(t),
        }
    }
}

impl LangEdge for OptEdge {
    fn state_edge() -> Self {
        Self::State
    }
    fn value_edge() -> Self {
        Self::Value {
            ty: TypeState::Unset,
        }
    }
    fn is_state_edge(&self) -> bool {
        if let Self::State = self {
            true
        } else {
            false
        }
    }

    fn is_value_edge(&self) -> bool {
        if let Self::Value { .. } = self {
            true
        } else {
            false
        }
    }
}

impl View for OptEdge {
    fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
        match self {
            Self::State => rvsdg_viewer::macroquad::color::Color::from_rgba(255, 0, 0, 255),
            Self::Value { ty } => match ty {
                TypeState::Unset => rvsdg_viewer::macroquad::color::Color::from_rgba(0, 0, 0, 255),
                TypeState::Set(_) => {
                    rvsdg_viewer::macroquad::color::Color::from_rgba(0, 255, 0, 255)
                }
                TypeState::Derived(_) => {
                    rvsdg_viewer::macroquad::color::Color::from_rgba(0, 0, 255, 255)
                }
            },
        }
    }
    fn name(&self) -> String {
        match self {
            Self::State => "State".to_owned(),
            Self::Value { ty } => match ty {
                TypeState::Derived(t) => format!("Value(derived {:?})", t),
                TypeState::Set(t) => format!("Value(set {:?})", t),
                TypeState::Unset => format!("Value(unset)"),
            },
        }
    }
    fn stroke(&self) -> rvsdg_viewer::Stroke {
        match self {
            Self::State => rvsdg_viewer::Stroke::Line,
            Self::Value { .. } => rvsdg_viewer::Stroke::Line,
        }
    }
}

///The _whole_ optimizer. Mostly ties together the RVSDG and some auxiliary structures that
/// make wiring the the correct nodes together possible.
pub struct Optimizer {
    pub(crate) graph: OptGraph,

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

    ///All known type tags of ports and nodes. Can be used to do type checking, or infer edge types.
    pub(crate) typemap: AttribStore<Ty>,
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
            typemap: AttribStore::new(),
        }
    }

    ///Adds a [VolaAst](vola_ast::VolaAst) to the optimizer. Might emit errors if the
    /// semantic analysis fails immediately while adding.
    pub fn add_ast(&mut self, ast: VolaAst) -> Result<(), OptError> {
        //NOTE we first add all def nodes, since those don't depend on anything else, and without those
        // some of the other nodes might not build, even though they could.
        //
        // After that we add all impl-blocks, since they only depend on the defs,
        // then all field-def, since they need the impl-blocks, and last are all exportfn.

        #[cfg(feature = "profile")]
        let ast_add_start = std::time::Instant::now();

        let mut error_counter = 0;

        //NOTE yes collecting into a big'ol Vec all the time is kinda wasteful, but since
        // we use `self` in the filter, we can't just connect multiple filter_maps :O .

        //concept loop
        let sans_defs = ast
            .entries
            .into_iter()
            .filter_map(|ast_entry| {
                if ast_entry.entry.is_def_node() {
                    //Early add def
                    if let Err(_e) = self.add_tl_node(ast_entry) {
                        error_counter += 1;
                    }
                    None
                } else {
                    Some(ast_entry)
                }
            })
            .collect::<Vec<_>>();

        //implblock loop
        let sans_impl_block = sans_defs
            .into_iter()
            .filter_map(|ast_entry| {
                if ast_entry.entry.is_impl_block() {
                    //Early add def
                    if let Err(_e) = self.add_tl_node(ast_entry) {
                        error_counter += 1;
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
                    if let Err(_e) = self.add_tl_node(ast_entry) {
                        error_counter += 1;
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
            if let Err(_e) = self.add_tl_node(tl) {
                error_counter += 1;
            }
        }

        #[cfg(feature = "profile")]
        println!(
            "Adding AST took {}ms / {}ns",
            ast_add_start.elapsed().as_millis(),
            ast_add_start.elapsed().as_nanos()
        );

        if error_counter > 0 {
            Err(OptError::ErrorsOccurred(error_counter))
        } else {
            Ok(())
        }
    }

    pub fn dump_svg(&self, name: &str) {
        println!("Found {} type-tags", self.typemap.attribs.len());

        let conf = LayoutConfig {
            grid_padding: 30,
            grid_empty_spacing: 15,
            ignore_dead_node: true,
            ..Default::default()
        };
        rvsdg_viewer::into_svg_with_config(&self.graph, name, &conf)
    }
}
