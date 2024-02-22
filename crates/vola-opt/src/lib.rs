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

use std::fmt::Debug;

use ahash::AHashMap;
use alge::implblock::{ConceptImpl, ConceptImplKey};
use common::Ty;
use error::OptError;
use rvsdg::{
    attrib::AttribStore, edge::LangEdge, nodes::LangNode, rvsdg_derive_lang::LangNode, Rvsdg,
};

use rvsdg_viewer::View;
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

///A node of some dialect
pub trait DialectNode: LangNode + View {
    ///Dialect identifier of this node.
    fn dialect(&self) -> &'static str;
}

///Single optimizer node of some dialect.
#[derive(LangNode)]
pub struct OptNode {
    ///The source span this node originated from
    pub span: Span,
    ///The inner node that is being represented
    #[expose]
    node: Box<dyn DialectNode + Send + Sync + 'static>,
}

impl OptNode {
    pub fn new(node: impl DialectNode + Send + Sync + 'static, span: Span) -> Self {
        OptNode {
            span,
            node: Box::new(node),
        }
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

pub enum TypeState {
    Set(Ty),
    Derived(Ty),
    Unset,
}
pub enum OptEdge {
    State,
    Value { ty: TypeState },
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

///The _whole_ optimizer. Mostly ties together the RVSDG and some auxiliary structures that
/// make wiring the the correct nodes together possible.
pub struct Optimizer {
    pub(crate) graph: Rvsdg<OptNode, OptEdge>,

    ///All known concept definitions keyed by their name
    //NOTE: using the name, since thats how we reference them all the time.
    pub(crate) concepts: AHashMap<String, CSGConcept>,
    ///All known entity and operation defs
    pub(crate) csg_node_defs: AHashMap<String, CSGNodeDef>,

    ///lookup table for the λ-Nodes of entity implementation of concepts
    pub(crate) concept_impl: AHashMap<ConceptImplKey, ConceptImpl>,

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
            typemap: AttribStore::new(),
        }
    }

    ///Adds a [VolaAst](vola_ast::VolaAst) to the optimizer. Might emit errors if the
    /// semantic analysis fails immediately while adding.
    ///
    /// Stops whenever an error occurs.
    pub fn add_ast(&mut self, ast: VolaAst) -> Result<(), OptError> {
        //NOTE we first add all def nodes, since those don't depend on anything else, and without those
        // some of the other nodes might not build, even though they could.

        let mut error_counter = 0;

        let heavy_entries = ast
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

        for tl in heavy_entries {
            if let Err(_e) = self.add_tl_node(tl) {
                error_counter += 1;
            }
        }

        if error_counter > 0 {
            Err(OptError::ErrorsOccurred(error_counter))
        } else {
            Ok(())
        }
    }
}
