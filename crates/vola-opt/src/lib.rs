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

use ahash::AHashMap;
use alge::{ConceptImpl, ConceptImplKey};
use error::OptError;
use rvsdg::{edge::LangEdge, nodes::LangNode, Rvsdg};

use rvsdg_viewer::View;
///Type in the vola type-system.
pub use vola_ast::common::Ty;
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
pub struct OptNode {
    ///The source span this node originated from
    pub span: Span,
    ///The inner node that is being represented
    node: Box<dyn DialectNode + Send + Sync + 'static>,
}

impl View for OptNode {
    fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
        self.node.color()
    }
    fn name(&self) -> &str {
        self.node.name()
    }
    fn stroke(&self) -> rvsdg_viewer::Stroke {
        self.node.stroke()
    }
}

impl LangNode for OptNode {
    fn inputs(&self) -> &[rvsdg::region::Input] {
        self.node.inputs()
    }
    fn inputs_mut(&mut self) -> &mut [rvsdg::region::Input] {
        self.node.inputs_mut()
    }
    fn outputs(&self) -> &[rvsdg::region::Output] {
        self.node.outputs()
    }
    fn outputs_mut(&mut self) -> &mut [rvsdg::region::Output] {
        self.node.outputs_mut()
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
}

impl Optimizer {
    pub fn new() -> Self {
        Optimizer {
            graph: Rvsdg::new(),
            concepts: AHashMap::default(),
            csg_node_defs: AHashMap::default(),
            concept_impl: AHashMap::default(),
        }
    }

    ///Adds a [VolaAst](vola_ast::VolaAst) to the optimizer. Might emit errors if the
    /// semantic analysis fails immediately while adding.
    ///
    /// Stops whenever an error occurs.
    pub fn add_ast(&mut self, ast: VolaAst) -> Result<(), OptError> {
        for tl in ast.entries {
            self.add_tl_node(tl)?;
        }

        Ok(())
    }
}
