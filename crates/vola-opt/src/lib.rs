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

use rvsdg::{edge::LangEdge, nodes::LangNode, Rvsdg};

use rvsdg_viewer::View;
///Type in the vola type-system.
pub use vola_ast::common::Ty;
use vola_common::Span;

mod alge;
mod common;
mod csg;

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

pub struct Optimizer {
    graph: Rvsdg<OptNode, OptEdge>,
}
