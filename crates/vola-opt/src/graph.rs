/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Module that definse the OptNode and OptEdge related structures.

use std::{any::Any, fmt::Debug};

use crate::common::Ty;
use crate::error::OptError;
use ahash::AHashMap;
use rvsdg::{
    attrib::AttribStore, edge::LangEdge, nodes::LangNode, rvsdg_derive_lang::LangNode,
    util::copy::StructuralClone,
};

use rvsdg_viewer::View;
use vola_ast::csg::{CSGConcept, CSGNodeDef};
use vola_common::Span;

use crate::OptGraph;

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

    ///Tries to downcast the inner `node` to `T`.
    pub fn try_downcast_ref<'a, T: DialectNode + Send + Sync + 'static>(&'a self) -> Option<&'a T> {
        let upcast: &'a dyn Any = self.node.as_ref();
        upcast.downcast_ref()
    }
    ///Tries to downcast the inner `node` to `T`.
    pub fn try_downcast_mut<'a, T: DialectNode + Send + Sync + 'static>(
        &'a mut self,
    ) -> Option<&'a mut T> {
        let upcast: &'a mut dyn Any = self.node.as_mut();
        upcast.downcast_mut()
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

#[derive(Clone, Debug, PartialEq)]
pub enum TypeState {
    Set(Ty),
    Derived(Ty),
    Unset,
}

impl TypeState {
    pub fn get_type(&self) -> Option<Ty> {
        match self {
            Self::Set(t) | Self::Derived(t) => Some(t.clone()),
            Self::Unset => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
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
