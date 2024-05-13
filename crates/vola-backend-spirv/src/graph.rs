/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Defines the spirv-backend graph nodes and edges as well as the opt->spv RVSDG
//! rewriter.

use std::fmt::Debug;

use rvsdg::{
    edge::LangEdge,
    nodes::{LangNode, NodeType},
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::SmallVec,
    NodeRef,
};
use rvsdg_viewer::View;

use crate::{
    hl::HlOp,
    spv::{CoreOp, SpvOp, SpvType},
    SpirvBackend,
};

pub enum BackendOp {
    SpirvOp(SpvOp),
    HlOp(HlOp),
    Dummy,
}

impl BackendOp {
    pub fn unwrap_spv_ref(&self) -> &SpvOp {
        if let BackendOp::SpirvOp(spv) = self {
            spv
        } else {
            panic!("Was not a SPIRV backend op!");
        }
    }
}

///The backend graph is characterised by SSA-like multi-input, single-output nodes.
#[derive(LangNode)]
pub struct BackendNode {
    #[inputs]
    pub inputs: SmallVec<[Input; 3]>,
    #[output]
    pub output: Output,

    pub(crate) op: BackendOp,
}

impl Debug for BackendNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.op {
            BackendOp::SpirvOp(o) => write!(f, "SpirvOp({})", o.name()),
            BackendOp::HlOp(hl) => write!(f, "HlOp({:?})", hl),
            BackendOp::Dummy => write!(f, "Dummy"),
        }
    }
}

impl View for BackendNode {
    fn name(&self) -> String {
        match &self.op {
            BackendOp::SpirvOp(o) => o.name(),
            BackendOp::HlOp(o) => format!("{o:?}"),
            BackendOp::Dummy => "Dummy".to_owned(),
        }
    }
    fn color(&self) -> rvsdg_viewer::Color {
        match self.op {
            BackendOp::SpirvOp(_) => rvsdg_viewer::Color::from_rgba(200, 180, 150, 255),
            BackendOp::HlOp(_) => rvsdg_viewer::Color::from_rgba(150, 150, 200, 255),
            BackendOp::Dummy => rvsdg_viewer::Color::from_rgba(250, 150, 150, 255),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BackendEdge {
    Value(SpvType),
    State,
}

impl BackendEdge {
    pub fn get_type(&self) -> Option<&SpvType> {
        if let Self::Value(ty) = self {
            Some(ty)
        } else {
            None
        }
    }

    ///Sets the type of `self` to `ty`, if this is a value edge.
    /// Returns the type that was set before, if there is any.
    pub fn set_type(&mut self, ty: SpvType) -> Option<SpvType> {
        match self {
            Self::Value(v) => {
                let mut ty = ty;
                std::mem::swap(v, &mut ty);
                Some(ty)
            }
            Self::State => None,
        }
    }
}

impl View for BackendEdge {
    fn name(&self) -> String {
        match self {
            Self::Value(ty) => format!("Value<{:?}>", ty),
            Self::State => "State".to_owned(),
        }
    }
    fn color(&self) -> rvsdg_viewer::Color {
        match self {
            Self::Value(ty) => match ty {
                SpvType::Undefined => rvsdg_viewer::Color::from_rgba(255, 0, 255, 255),
                SpvType::Arith(_) => rvsdg_viewer::Color::from_rgba(20, 20, 20, 255),
                _ => rvsdg_viewer::Color::from_rgba(0, 255, 0, 255),
            },
            Self::State => rvsdg_viewer::Color::from_rgba(200, 0, 0, 255),
        }
    }
}

impl LangEdge for BackendEdge {
    fn value_edge() -> Self {
        Self::Value(SpvType::undefined())
    }
    fn state_edge() -> Self {
        Self::State
    }

    fn is_value_edge(&self) -> bool {
        if let Self::Value(_) = self {
            true
        } else {
            false
        }
    }
    fn is_state_edge(&self) -> bool {
        if let Self::State = self {
            true
        } else {
            false
        }
    }
}

impl SpirvBackend {
    ///Returns the _single_ return type of `node`, assuming its a _simple-node_ or a apply node.
    ///Returns None, if `node` is not a _simple-node_.
    pub fn get_single_node_result_type(&self, node: NodeRef) -> Option<SpvType> {
        {
            let node = &self.graph.node(node).node_type;
            if node.is_lambda() || node.is_delta() || node.is_phi() {
                return None;
            }
        }

        assert!(self.graph.node(node).outputs().len() == 1);
        let mut unified_type = None;
        for edg in &self.graph.node(node).outputs()[0].edges {
            let edgty = self
                .graph
                .edge(*edg)
                .ty
                .get_type()
                .cloned()
                .expect("Expected edge to be type set!");
            if let Some(set_type) = &unified_type {
                assert!(
                    edgty == *set_type,
                    "had different edge types connected to outport of simple node!"
                )
            } else {
                unified_type = Some(edgty)
            }
        }

        unified_type
    }

    ///Overrides the output-type on the edges connected to `node`
    pub fn set_simple_note_output_type(&mut self, node: NodeRef, ty: SpvType) {
        if let NodeType::Simple(s) = &self.graph.node(node).node_type {
            assert!(s.outputs().len() == 1);
            for edg in &s.outputs()[0].edges.clone() {
                self.graph.edge_mut(*edg).ty.set_type(ty.clone());
            }
        } else {
            panic!("Was not a simple node")
        }
    }

    pub fn is_core_op(&self, node: NodeRef, op: CoreOp) -> bool {
        if let NodeType::Simple(s) = &self.graph.node(node).node_type {
            if let BackendOp::SpirvOp(SpvOp::CoreOp(c)) = s.op {
                if c == op {
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        }
    }
}
