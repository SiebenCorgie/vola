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
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::SmallVec,
};
use rvsdg_viewer::View;

use crate::spv::{SpvNode, SpvType};

pub enum BackendOp {
    SpirvOp(SpvNode),
    Dummy,
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
            BackendOp::Dummy => write!(f, "Dummy"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BackendEdge {
    Value(SpvType),
    State,
}

impl View for BackendNode {
    fn name(&self) -> String {
        match &self.op {
            BackendOp::SpirvOp(o) => o.name(),
            BackendOp::Dummy => "Dummy".to_owned(),
        }
    }
    fn color(&self) -> rvsdg_viewer::Color {
        rvsdg_viewer::Color::from_rgba(200, 180, 150, 255)
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
            Self::Value(_) => rvsdg_viewer::Color::from_rgba(0, 0, 0, 255),
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
