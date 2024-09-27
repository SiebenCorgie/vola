/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::{env::var_os, fmt::Display};

use rvsdg::{
    edge::LangEdge,
    nodes::LangNode,
    region::{Inport, Input, Outport, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::smallvec,
    SmallColl,
};
use rvsdg_viewer::View;
use vola_opt::{
    alge::{
        arithmetic::{BinaryArith, UnaryArith},
        buildin::Buildin,
        logical::{BinaryBool, UnaryBool},
        matrix::UnaryMatrix,
        relational::BinaryRel,
        trigonometric::Trig,
    },
    imm::{ImmMatrix, ImmNat, ImmScalar, ImmVector},
    OptNode,
};

use crate::{
    error::WasmError,
    wasm::{self, WasmBinaryOp, WasmRuntimeOp, WasmUnaryOp, WasmValue},
};

pub enum WasmNode {
    Unary(WasmUnaryOp),
    Binary(WasmBinaryOp),
    Runtime(WasmRuntimeOp),
    Value(WasmValue),
    Error {
        inputs: SmallColl<Input>,
        outputs: SmallColl<Output>,
    },
}

impl WasmNode {
    pub fn error_for_opt(opt: &OptNode) -> Self {
        Self::Error {
            inputs: smallvec![Inport::default(); opt.inputs().len()],
            outputs: smallvec![Outport::default(); opt.outputs().len()],
        }
    }
}

impl TryFrom<&OptNode> for WasmNode {
    type Error = WasmError;
    fn try_from(value: &OptNode) -> Result<Self, WasmError> {
        //Our wasm backend supports the whole alge dialect, and all immediate values.
        if let Some(imm) = value.try_downcast_ref::<ImmNat>() {
            return Ok(WasmNode::from(imm));
        }
        if let Some(imm) = value.try_downcast_ref::<ImmScalar>() {
            return Ok(WasmNode::from(imm));
        }

        if let Some(binop) = value.try_downcast_ref::<BinaryArith>() {
            return Ok(WasmNode::from(binop));
        }
        if let Some(binop) = value.try_downcast_ref::<BinaryRel>() {
            return Ok(WasmNode::from(binop));
        }
        if let Some(binop) = value.try_downcast_ref::<BinaryBool>() {
            return Ok(WasmNode::from(binop));
        }

        if let Some(unop) = value.try_downcast_ref::<UnaryArith>() {
            return Ok(WasmNode::from(unop));
        }
        if let Some(unop) = value.try_downcast_ref::<UnaryBool>() {
            return Ok(WasmNode::from(unop));
        }

        if let Some(unop) = value.try_downcast_ref::<Buildin>() {
            return Ok(WasmNode::from(unop));
        }
        if let Some(unop) = value.try_downcast_ref::<Trig>() {
            return Ok(WasmNode::from(unop));
        }
        if let Some(unop) = value.try_downcast_ref::<UnaryMatrix>() {
            return Ok(WasmNode::from(unop));
        }

        //This would hint that the expected ImmScalarizer was not executed.
        if value.try_downcast_ref::<ImmVector>().is_some()
            || value.try_downcast_ref::<ImmMatrix>().is_some()
        {
            return Err(WasmError::UnexpectedComposite);
        }

        Err(WasmError::UnsupportedNode(value.name().to_string()))
    }
}

impl LangNode for WasmNode {
    fn inputs(&self) -> &[rvsdg::region::Input] {
        match self {
            Self::Unary(u) => u.inputs(),
            Self::Binary(b) => b.inputs(),
            Self::Runtime(r) => r.inputs(),
            Self::Value(v) => v.inputs(),
            Self::Error { inputs, outputs: _ } => inputs,
        }
    }

    fn inputs_mut(&mut self) -> &mut [rvsdg::region::Input] {
        match self {
            Self::Unary(u) => u.inputs_mut(),
            Self::Binary(b) => b.inputs_mut(),
            Self::Runtime(r) => r.inputs_mut(),
            Self::Value(v) => v.inputs_mut(),
            Self::Error { inputs, outputs: _ } => inputs,
        }
    }

    fn outputs(&self) -> &[rvsdg::region::Output] {
        match self {
            Self::Unary(u) => u.outputs(),
            Self::Binary(b) => b.outputs(),
            Self::Runtime(r) => r.outputs(),
            Self::Value(v) => v.outputs(),
            Self::Error { inputs: _, outputs } => outputs,
        }
    }

    fn outputs_mut(&mut self) -> &mut [rvsdg::region::Output] {
        match self {
            Self::Unary(u) => u.outputs_mut(),
            Self::Binary(b) => b.outputs_mut(),
            Self::Runtime(r) => r.outputs_mut(),
            Self::Value(v) => v.outputs_mut(),
            Self::Error { inputs: _, outputs } => outputs,
        }
    }
}

impl View for WasmNode {
    fn name(&self) -> String {
        match self {
            Self::Unary(u) => format!("{:?}", u.op),
            Self::Binary(b) => format!("{:?}", b.op),
            Self::Runtime(r) => format!("{:?}", r.op),
            Self::Value(v) => format!("{:?}", v.op),
            Self::Error { .. } => format!("Error"),
        }
    }

    fn color(&self) -> rvsdg_viewer::Color {
        match self {
            Self::Unary(_u) => rvsdg_viewer::Color::from_rgba(255, 230, 220, 255),
            Self::Binary(_b) => rvsdg_viewer::Color::from_rgba(220, 255, 230, 255),
            Self::Runtime(_r) => rvsdg_viewer::Color::from_rgba(230, 220, 255, 255),
            Self::Value(_r) => rvsdg_viewer::Color::from_rgba(150, 220, 150, 255),
            Self::Error { .. } => rvsdg_viewer::Color::from_rgba(255, 150, 100, 255),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum WasmTy {
    I32,
    I64,
    F32,
    F64,
    Undefined,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyShape {
    Scalar,
    Vector { width: usize },
    Matrix { width: usize, height: usize },
    Tensor { dim: SmallColl<usize> },
}

impl Display for TyShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar => write!(f, "Scalar"),
            Self::Vector { width } => write!(f, "Vector{width}"),
            Self::Matrix { width, height } => write!(f, "Mat{width}x{height}"),
            Self::Tensor { dim } => write!(f, "Tensor[{dim:?}]"),
        }
    }
}

pub enum WasmEdge {
    State,
    Value { ty: WasmTy, shape: TyShape },
}

impl LangEdge for WasmEdge {
    fn state_edge() -> Self {
        Self::State
    }

    fn value_edge() -> Self {
        Self::Value {
            ty: WasmTy::Undefined,
            shape: TyShape::Scalar,
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

#[cfg(feature = "viewer")]
impl View for WasmEdge {
    fn name(&self) -> String {
        match self {
            Self::State => format!("State"),
            Self::Value { ty, shape } => format!("{shape}<{ty:?}>"),
        }
    }

    fn color(&self) -> rvsdg_viewer::Color {
        match self {
            Self::State => rvsdg_viewer::Color::RED,
            Self::Value { .. } => rvsdg_viewer::Color::BLACK,
        }
    }
}
