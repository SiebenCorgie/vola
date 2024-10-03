/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::fmt::Display;

use rvsdg::{
    edge::LangEdge,
    nodes::LangNode,
    region::{Inport, Input, Outport, Output},
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

mod utils;

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
        Self::error_for_sig(opt.inputs().len(), opt.outputs().len())
    }

    pub fn error_for_sig(inputs: usize, outputs: usize) -> Self {
        Self::Error {
            inputs: smallvec![Inport::default(); inputs],
            outputs: smallvec![Outport::default(); outputs],
        }
    }

    pub fn try_from_opt(
        value: &OptNode,
        input_sig: &[Option<vola_opt::common::Ty>],
        output_sig: &[Option<vola_opt::common::Ty>],
    ) -> Result<Self, WasmError> {
        //Our wasm backend supports the whole alge dialect, and all immediate values.
        if let Some(imm) = value.try_downcast_ref::<ImmNat>() {
            assert!(input_sig.len() == 0);
            assert!(output_sig.len() == 1);
            assert!(output_sig[0] == Some(vola_opt::common::Ty::Nat));
            return Ok(WasmNode::from(imm));
        }
        if let Some(imm) = value.try_downcast_ref::<ImmScalar>() {
            assert!(input_sig.len() == 0);
            assert!(output_sig.len() == 1);
            assert!(output_sig[0] == Some(vola_opt::common::Ty::Scalar));
            return Ok(WasmNode::from(imm));
        }

        if let Some(binop) = value.try_downcast_ref::<BinaryArith>() {
            assert!(input_sig.len() == 2);
            assert!(input_sig[0].is_some() && input_sig[1].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());
            return Ok(WasmNode::try_from_opt_binary(
                binop,
                [
                    input_sig[0].as_ref().unwrap().clone(),
                    input_sig[1].as_ref().unwrap().clone(),
                ],
                output_sig[0].as_ref().unwrap().clone(),
            ));
        }
        if let Some(binop) = value.try_downcast_ref::<BinaryRel>() {
            assert!(input_sig.len() == 2);
            assert!(input_sig[0].is_some() && input_sig[1].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());
            return Ok(WasmNode::try_from_op_binaryrel(
                binop,
                [
                    input_sig[0].as_ref().unwrap().clone(),
                    input_sig[1].as_ref().unwrap().clone(),
                ],
                output_sig[0].as_ref().unwrap().clone(),
            ));
        }
        if let Some(binop) = value.try_downcast_ref::<BinaryBool>() {
            assert!(input_sig.len() == 2);
            assert!(input_sig[0].is_some() && input_sig[1].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());
            return Ok(WasmNode::try_from_binary_bool(
                binop,
                [
                    input_sig[0].as_ref().unwrap().clone(),
                    input_sig[1].as_ref().unwrap().clone(),
                ],
                output_sig[0].as_ref().unwrap().clone(),
            ));
        }

        if let Some(unop) = value.try_downcast_ref::<UnaryArith>() {
            assert!(input_sig.len() == 1);
            assert!(input_sig[0].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());
            return Ok(WasmNode::try_from_unary_arith(
                unop,
                input_sig[0].as_ref().unwrap().clone(),
                output_sig[0].as_ref().unwrap().clone(),
            ));
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

#[derive(Debug, Clone, PartialEq)]
pub enum WasmTy {
    Defined { shape: TyShape, ty: walrus::ValType },
    Undefined,
}

impl From<vola_opt::common::Ty> for WasmTy {
    fn from(value: vola_opt::common::Ty) -> Self {
        let (shape, ty) = match value {
            vola_opt::common::Ty::Bool => (TyShape::Scalar, walrus::ValType::I32),
            vola_opt::common::Ty::Nat => (TyShape::Scalar, walrus::ValType::I32),
            vola_opt::common::Ty::Scalar => (TyShape::Scalar, walrus::ValType::F32),
            vola_opt::common::Ty::Vector { width } => {
                (TyShape::Vector { width }, walrus::ValType::F32)
            }
            vola_opt::common::Ty::Matrix { width, height } => {
                (TyShape::Matrix { width, height }, walrus::ValType::F32)
            }
            vola_opt::common::Ty::Tensor { dim } => (
                TyShape::Tensor {
                    dim: dim.into_iter().collect(),
                },
                walrus::ValType::F32,
            ),
            _ => return WasmTy::Undefined,
        };

        WasmTy::Defined { shape, ty }
    }
}

impl WasmTy {
    pub fn is_vector(&self) -> bool {
        if let Self::Defined {
            shape: TyShape::Vector { .. },
            ..
        } = self
        {
            true
        } else {
            false
        }
    }
    pub fn new_with_shape(ty: walrus::ValType, shape: TyShape) -> Self {
        Self::Defined { shape, ty }
    }

    pub fn element_count(&self) -> usize {
        match self {
            Self::Defined { shape, ty } => shape.element_count(),
            Self::Undefined => 0,
        }
    }

    pub fn unwarp_walrus_ty(&self) -> walrus::ValType {
        if let Self::Defined { shape: _, ty } = self {
            ty.clone()
        } else {
            panic!("Type was undefined!")
        }
    }

    pub fn append_elements_to_signature(&self, signature: &mut SmallColl<walrus::ValType>) {
        match self {
            Self::Undefined => {}
            Self::Defined { shape, ty } => {
                for _ in 0..shape.element_count() {
                    signature.push(ty.clone());
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyShape {
    Scalar,
    Vector { width: usize },
    Matrix { width: usize, height: usize },
    Tensor { dim: SmallColl<usize> },
}

impl TyShape {
    pub fn element_count(&self) -> usize {
        match self {
            Self::Scalar => 1,
            Self::Vector { width } => *width,
            Self::Matrix { width, height } => width * height,
            Self::Tensor { dim } => dim.iter().fold(1, |x, y| x * y),
        }
    }
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

#[derive(Debug, Clone)]
pub enum WasmEdge {
    State,
    Value(WasmTy),
}

impl WasmEdge {
    pub fn type_or_undefined(&self) -> WasmTy {
        match self {
            Self::State => WasmTy::Undefined,
            Self::Value(t) => t.clone(),
        }
    }
}

impl LangEdge for WasmEdge {
    fn state_edge() -> Self {
        Self::State
    }

    fn value_edge() -> Self {
        Self::Value(WasmTy::Undefined)
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
            Self::Value(WasmTy::Defined { shape, ty }) => format!("{shape}<{ty:?}>"),
            Self::Value(WasmTy::Undefined) => format!("Undefined"),
        }
    }

    fn color(&self) -> rvsdg_viewer::Color {
        match self {
            Self::State => rvsdg_viewer::Color::RED,
            Self::Value { .. } => rvsdg_viewer::Color::BLACK,
        }
    }
}
