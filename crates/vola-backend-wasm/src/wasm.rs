/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # WASM dialect

use rvsdg::{
    region::{Inport, Outport, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::smallvec,
    SmallColl,
};
use vola_opt::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp},
        buildin::{Buildin, BuildinOp},
        logical::{BinaryBool, BinaryBoolOp, UnaryBool},
        matrix::{UnaryMatrix, UnaryMatrixOp},
        relational::{BinaryRel, BinaryRelOp},
        trigonometric::{Trig, TrigOp},
    },
    imm::{ImmNat, ImmScalar},
};
use walrus::ValType;

use crate::{
    graph::{TyShape, WasmNode, WasmTy},
    WasmError,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExternOp {
    Cross,
    Dot,
    Length,
    MatrixInverse,

    Sin,

    Mod,

    Fract,

    //Emulation of piece wise vector ops
    //Vec2
    AddVec,
    SubVec,
    MulVec,
    DivVec,
    ModVec,
}

impl ExternOp {
    pub fn get_static_symbol_name(&self, input_types: &[WasmTy]) -> Result<String, WasmError> {
        let (ty_suffix, symbol_name) = match self {
            Self::Length => {
                if input_types.len() != 1 || !input_types[0].is_vector() {
                    return Err(WasmError::RuntimeIncompatibleSig(
                        self.clone(),
                        input_types.len(),
                    ));
                }

                let element_count = input_types[0].element_count();
                let waltype = input_types[0].unwarp_walrus_ty();

                if element_count > 4 || element_count == 1 || waltype != ValType::F32 {
                    return Err(WasmError::RuntimeIncompatibleType(
                        self.clone(),
                        0,
                        input_types[0].clone(),
                    ));
                }

                (format!("vec{}", element_count), "length")
            }

            _ => panic!("Extern Op {self:?} not implemented"),
        };

        Ok(format!("{}_{}", symbol_name, ty_suffix))
    }
}

#[derive(LangNode)]
pub struct WasmRuntimeOp {
    #[inputs]
    inputs: SmallColl<Inport>,
    pub op: ExternOp,
    #[output]
    output: Outport,
}

impl WasmRuntimeOp {
    pub fn new_with_signature(inputs: usize, op: ExternOp) -> Self {
        Self {
            inputs: smallvec![Inport::default(); inputs],
            output: Outport::default(),
            op,
        }
    }
}

impl From<&UnaryMatrix> for WasmNode {
    fn from(value: &UnaryMatrix) -> Self {
        match value.op {
            UnaryMatrixOp::Invert => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                1,
                ExternOp::MatrixInverse,
            )),
        }
    }
}

impl From<&Buildin> for WasmNode {
    fn from(value: &Buildin) -> Self {
        match &value.op {
            BuildinOp::Dot => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::Dot,
            )),
            BuildinOp::Cross => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::Cross,
            )),
            BuildinOp::Length => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::Length,
            )),
            _ => panic!("Unsupported buildin op {:?}", value.op),
        }
    }
}

impl From<&Trig> for WasmNode {
    fn from(value: &Trig) -> Self {
        match &value.op {
            TrigOp::Sin => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Sin)),
            _ => todo!("unimplemented trig op"),
        }
    }
}

#[derive(LangNode)]
pub struct WasmValue {
    pub op: walrus::ir::Value,
    #[output]
    output: Output,
}

impl From<&ImmNat> for WasmNode {
    fn from(value: &ImmNat) -> Self {
        WasmNode::Value(WasmValue {
            //NOTE the u64 should not have the highest bit set...
            op: walrus::ir::Value::I32(value.lit.try_into().unwrap()),
            output: Outport::default(),
        })
    }
}

impl From<&ImmScalar> for WasmNode {
    fn from(value: &ImmScalar) -> Self {
        WasmNode::Value(WasmValue {
            //NOTE the u64 should not have the highest bit set...
            op: walrus::ir::Value::F32(value.lit as f32),
            output: Outport::default(),
        })
    }
}

#[derive(LangNode)]
pub struct WasmBinaryOp {
    #[inputs]
    inputs: [Inport; 2],
    pub op: walrus::ir::BinaryOp,
    #[output]
    output: Outport,
}

impl WasmBinaryOp {
    pub fn new(op: walrus::ir::BinaryOp) -> Self {
        WasmBinaryOp {
            op,
            inputs: [Inport::default(), Inport::default()],
            output: Outport::default(),
        }
    }
}

impl WasmNode {
    pub fn try_from_opt_binary(
        value: &BinaryArith,
        input_sig: [vola_opt::common::Ty; 2],
        output_sig: vola_opt::common::Ty,
    ) -> Self {
        assert!(input_sig[0] == input_sig[1] && input_sig[0] == output_sig);

        let wasm_inout_ty = WasmTy::from(input_sig[0].clone());

        match wasm_inout_ty {
            //Scalar binary ops
            WasmTy::Defined {
                shape: TyShape::Scalar,
                ty,
            } => match ty {
                walrus::ValType::F32 => match value.op {
                    BinaryArithOp::Add => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Add))
                    }
                    BinaryArithOp::Sub => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Sub))
                    }
                    BinaryArithOp::Mul => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Mul))
                    }
                    BinaryArithOp::Div => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Div))
                    }
                    BinaryArithOp::Mod => {
                        WasmNode::Runtime(WasmRuntimeOp::new_with_signature(2, ExternOp::Mod))
                    }
                },
                walrus::ValType::I32 => match value.op {
                    BinaryArithOp::Add => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Add))
                    }
                    BinaryArithOp::Sub => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Sub))
                    }
                    BinaryArithOp::Mul => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Mul))
                    }
                    BinaryArithOp::Div => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Div))
                    }
                    BinaryArithOp::Mod => {
                        WasmNode::Runtime(WasmRuntimeOp::new_with_signature(2, ExternOp::Mod))
                    }
                },
                _ => WasmNode::error_for_sig(2, 1),
            },
            //Vector binary ops
            WasmTy::Defined {
                shape: TyShape::Vector { width: _ },
                ty,
            } => match ty {
                walrus::ValType::F32 => match value.op {
                    BinaryArithOp::Add => {
                        WasmNode::Runtime(WasmRuntimeOp::new_with_signature(2, ExternOp::AddVec))
                    }
                    BinaryArithOp::Sub => {
                        WasmNode::Runtime(WasmRuntimeOp::new_with_signature(2, ExternOp::SubVec))
                    }
                    BinaryArithOp::Mul => {
                        WasmNode::Runtime(WasmRuntimeOp::new_with_signature(2, ExternOp::MulVec))
                    }
                    BinaryArithOp::Div => {
                        WasmNode::Runtime(WasmRuntimeOp::new_with_signature(2, ExternOp::DivVec))
                    }
                    BinaryArithOp::Mod => {
                        WasmNode::Runtime(WasmRuntimeOp::new_with_signature(2, ExternOp::Mod))
                    }
                },
                _ => WasmNode::error_for_sig(2, 1),
            },

            _ => WasmNode::error_for_sig(2, 1),
        }
    }
}

impl From<&BinaryRel> for WasmNode {
    fn from(value: &BinaryRel) -> Self {
        match value.op {
            BinaryRelOp::Eq => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Eq)),
            BinaryRelOp::Gt => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Ge)),
            BinaryRelOp::Gte => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Gt)),
            BinaryRelOp::Lt => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Lt)),
            BinaryRelOp::Lte => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Le)),
            BinaryRelOp::NotEq => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Ne)),
        }
    }
}

impl From<&BinaryBool> for WasmNode {
    fn from(value: &BinaryBool) -> Self {
        match value.op {
            BinaryBoolOp::And => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32And)),
            BinaryBoolOp::Or => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32Or)),
        }
    }
}

#[derive(LangNode)]
pub struct WasmUnaryOp {
    #[input]
    inputs: Inport,
    pub op: walrus::ir::UnaryOp,
    #[output]
    output: Outport,
}

impl WasmUnaryOp {
    pub fn new(op: walrus::ir::UnaryOp) -> Self {
        WasmUnaryOp {
            inputs: Inport::default(),
            op,
            output: Outport::default(),
        }
    }
}

impl From<&UnaryArith> for WasmNode {
    fn from(value: &UnaryArith) -> Self {
        match value.op {
            UnaryArithOp::Abs => WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Abs)),
            UnaryArithOp::Ceil => WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Ceil)),
            UnaryArithOp::Floor => WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Floor)),
            UnaryArithOp::Neg => WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Neg)),
            UnaryArithOp::Round => WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Abs)),
            UnaryArithOp::Fract => {
                WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Fract))
            }
        }
    }
}

impl From<&UnaryBool> for WasmNode {
    fn from(value: &UnaryBool) -> Self {
        match value.op {
            _ => panic!("unimplemented unary op"),
        }
    }
}
