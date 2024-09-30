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
        logical::{BinaryBool, BinaryBoolOp, UnaryBool, UnaryBoolOp},
        matrix::{UnaryMatrix, UnaryMatrixOp},
        relational::{BinaryRel, BinaryRelOp},
        trigonometric::{Trig, TrigOp},
    },
    imm::{ImmNat, ImmScalar},
};

use crate::{
    graph::{WasmNode, WasmTy},
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
}

impl ExternOp {
    pub fn get_static_symbol_name(&self, input_types: &[WasmTy]) -> Result<String, WasmError> {
        match self {
            Self::Length => {
                if input_types.len() != 1 {
                    return Err(WasmError::RuntimeIncompatibleSig(
                        self.clone(),
                        input_types.len(),
                    ));
                }

                match (
                    input_types[0].element_count(),
                    input_types[0].unwarp_walrus_ty(),
                ) {
                    (3, walrus::ValType::F32) => Ok("length_vec3".to_string()),
                    (_count, _ty) => Err(WasmError::RuntimeIncompatibleType(
                        self.clone(),
                        0,
                        input_types[0].clone(),
                    )),
                }
            }

            _ => panic!("Extern Op {self:?} not implemented"),
        }
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
            op: walrus::ir::Value::I64(value.lit.try_into().unwrap()),
            output: Outport::default(),
        })
    }
}

impl From<&ImmScalar> for WasmNode {
    fn from(value: &ImmScalar) -> Self {
        WasmNode::Value(WasmValue {
            //NOTE the u64 should not have the highest bit set...
            op: walrus::ir::Value::F64(value.lit.try_into().unwrap()),
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

impl From<&BinaryArith> for WasmNode {
    fn from(value: &BinaryArith) -> Self {
        match value.op {
            BinaryArithOp::Add => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Add)),
            BinaryArithOp::Sub => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Sub)),
            BinaryArithOp::Mul => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Mul)),
            BinaryArithOp::Div => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Div)),
            BinaryArithOp::Mod => {
                WasmNode::Runtime(WasmRuntimeOp::new_with_signature(2, ExternOp::Mod))
            }
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
