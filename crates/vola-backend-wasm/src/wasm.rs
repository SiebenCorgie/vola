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

use crate::{
    graph::{TyShape, WasmNode, WasmTy},
    WasmError,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExternOp {
    Dot,
    Cross,
    Length,

    //NOTE Native for f32, else extern
    SquareRoot,

    Exp,
    Pow,

    Abs,
    Ceil,
    Floor,
    Neg,
    Round,

    Min,
    Max,
    Mix,
    Clamp,

    MatrixInverse,

    //All Trigs
    Sin,
    Cos,
    Tan,
    ASin,
    ACos,
    ATan,

    Mod,
    Fract,

    //Emulation of piece wise vector ops
    //Vec2
    AddVec,
    SubVec,
    MulVec,
    DivVec,
}

impl ExternOp {
    pub fn base_symbol_name(&self) -> &'static str {
        match self {
            Self::Dot => "dot",
            Self::Cross => "cross",
            Self::Length => "length",
            Self::SquareRoot => "sqrt",
            Self::Exp => "exp",
            Self::Pow => "pow",
            Self::Min => "min",
            Self::Max => "max",
            Self::Mix => "mix",
            Self::Clamp => "clamp",
            Self::MatrixInverse => "inverse",

            Self::Sin => "sin",
            Self::Cos => "cos",
            Self::Tan => "tan",
            Self::ASin => "asin",
            Self::ACos => "acos",
            Self::ATan => "atan",

            Self::Mod => "mod",
            Self::Fract => "fract",
            Self::AddVec => "add",
            Self::SubVec => "sub",
            Self::MulVec => "mul",
            Self::DivVec => "div",
            _ => "unknown",
        }
    }

    pub fn get_static_symbol_name(&self, input_types: &[WasmTy]) -> Result<String, WasmError> {
        //NOTE: all external functions are suffixed by the first args's type.
        let ty_suffix = {
            if let WasmTy::Defined { shape, ty: _ } = &input_types[0] {
                match shape {
                    TyShape::Scalar => "scalar",
                    TyShape::Vector { width } => &format!("vec{}", width),
                    TyShape::Matrix { width, height } => match (width, height) {
                        (2, 2) => "mat2",
                        (3, 3) => "mat3",
                        (4, 4) => "mat4",
                        _ => {
                            return Err(WasmError::RuntimeIncompatibleSig(
                                self.clone(),
                                input_types.len(),
                            ))
                        }
                    },
                    //NOTE: we currently have no vector typed extern functions
                    _ => {
                        return Err(WasmError::RuntimeIncompatibleSig(
                            self.clone(),
                            input_types.len(),
                        ))
                    }
                }
            } else {
                return Err(WasmError::RuntimeIncompatibleSig(
                    self.clone(),
                    input_types.len(),
                ));
            }
        };

        let symbol_name = self.base_symbol_name();

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
            BuildinOp::SquareRoot => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::SquareRoot,
            )),
            BuildinOp::Exp => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::Exp,
            )),
            BuildinOp::Pow => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::Pow,
            )),
            BuildinOp::Min => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::Min,
            )),
            BuildinOp::Max => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::Max,
            )),
            BuildinOp::Mix => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::Mix,
            )),
            BuildinOp::Clamp => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                value.inputs.len(),
                ExternOp::Clamp,
            )),
        }
    }
}

impl From<&Trig> for WasmNode {
    fn from(value: &Trig) -> Self {
        match &value.op {
            TrigOp::Sin => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Sin)),
            TrigOp::Cos => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Cos)),
            TrigOp::Tan => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Tan)),

            TrigOp::ASin => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::ASin)),
            TrigOp::ACos => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::ACos)),
            TrigOp::ATan => WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::ATan)),
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

impl WasmNode {
    pub fn try_from_op_binaryrel(
        value: &BinaryRel,
        input_sig: [vola_opt::common::Ty; 2],
        output_sig: vola_opt::common::Ty,
    ) -> Self {
        //NOTE make sure both are of scalar shape, otherwise
        //abort
        if !input_sig[0].is_scalar() || input_sig[0].is_scalar() {
            return Self::error_for_sig(2, 1);
        }
        //Also, output must be a bool.
        if !output_sig.is_bool() {
            return Self::error_for_sig(2, 1);
        }
        //Must be of same input type.
        if input_sig[0] != input_sig[1] {
            return Self::error_for_sig(2, 1);
        }

        //now dispatch either to f32 rels, or i32 rels, depending on the type
        match input_sig[0] {
            //NOTE: we have _natural_ numbers, so only signed integers
            vola_opt::common::Ty::Nat => match value.op {
                BinaryRelOp::Eq => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32Eq)),
                BinaryRelOp::Gt => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32GeS))
                }
                BinaryRelOp::Gte => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32GtS))
                }
                BinaryRelOp::Lt => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32LtS))
                }
                BinaryRelOp::Lte => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32LeS))
                }
                BinaryRelOp::NotEq => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32Ne))
                }
            },
            vola_opt::common::Ty::Scalar => match value.op {
                BinaryRelOp::Eq => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Eq)),
                BinaryRelOp::Gt => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Ge)),
                BinaryRelOp::Gte => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Gt))
                }
                BinaryRelOp::Lt => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Lt)),
                BinaryRelOp::Lte => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Le))
                }
                BinaryRelOp::NotEq => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Ne))
                }
            },
            _ => Self::error_for_sig(2, 1),
        }
    }
}

impl WasmNode {
    pub fn try_from_binary_bool(
        value: &BinaryBool,
        input_sig: [vola_opt::common::Ty; 2],
        output_sig: vola_opt::common::Ty,
    ) -> Self {
        //NOTE we expect a bool input
        if !input_sig[0].is_bool() || !input_sig[1].is_bool() || !output_sig.is_bool() {
            return WasmNode::error_for_sig(2, 1);
        }

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

impl WasmNode {
    pub fn try_from_unary_arith(
        value: &UnaryArith,
        input_sig: vola_opt::common::Ty,
        output_sig: vola_opt::common::Ty,
    ) -> Self {
        if input_sig != output_sig {
            return WasmNode::error_for_sig(1, 1);
        }

        match input_sig {
            vola_opt::common::Ty::Nat => {
                //Not in WASM, and we don't (yet?) have implementations for those
                WasmNode::error_for_sig(1, 1)
            }
            //NOTE: We can dispatch those to _buildin_ ops mostly
            vola_opt::common::Ty::Scalar => match value.op {
                UnaryArithOp::Abs => WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Abs)),
                UnaryArithOp::Ceil => {
                    WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Ceil))
                }
                UnaryArithOp::Floor => {
                    WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Floor))
                }
                UnaryArithOp::Neg => WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Neg)),
                UnaryArithOp::Round => {
                    WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Abs))
                }
                UnaryArithOp::Fract => {
                    WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Fract))
                }
            },
            //For externs, we have runtime calls
            vola_opt::common::Ty::Vector { .. } => match value.op {
                UnaryArithOp::Abs => {
                    WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Abs))
                }
                UnaryArithOp::Ceil => {
                    WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Ceil))
                }
                UnaryArithOp::Floor => {
                    WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Floor))
                }
                UnaryArithOp::Neg => {
                    WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Neg))
                }
                UnaryArithOp::Round => {
                    WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Round))
                }
                UnaryArithOp::Fract => {
                    WasmNode::Runtime(WasmRuntimeOp::new_with_signature(1, ExternOp::Fract))
                }
            },
            _ => WasmNode::error_for_sig(1, 1),
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
