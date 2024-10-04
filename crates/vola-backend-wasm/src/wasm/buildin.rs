/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # WASM dialect

use rvsdg::{
    region::{Inport, Outport},
    rvsdg_derive_lang::LangNode,
    smallvec::smallvec,
    SmallColl,
};
use vola_opt::alge::buildin::{Buildin, BuildinOp};

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
