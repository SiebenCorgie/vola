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
use walrus::ValType;

use crate::{
    graph::{TyShape, WasmNode, WasmTy},
    wasm::WasmUnaryOp,
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

    MulMat,
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

            Self::Abs => "abs",
            Self::Floor => "floor",
            Self::Neg => "neg",
            Self::Round => "round",

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
            other => {
                println!("{:?} not known", other);
                "unknown"
            }
        }
    }

    pub fn get_static_symbol_name(&self, input_types: &[WasmTy]) -> Result<String, WasmError> {
        //NOTE: all external functions are suffixed by the first args's type.
        //      the only outlier is Mul (so far), which is also defined for vecn_scalar. This i caught early

        if self == &Self::MulVec {
            if input_types[0].is_vector() && input_types[1].is_scalar() {
                match &input_types[0] {
                    WasmTy::Defined {
                        shape: TyShape::Vector { width },
                        ty: _,
                    } => match width {
                        2 => return Ok("mul_vec2_scalar".to_string()),
                        3 => return Ok("mul_vec3_scalar".to_string()),
                        4 => return Ok("mul_vec4_scalar".to_string()),
                        other => {
                            return Err(WasmError::UnsupportedNode(format!(
                                "MulVec is not defined for vector with width={}",
                                other
                            )))
                        }
                    },
                    _ => {
                        return Err(WasmError::UnsupportedNode(format!(
                            "MulVec is not defined for {:?} * {:?}",
                            input_types[0], input_types[1]
                        )));
                    }
                }
            }
        }

        if self == &Self::MulMat {
            let a = if let WasmTy::Defined { shape, ty } = &input_types[0] {
                (shape, ty)
            } else {
                return Err(WasmError::UndefinedType);
            };

            let b = if let WasmTy::Defined { shape, ty } = &input_types[1] {
                (shape, ty)
            } else {
                return Err(WasmError::UndefinedType);
            };

            match (a, b) {
                //MatScalar
                (
                    (
                        TyShape::Matrix {
                            width: 2,
                            height: 2,
                        },
                        ValType::F32,
                    ),
                    (TyShape::Scalar, ValType::F32),
                ) => return Ok("mul_mat2_scalar".to_string()),
                (
                    (
                        TyShape::Matrix {
                            width: 3,
                            height: 3,
                        },
                        ValType::F32,
                    ),
                    (TyShape::Scalar, ValType::F32),
                ) => return Ok("mul_mat3_scalar".to_string()),
                (
                    (
                        TyShape::Matrix {
                            width: 4,
                            height: 4,
                        },
                        ValType::F32,
                    ),
                    (TyShape::Scalar, ValType::F32),
                ) => return Ok("mul_mat4_scalar".to_string()),
                //MatVec
                //2
                (
                    (
                        TyShape::Matrix {
                            width: 2,
                            height: 2,
                        },
                        ValType::F32,
                    ),
                    (TyShape::Vector { width: 2 }, ValType::F32),
                ) => return Ok("mul_mat2_vec2".to_string()),
                (
                    (TyShape::Vector { width: 2 }, ValType::F32),
                    (
                        TyShape::Matrix {
                            width: 2,
                            height: 2,
                        },
                        ValType::F32,
                    ),
                ) => return Ok("mul_vec2_mat2".to_string()),
                //3
                (
                    (
                        TyShape::Matrix {
                            width: 3,
                            height: 3,
                        },
                        ValType::F32,
                    ),
                    (TyShape::Vector { width: 3 }, ValType::F32),
                ) => return Ok("mul_mat3_vec3".to_string()),
                (
                    (TyShape::Vector { width: 3 }, ValType::F32),
                    (
                        TyShape::Matrix {
                            width: 3,
                            height: 3,
                        },
                        ValType::F32,
                    ),
                ) => return Ok("mul_vec3_mat3".to_string()),
                //4
                (
                    (
                        TyShape::Matrix {
                            width: 4,
                            height: 4,
                        },
                        ValType::F32,
                    ),
                    (TyShape::Vector { width: 4 }, ValType::F32),
                ) => return Ok("mul_mat4_vec4".to_string()),
                (
                    (TyShape::Vector { width: 4 }, ValType::F32),
                    (
                        TyShape::Matrix {
                            width: 4,
                            height: 4,
                        },
                        ValType::F32,
                    ),
                ) => return Ok("mul_vec4_mat4".to_string()),
                //Mat Mat
                (
                    (
                        TyShape::Matrix {
                            width: 2,
                            height: 2,
                        },
                        ValType::F32,
                    ),
                    (
                        TyShape::Matrix {
                            width: 2,
                            height: 2,
                        },
                        ValType::F32,
                    ),
                ) => return Ok("mul_mat2_mat2".to_string()),
                (
                    (
                        TyShape::Matrix {
                            width: 3,
                            height: 3,
                        },
                        ValType::F32,
                    ),
                    (
                        TyShape::Matrix {
                            width: 3,
                            height: 3,
                        },
                        ValType::F32,
                    ),
                ) => return Ok("mul_mat3_mat3".to_string()),
                (
                    (
                        TyShape::Matrix {
                            width: 4,
                            height: 4,
                        },
                        ValType::F32,
                    ),
                    (
                        TyShape::Matrix {
                            width: 4,
                            height: 4,
                        },
                        ValType::F32,
                    ),
                ) => return Ok("mul_mat4_mat4".to_string()),
                other => {
                    return Err(WasmError::UnsupportedNode(format!(
                        "Unuspported MatMul signature:\n{:?}\n{:?}",
                        other.0, other.1
                    )))
                }
            }
        }

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

impl WasmNode {
    pub fn try_from_opt_buildin(
        value: &Buildin,
        input_sig: &[vola_opt::common::Ty],
        #[allow(unused_variables)] output_sig: &[vola_opt::common::Ty],
    ) -> Result<Self, WasmError> {
        let node = match &value.op {
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
            BuildinOp::SquareRoot => {
                assert!(input_sig.len() == 1);
                if input_sig[0].is_scalar() {
                    WasmNode::Unary(WasmUnaryOp::new(walrus::ir::UnaryOp::F32Sqrt))
                } else {
                    WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                        value.inputs.len(),
                        ExternOp::SquareRoot,
                    ))
                }
            }
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
        };

        Ok(node)
    }
}
