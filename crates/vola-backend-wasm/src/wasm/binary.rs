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
};
use vola_opt::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp},
        logical::{BinaryBool, BinaryBoolOp},
        relational::{BinaryRel, BinaryRelOp},
    },
    common::{DataType, Shape},
};
use walrus::ValType;

use crate::{
    graph::{TyShape, WasmNode, WasmTy},
    wasm::{ExternOp, WasmRuntimeOp},
    WasmError,
};

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
    ) -> Result<Self, WasmError> {
        //NOTE: Most of those operations expect that all types are the same, so input[0] == input[1] == outupt.
        //      The only outlier is multiplication, which is defined for VecX * Scalar as well. So what we do is
        //      catching that case early, and emit a runtime op accordingly

        if value.op == BinaryArithOp::Mul && input_sig[0] != input_sig[1] {
            match input_sig {
                [vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width: _ },
                }, vola_opt::common::Ty::SCALAR_REAL] => {
                    return Ok(WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                        2,
                        ExternOp::MulVec,
                    )))
                }
                //Simple Matrix scaling
                [vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Matrix { width, height },
                }, vola_opt::common::Ty::SCALAR_REAL] => {
                    if width != height {
                        return Err(WasmError::UnsupportedNode(format!(
                            "Matrix-Scalar multiplication is only supported for square matrices: This one was Mat<{},{}>",
                            width, height
                        )));
                    }

                    if width > 4 {
                        return Err(WasmError::UnsupportedNode(format!(
                                "Matrix-Scalar multiplication is only supported up to Mat<4,4>. This one was Mat<{},{}>",
                                width, height
                            )));
                    }
                    return Ok(WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                        2,
                        ExternOp::MulMat,
                    )));
                }
                //Matrix-Vector multiplication. Note Both work, Mat_x_Vec and Vec_x_Mat
                [vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape:
                        Shape::Matrix {
                            width: 2,
                            height: 2,
                        },
                }, vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width: 2 },
                }]
                | [vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width: 2 },
                }, vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape:
                        Shape::Matrix {
                            width: 2,
                            height: 2,
                        },
                }] => {
                    return Ok(WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                        2,
                        ExternOp::MulMat,
                    )));
                }
                [vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape:
                        Shape::Matrix {
                            width: 3,
                            height: 3,
                        },
                }, vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width: 3 },
                }]
                | [vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width: 3 },
                }, vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape:
                        Shape::Matrix {
                            width: 3,
                            height: 3,
                        },
                }] => {
                    return Ok(WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                        2,
                        ExternOp::MulMat,
                    )));
                }
                [vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape:
                        Shape::Matrix {
                            width: 4,
                            height: 4,
                        },
                }, vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width: 4 },
                }]
                | [vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width: 4 },
                }, vola_opt::common::Ty::Shaped {
                    ty: DataType::Real,
                    shape:
                        Shape::Matrix {
                            width: 4,
                            height: 4,
                        },
                }] => {
                    return Ok(WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                        2,
                        ExternOp::MulMat,
                    )));
                }
                _ => {
                    return Err(WasmError::UnsupportedNode(format!(
                        "{:?} does not support signature\nInput: {}, {}\nOutput: {}",
                        value.op, input_sig[0], input_sig[1], output_sig
                    )))
                }
            }
        }

        assert!(
            input_sig[0] == input_sig[1] && input_sig[0] == output_sig,
            "signature test failed for {:?}: {:?} != {:?}",
            value.op,
            input_sig,
            output_sig
        );

        let wasm_inout_ty = WasmTy::try_from(input_sig[0].clone()).unwrap();

        let node = match wasm_inout_ty {
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
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32Add))
                    }
                    BinaryArithOp::Sub => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32Sub))
                    }
                    BinaryArithOp::Mul => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32Mul))
                    }
                    BinaryArithOp::Div => {
                        WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32DivU))
                    }
                    BinaryArithOp::Mod => {
                        return Err(WasmError::UnsupportedNode(format!(
                            "Mod is unsupported for Nat"
                        )))
                    }
                },
                _ => {
                    return Err(WasmError::UnexpectedSignature {
                        node: format!("{:?}", value.op),
                        input: input_sig.into_iter().collect(),
                        output: smallvec![output_sig],
                    })
                }
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
                _ => {
                    return Err(WasmError::UnexpectedSignature {
                        node: format!("{:?}", value.op),
                        input: input_sig.into_iter().collect(),
                        output: smallvec![output_sig],
                    });
                }
            },

            WasmTy::Defined {
                shape: TyShape::Matrix { width, height },
                ty: ValType::F32,
            } => {
                if width != height {
                    return Err(WasmError::UnsupportedNode(format!("Matrix-Matrix multiplication exepects square matrix, but had shape Mat<{}, {}>", width, height)));
                }
                match width{
                    1 => return Err(WasmError::UnsupportedNode(format!("Encountered Mat<1,1>"))),
                    2 | 3 | 4 =>
                    return Ok(WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                            2,
                            ExternOp::MulMat,
                        ))),
                    _other =>
                    return Err(WasmError::UnsupportedNode(format!("Matrix-Matrix multiplication only supported up to Mat<4,4>, is Mat<{width}, {height}>")))
                }
            }

            _ => {
                return Err(WasmError::UnexpectedSignature {
                    node: format!("{:?}", value.op),
                    input: input_sig.into_iter().collect(),
                    output: smallvec![output_sig],
                });
            }
        };

        Ok(node)
    }
}

impl WasmNode {
    pub fn try_from_op_binaryrel(
        value: &BinaryRel,
        input_sig: [vola_opt::common::Ty; 2],
        output_sig: vola_opt::common::Ty,
    ) -> Result<Self, WasmError> {
        //NOTE make sure both are of scalar or nat, otherwise
        //abort
        if !((input_sig[0].is_scalar() && input_sig[1].is_scalar())
            || (input_sig[0].is_integer() && input_sig[1].is_integer()))
        {
            return Err(WasmError::UnexpectedSignature {
                node: format!("{:?}", value.op),
                input: input_sig.into_iter().collect(),
                output: smallvec![output_sig],
            });
        }
        //Also, output must be a bool.
        if !output_sig.is_bool() {
            return Err(WasmError::UnexpectedSignature {
                node: format!("{:?}", value.op),
                input: input_sig.into_iter().collect(),
                output: smallvec![output_sig],
            });
        }

        //now dispatch either to f32 rels, or i32 rels, depending on the type
        let node = match input_sig[0] {
            //NOTE: we have _natural_ numbers, so only signed integers
            vola_opt::common::Ty::SCALAR_INT => match value.op {
                BinaryRelOp::Eq => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32Eq)),
                BinaryRelOp::Gt => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32GtU))
                }
                BinaryRelOp::Gte => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32GeU))
                }
                BinaryRelOp::Lt => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32LtU))
                }
                BinaryRelOp::Lte => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32LeU))
                }
                BinaryRelOp::NotEq => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32Ne))
                }
            },
            vola_opt::common::Ty::SCALAR_REAL => match value.op {
                BinaryRelOp::Eq => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Eq)),
                BinaryRelOp::Gt => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Gt)),
                BinaryRelOp::Gte => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Ge))
                }
                BinaryRelOp::Lt => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Lt)),
                BinaryRelOp::Lte => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Le))
                }
                BinaryRelOp::NotEq => {
                    WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::F32Ne))
                }
            },
            _ => return Err(WasmError::UnsupportedNode(format!("{:?}", value.op))),
        };

        Ok(node)
    }
}

impl WasmNode {
    pub fn try_from_opt_binary_bool(
        value: &BinaryBool,
        input_sig: [vola_opt::common::Ty; 2],
        output_sig: vola_opt::common::Ty,
    ) -> Result<Self, WasmError> {
        //NOTE we expect a bool input
        if !input_sig[0].is_bool() || !input_sig[1].is_bool() || !output_sig.is_bool() {
            return Err(WasmError::UnexpectedSignature {
                node: format!("{:?}", value.op),
                input: input_sig.into_iter().collect(),
                output: smallvec![output_sig],
            });
        }

        let node = match value.op {
            BinaryBoolOp::And => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32And)),
            BinaryBoolOp::Or => WasmNode::Binary(WasmBinaryOp::new(walrus::ir::BinaryOp::I32Or)),
        };

        Ok(node)
    }
}
