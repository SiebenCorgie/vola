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
use vola_opt::alge::{
    arithmetic::{BinaryArith, BinaryArithOp},
    logical::{BinaryBool, BinaryBoolOp},
    relational::{BinaryRel, BinaryRelOp},
};

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
        assert!(input_sig[0] == input_sig[1] && input_sig[0] == output_sig);

        let wasm_inout_ty = WasmTy::from(input_sig[0].clone());

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
        //NOTE make sure both are of scalar shape, otherwise
        //abort
        if !input_sig[0].is_scalar() || !input_sig[1].is_scalar() {
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
