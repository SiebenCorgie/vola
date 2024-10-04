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
};
use vola_opt::alge::{
    arithmetic::{UnaryArith, UnaryArithOp},
    logical::UnaryBool,
    matrix::{UnaryMatrix, UnaryMatrixOp},
    trigonometric::{Trig, TrigOp},
};

use crate::graph::WasmNode;

use super::{ExternOp, WasmRuntimeOp};

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
