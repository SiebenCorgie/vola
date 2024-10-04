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
use vola_opt::imm::{ImmNat, ImmScalar};

use crate::graph::WasmNode;

mod binary;
pub use binary::WasmBinaryOp;
mod buildin;
pub use buildin::{ExternOp, WasmRuntimeOp};
mod unary;
pub use unary::WasmUnaryOp;

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
pub struct Index {
    #[input]
    pub input: Inport,
    pub index: usize,
    #[output]
    pub output: Outport,
}

#[derive(LangNode)]
pub struct Construct {
    #[inputs]
    pub inputs: SmallColl<Inport>,
    #[output]
    pub output: Outport,
}

impl From<&vola_opt::alge::Construct> for Construct {
    fn from(value: &vola_opt::alge::Construct) -> Self {
        Self {
            inputs: smallvec![Inport::default(); value.inputs.len()],
            output: Outport::default(),
        }
    }
}
