/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # WASM dialect

use rvsdg::{
    region::{Inport, Output},
    rvsdg_derive_lang::LangNode,
    SmallColl,
};

pub enum ExternOp {
    Cross,
    Dot,
}

#[derive(LangNode)]
pub struct WasmExternOp {
    #[inputs]
    inputs: SmallColl<Inport>,
    op: ExternOp,
    #[output]
    output: Output,
}

#[derive(LangNode)]
pub struct WasmBinaryOp {
    #[inputs]
    inputs: [Inport; 2],
    op: walrus::ir::UnaryOp,
    #[output]
    output: Output,
}

#[derive(LangNode)]
pub struct WasmUnaryOp {
    #[input]
    inputs: Inport,
    op: walrus::ir::UnaryOp,
    #[output]
    output: Output,
}
