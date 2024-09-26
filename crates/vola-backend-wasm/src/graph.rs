/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{edge::LangEdge, nodes::LangNode, rvsdg_derive_lang::LangNode};

use crate::wasm;

#[derive(LangNode)]
pub struct WasmNode {
    #[expose]
    node: Box<dyn LangNode + 'static>,
}

pub enum WasmTy {
    I32,
    I64,
    F32,
    F64,
    Undefined,
}

pub enum WasmEdge {
    State,
    Value { ty: WasmTy },
}

impl LangEdge for WasmEdge {
    fn state_edge() -> Self {
        Self::State
    }

    fn value_edge() -> Self {
        Self::Value {
            ty: WasmTy::Undefined,
        }
    }

    fn is_state_edge(&self) -> bool {
        if let Self::State = self {
            true
        } else {
            false
        }
    }

    fn is_value_edge(&self) -> bool {
        if let Self::Value { .. } = self {
            true
        } else {
            false
        }
    }
}
