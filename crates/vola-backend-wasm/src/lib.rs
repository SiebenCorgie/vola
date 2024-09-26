/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # Vola's WASM backend
//!
//! The backend takes care of transforming a [Optimizer](vola_opt::Optimizer) into a WASM module. Any exported field is also exported under
//! its name into the module. You can then use the WASM code (or file) to load the generated code. For instance via [Wasmi](https://github.com/wasmi-labs/wasmi) or [Cranelift-Wasm] (https://docs.rs/cranelift-wasm/0.112.1/cranelift_wasm/).
//!
//!
//! Under the hood the module loads [vola-wasm-runtime]() crate's wasm module as a runtime. The crate implements special functions like `length`, `cross` etc, and loads functions like `sqrt`, `sin` etc.

use rvsdg::Rvsdg;
mod graph;
mod runtime;
mod wasm;

pub struct WasmBackend {
    graph: Rvsdg<graph::WasmNode, graph::WasmEdge>,
}
