/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # WASM-Runtime
//!
//! Exports all symbols needed by the wasm backend to fully function.
//!
//! Gets build automatically by the backend's build script.

#![no_std]

pub mod arithmetic;
pub mod buildin;
pub mod impl_macros;
pub mod trigonometric;
