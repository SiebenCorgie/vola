/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
mod legalization;
mod lower_ast;
mod spv;
pub use spv::EmitCtx;
mod hl_dispatch;
//Not really a pass, but a small utility that allows us to
//render a region's CFG to svg.
#[cfg(feature = "dot")]
mod cfg_dot;
