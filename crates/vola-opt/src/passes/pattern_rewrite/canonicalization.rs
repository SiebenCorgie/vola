/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2026 Tendsin Mende
 */

//! Collects canonicalization rewrites, i.e. rewrites that create _simpler_, possibly bigger versions
//! of non trivial patterns.
//!
//! A classic is unrolling matrix-matrix multiplications, or breaking up a _vector-length_ calculation into
//! a square-root of the squares of each vector component.

mod matrix_multiplication;
pub use matrix_multiplication::UnrollMul;
mod loops;
pub use loops::UnrollOrPanic;
mod unary;
pub use unary::AproxAbs;
mod buildin;
pub use buildin::{AproxClamp, AproxMinMax, AproxMix, LowerLength};
