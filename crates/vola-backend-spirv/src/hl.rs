/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

///A higher level op. Already in SSA-form and SPIR-V like.
///
/// We use those ops mainly to defer the creation of the actual SpvNode, until we know the input and output types
/// of that node. For instance there are 3 "Add" instructions in SPIR-V: float, signed_int, unsigned_int.
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HlOp {
    Negate,
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    Neq,

    Min,
    Max,
    Mix,
    Clamp,
    Abs,
}
