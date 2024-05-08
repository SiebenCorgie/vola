/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use smallvec::SmallVec;
use vola_common::Span;

use crate::{
    alge::{EvalExpr, Expr},
    common::{Block, Call, Ident, Ty, TypedIdent},
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct ScopedCall {
    pub span: Span,
    pub call: Call,
    //NOTE: using Vec, since _block_ can become pretty big
    pub blocks: Vec<Block>,
}

impl ScopedCall {
    ///Returns the span that only identifies the head of this operation
    pub fn head_span(&self) -> Span {
        self.call.span.clone()
    }
}

///Binds a CSG-Tree to an identifier
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct CsgStmt {
    pub span: Span,
    pub decl_name: Ident,
    pub expr: Expr,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct FieldDef {
    pub span: Span,
    pub name: Ident,
    pub args: SmallVec<[TypedIdent; 2]>,
    pub block: Block,
}

///A export function, which defines a clear signature
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct ExportFn {
    pub span: Span,
    pub name: Ident,
    pub args: SmallVec<[TypedIdent; 2]>,
    pub block: Block,
}

///Last part of an export function. Describes how, and based on what parameters trees are evaluated.
///
/// Implicitly declares the return type of a [ExportFn].
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct AccessDesc {
    pub span: Span,
    pub evals: SmallVec<[EvalExpr; 3]>,
}

///All types of nodes in a CSGTree
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CSGNodeTy {
    Entity,
    Operation,
}

///Either a entity or operation definition. Which means a definition of some kind of node in a CSGTree.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct CSGNodeDef {
    pub span: Span,
    pub ty: CSGNodeTy,
    pub name: Ident,
    pub args: SmallVec<[TypedIdent; 1]>,
}

///Defines a concept, that transforms some argument (or none) into a result.
///
/// This could for instance be a distance-field-type, that transforms a positional argument into the distance, or
/// a color-concept that just returns a color.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct CSGConcept {
    pub span: Span,
    pub name: Ident,
    pub src_ty: SmallVec<[Ty; 1]>,
    pub dst_ty: Ty,
}
