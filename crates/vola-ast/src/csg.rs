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
    alge::Expr,
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

///All types of nodes in a CSGTree
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CsgTy {
    Entity,
    Operation,
}

///Either a entity or operation definition. Which means a definition of some kind of node in a CSGTree.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct CsgDef {
    pub span: Span,
    pub ty: CsgTy,
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
    pub src_ty: Ty,
    pub dst_ty: Ty,
}

///Implementation of either a `Operation` or `Entity`
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct ImplBlock {
    pub span: Span,
    //What is being implemented
    pub dst: Ident,
    //On how many operands we implement
    pub operands: SmallVec<[Ident; 2]>,
    ///The concept on which we implement
    pub concept: Ident,
    ///(Re)naming of the concepts input argument.
    pub concept_arg_name: Ident,
    pub block: Block,
}

impl ImplBlock {
    pub fn head_span(&self) -> Span {
        Span {
            file: self.span.file.clone(),
            from: self.span.from,
            to: self.block.span.from,
            byte_start: self.span.byte_start,
            byte_end: self.block.span.byte_start,
        }
    }
}
