use smallvec::SmallVec;
use vola_common::Span;

use crate::{
    alge::{AlgeExpr, LetStmt},
    common::{Call, Ident, Ty, TypedIdent},
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct CSGOp {
    pub span: Span,
    pub op: Ident,
    pub args: SmallVec<[AlgeExpr; 3]>,
    ///Sub trees this CSGOp evaluates. Mostly this is either 0, 1 or 2.
    pub sub_trees: Vec<CSGOp>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum CSGStmt {
    LetStmt(LetStmt),
    CSGBinding(CSGBinding),
}

///Binds a CSG-Tree to an identifier
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct CSGBinding {
    pub span: Span,
    pub decl_name: Ident,
    pub tree: CSGOp,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct FieldDef {
    pub span: Span,
    pub name: Ident,
    pub inputs: SmallVec<[TypedIdent; 2]>,
    pub stmts: Vec<CSGStmt>,
    pub ret: CSGOp,
}

///A export function, which defines a clear signature
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct ExportFn {
    pub span: Span,
    pub name: Ident,
    pub inputs: SmallVec<[TypedIdent; 2]>,
    pub stmts: Vec<CSGStmt>,
    pub access_descriptors: SmallVec<[AccessDesc; 2]>,
}

///Last part of an export function. Describes how, and based on what parameters trees are evaluated.
///
/// Implicitly declares the return type of a [ExportFn].
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct AccessDesc {
    pub span: Span,
    ///The tree that is being called
    pub tree_ref: Ident,
    ///The concept that is evaluated based on some arguments
    pub call: Call,
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
