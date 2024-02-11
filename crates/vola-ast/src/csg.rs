use smallvec::SmallVec;
use vola_common::Span;

use crate::{
    alge::AlgeExpr,
    common::{Call, Ident, Ty, TypedIdent},
};

#[derive(Clone, Debug)]
pub struct CSGOp {
    pub span: Span,
    pub op: Ident,
    pub args: SmallVec<[AlgeExpr; 3]>,
    ///Sub trees this CSGOp evaluates. Mostly this is either 0, 1 or 2.
    pub sub_trees: Vec<CSGOp>,
}

#[derive(Clone, Debug)]
pub enum CSGStmt {
    LetStmt {
        span: Span,
        decl_name: Ident,
        expr: AlgeExpr,
    },
    CSGBinding {
        span: Span,
        decl_name: Ident,
        tree: CSGOp,
    },
}

#[derive(Clone, Debug)]
pub struct FieldDef {
    pub span: Span,
    pub name: Ident,
    pub inputs: SmallVec<[TypedIdent; 2]>,
    pub stmts: Vec<CSGStmt>,
    pub ret: CSGOp,
}

///A export function, which defines a clear signature
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
#[derive(Clone, Debug)]
pub struct AccessDesc {
    ///The tree that is being called
    pub tree_ref: Ident,
    ///The concept that is evaluated based on some arguments
    pub call: Call,
}
