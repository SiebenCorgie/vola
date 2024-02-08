use smallvec::SmallVec;
use vola_common::Span;

use crate::{
    alge::AlgeExpr,
    common::{Call, Ident, Ty, TypedIdent},
};

pub struct CSGOp {
    span: Span,
    op: Ident,
    args: SmallVec<[AlgeExpr; 3]>,
    ///Sub trees this CSGOp evaluates. Mostly this is either 0, 1 or 2.
    sub_trees: Vec<CSGOp>,
}

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

pub struct FieldDef {
    span: Span,
    inputs: SmallVec<[TypedIdent; 2]>,
    stmts: Vec<CSGStmt>,
    ret: CSGOp,
}

///A export function, which defines a clear signature
pub struct ExportFn {
    span: Span,
    inputs: SmallVec<[TypedIdent; 2]>,
    outputs: SmallVec<[Ty; 3]>,
    stmts: Vec<CSGStmt>,
    access_descriptors: SmallVec<[AccessDesc; 2]>,
}

///Last part of an export function. Describes how, and based on what parameters trees are evaluated.
///
/// Implicitly declares the return type of a [ExportFn].
pub struct AccessDesc {
    ///The tree that is being called
    pub tree_ref: Ident,
    ///The concept that is evaluated based on some arguments
    pub call: Call,
}
