/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! The Algebraic AST

use std::{arch::asm, fmt::Display};

use crate::{
    common::{Block, Branch, Call, Ident, Literal, Ty, TypedIdent},
    csg::ScopedCall,
};
use smallvec::SmallVec;
use vola_common::Span;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
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
    NotEq,

    Or,
    And,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Expr {
    pub span: Span,
    pub expr_ty: ExprTy,
}

impl Expr {
    ///By default the `span` contains the whole space of the sub expressions.
    /// That is technically correct, but hard to read when reporting errors.
    ///
    /// Consider `(some_long_expr) + (some_other_long_expr)`. The span of `+` contains the whole
    /// line, however, when reporting an error on `+` you might only want the single char to be highlighted.
    /// Thats what this function is for.
    pub fn op_span(&self) -> Span {
        match &self.expr_ty {
            ExprTy::Unary { op: _, operand } => {
                let mut subspan = self.span.clone();
                subspan.byte_end = operand.span.byte_start;
                subspan.to = operand.span.from;
                subspan
            }
            ExprTy::Binary { left, right, op: _ } => {
                let mut span = self.span.clone();
                span.byte_start = left.span.byte_end;
                span.from = left.span.to;
                span.byte_end = right.span.byte_start;
                span.to = right.span.from;
                span
            }
            ExprTy::Call(c) => {
                if c.args.len() > 0 {
                    let mut span = c.span.clone();
                    span.byte_end = c.args[0].span.byte_start;
                    span.to = c.args[0].span.from;
                    span
                } else {
                    c.span.clone()
                }
            }
            ExprTy::EvalExpr(eexpr) => eexpr.span.clone(),
            ExprTy::FieldAccess { .. } => self.span.clone(),
            ExprTy::Ident(_i) => self.span.clone(),
            ExprTy::List(_) => self.span.clone(),
            ExprTy::Tuple(_) => self.span.clone(),
            ExprTy::Literal(_) => self.span.clone(),
            ExprTy::ScopedCall(c) => c.head_span(),
            ExprTy::BranchExpr(e) => e.span.clone(),
            ExprTy::SplatExpr { expr, count: _ } => expr.span.clone(),
        }
    }
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FieldAccessor {
    Digit { span: Span, digit: usize },
    Ident { span: Span, ident: Ident },
}

impl Display for FieldAccessor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Digit { digit, .. } => write!(f, "{digit}"),
            Self::Ident { ident, .. } => write!(f, "{}", ident.0),
        }
    }
}

impl FieldAccessor {
    pub fn try_to_index(&self) -> Option<usize> {
        match self {
            Self::Digit { digit, .. } => Some(*digit),
            Self::Ident { ident, .. } => match ident.0.as_str() {
                "x" => Some(0),
                "y" => Some(1),
                "z" => Some(2),
                "w" => Some(3),
                _ => None,
            },
        }
    }
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum ExprTy {
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        right: Box<Expr>,
        op: BinaryOp,
    },
    EvalExpr(EvalExpr),
    Ident(Ident),
    FieldAccess {
        src: Ident,
        accessors: SmallVec<[FieldAccessor; 1]>,
    },
    Call(Box<Call>),
    ScopedCall(Box<ScopedCall>),
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Literal(Literal),
    BranchExpr(Box<Branch>),
    SplatExpr {
        expr: Box<Expr>,
        count: usize,
    },
}

///Binds an algebraic expression to an identifier
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct LetStmt {
    pub span: Span,
    pub decl_name: Ident,
    pub expr: Expr,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct EvalExpr {
    pub span: Span,
    ///The operand that is being evaluated.
    pub evaluator: Ident,
    ///The concept that is being evaluated.
    pub concept: Ident,
    pub params: Vec<Expr>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct AssignStmt {
    pub span: Span,
    pub dst: Ident,
    pub expr: Expr,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Func {
    pub span: Span,
    pub is_export: bool,
    pub name: Ident,
    pub args: SmallVec<[TypedIdent; 3]>,
    pub return_type: Ty,
    pub block: Block,
}

impl Func {
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
