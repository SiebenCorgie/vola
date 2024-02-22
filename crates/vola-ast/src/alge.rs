//! The Algebraic AST

use crate::common::{Call, Ident, Literal};
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
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct AlgeExpr {
    pub span: Span,
    pub expr_ty: AlgeExprTy,
}

impl AlgeExpr {
    ///By default the `span` contains the whole space of the sub expressions.
    /// That is technically correct, but hard to read when reporting errors.
    ///
    /// Consider `(some_long_expr) + (some_other_long_expr)`. The span of `+` contains the whole
    /// line, however, when reporting an error on `+` you might only want the single char to be highlighted.
    /// Thats what this function is for.
    pub fn op_span(&self) -> Span {
        match &self.expr_ty {
            AlgeExprTy::Unary { op: _, operand } => {
                let mut subspan = self.span.clone();
                subspan.byte_end = operand.span.byte_start;
                subspan.to = operand.span.from;
                subspan
            }
            AlgeExprTy::Binary { left, right, op: _ } => {
                let mut span = self.span.clone();
                span.byte_start = left.span.byte_end;
                span.from = left.span.to;
                span.byte_end = right.span.byte_start;
                span.to = right.span.from;
                span
            }
            AlgeExprTy::Call(c) => c.span.clone(),
            AlgeExprTy::EvalExpr(eexpr) => eexpr.span.clone(),
            AlgeExprTy::FieldAccess { .. } => self.span.clone(),
            AlgeExprTy::Ident(_i) => self.span.clone(),
            AlgeExprTy::List(_) => self.span.clone(),
            AlgeExprTy::Literal(_) => self.span.clone(),
        }
    }
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FieldAccessor {
    Digit { span: Span, digit: usize },
    Ident { span: Span, ident: Ident },
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AlgeExprTy {
    Unary {
        op: UnaryOp,
        operand: Box<AlgeExpr>,
    },
    Binary {
        left: Box<AlgeExpr>,
        right: Box<AlgeExpr>,
        op: BinaryOp,
    },
    EvalExpr(EvalExpr),
    Ident(Ident),
    FieldAccess {
        src: Ident,
        accessors: SmallVec<[FieldAccessor; 1]>,
    },
    Call(Box<Call>),
    List(Vec<AlgeExpr>),
    Literal(Literal),
}

///Binds an algebraic expression to an identifier
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct LetStmt {
    pub span: Span,
    pub decl_name: Ident,
    pub expr: AlgeExpr,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct EvalExpr {
    pub span: Span,
    ///The operand that is being evaluated.
    pub evaluator: Ident,
    ///The concept that is being evaluated.
    pub concept: Ident,
    pub params: Vec<AlgeExpr>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct AssignStmt {
    pub span: Span,
    pub dst: Ident,
    pub expr: AlgeExpr,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AlgeStmt {
    Let(LetStmt),
    Assign(AssignStmt),
    DeadEval(EvalExpr),
}

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
    pub concept_arg_naming: SmallVec<[Ident; 1]>,

    pub stmts: Vec<AlgeStmt>,
    pub return_expr: AlgeExpr,
}
