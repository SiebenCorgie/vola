//! The Algebraic AST

use smallvec::SmallVec;
use vola_common::Span;

use crate::common::{Call, Ident};

pub enum UnaryOp {
    Not,
    Neg,
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub struct AlgeExpr {
    span: Span,
    expr_ty: AlgeExprTy,
}

pub enum FieldAccessor {
    Digit(usize),
    Ident(Ident),
}

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
    EvalExpr {
        evaluator: Ident,
        params: Vec<AlgeExpr>,
    },
    Ident(Ident),
    FieldAccess {
        src: Ident,
        accessors: SmallVec<[FieldAccessor; 1]>,
    },
    Call(Box<Call>),
    List(Vec<[AlgeExpr; 3]>),
}

//NOTE yet specified!
pub struct AlgeOp {}
