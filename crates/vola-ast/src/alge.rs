//! The Algebraic AST

use smallvec::SmallVec;
use vola_common::Span;

use crate::common::{Call, Ident};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Not,
    Neg,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Clone, Debug)]
pub struct AlgeExpr {
    pub span: Span,
    pub expr_ty: AlgeExprTy,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FieldAccessor {
    Digit(usize),
    Ident(Ident),
}

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
#[derive(Clone, Debug)]
pub struct AlgeOp {}
