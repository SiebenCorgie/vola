//! The Algebraic AST

use crate::common::{Call, Ident};
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

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FieldAccessor {
    Digit(usize),
    Ident(Ident),
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
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct AlgeOp {}
