/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::fmt::Display;

use crate::{
    alge::{AssignStmt, Expr, LetStmt},
    csg::CsgStmt,
};
use smallvec::SmallVec;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use vola_common::Span;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

///All types that can be represented in the DSL.
/// By definition the scalar type (and elements of Vec/Mat/Tensor) are always
/// _floats_. Their resolution can be decided at code-generation time.

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataTy {
    Csg,
    Void,
    Real,
    Integer,
    Bool,
    Complex,
    Quaternion,
}

///The shape of _some_ value.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Shape {
    Vec { width: usize },
    Matrix { width: usize, height: usize },
    Tensor { sizes: SmallVec<[usize; 8]> },
    Interval,
}

///Describes the shape and data-type of a value.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ty {
    Simple(DataTy),
    Shaped {
        ty: DataTy,
        shape: Shape,
    },
    ///Aggregate type in the form of a tuple
    Tuple(Vec<Self>),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Digit(pub usize);

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    IntegerLiteral(usize),
    FloatLiteral(f64),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::FloatLiteral(fl) => write!(f, "{}f", fl),
            Literal::IntegerLiteral(i) => write!(f, "{}i", i),
        }
    }
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedIdent {
    pub span: Span,
    pub ident: Ident,
    pub ty: Ty,
}

///compile-time argument. Can only sit _above_ a function / be attached to a Toplevel node.
///
/// Hints _things_ to the compiler down the road.
//NOTE: Right now there is no real difference to a _call_. However
//      Both arg conceptually different, so I decided to split them already, maybe
//      they diverge further down the line.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct CTArg {
    pub span: Span,
    pub ident: Ident,
    pub args: SmallVec<[Expr; 3]>,
}

///Call to some `ident` with `args`
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Call {
    pub span: Span,
    pub ident: Ident,
    pub args: SmallVec<[Expr; 3]>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Stmt {
    ///Binding of an expression value to an identifier
    Let(LetStmt),
    ///Assignment to an already existing identifier
    Assign(AssignStmt),
    ///Binding of a csg expression to an identifier.
    Csg(CsgStmt),
    Loop(Loop),
    Branch(Branch),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Block {
    pub span: Span,
    pub stmts: SmallVec<[Stmt; 3]>,
    pub retexpr: Option<Expr>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Branch {
    pub span: Span,
    //The "if" expression, and the block that is executed, if the expression is true.
    pub conditional: (Expr, Box<Block>),
    // _else_ branch, if there is any
    pub unconditional: Option<Box<Block>>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Loop {
    pub span: Span,
    ///Identifier of the iteration variable, that ranges from _bound_lower_ to _bound_upper_-1
    pub iteration_variable_ident: Ident,
    pub bound_lower: Expr,
    pub bound_upper: Expr,
    pub body: Box<Block>,
}

impl Loop {
    pub fn range_expr_span(&self) -> Span {
        let mut span = self.bound_lower.span.clone();
        let byte_end = span.byte_end.max(self.bound_upper.span.byte_end);
        let end = span.to.max(self.bound_upper.span.to);
        span.byte_end = byte_end;
        span.to = end;
        span
    }
}
