use crate::alge::AlgeExpr;
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
pub enum Ty {
    CSGTree,
    Scalar,
    Vec { width: usize },
    Matrix { width: usize, height: usize },
    Tensor { dim: SmallVec<[usize; 3]> },
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

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedIdent {
    pub span: Span,
    pub ident: Ident,
    pub ty: Ty,
}

///Call to some `ident` with `args`

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Call {
    pub span: Span,
    pub ident: Ident,
    pub args: SmallVec<[AlgeExpr; 3]>,
}
