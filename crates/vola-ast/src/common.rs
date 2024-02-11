use smallvec::SmallVec;

use crate::alge::AlgeExpr;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub String);

///All types that can be represented in the DSL.
/// By definition the scalar type (and elements of Vec/Mat/Tensor) are always
/// _floats_. Their resolution can be decided at code-generation time.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ty {
    CSGTree,
    Scalar,
    Vec { width: usize },
    Matrix { width: usize, height: usize },
    Tensor { dim: SmallVec<[usize; 3]> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Digit(pub usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal {
    IntegerLiteral(usize),
    FloatLiteral(f64),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypedIdent {
    pub ident: Ident,
    pub ty: Ty,
}

///Call to some `ident` with `args`
#[derive(Clone, Debug)]
pub struct Call {
    pub ident: Ident,
    pub args: SmallVec<[AlgeExpr; 3]>,
}
