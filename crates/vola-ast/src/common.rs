use smallvec::SmallVec;

use crate::alge::AlgeExpr;

pub struct Ident(pub String);

///All types that can be represented in the DSL.
/// By definition the scalar type (and elements of Vec/Mat/Tensor) are always
/// _floats_. Their resolution can be decided at code-generation time.
pub enum Ty {
    CSGTree,
    Scalar,
    Vec { width: usize },
    Matrix { width: usize, height: usize },
    Tensor { dim: SmallVec<[usize; 3]> },
}

pub struct TypedIdent {
    pub ident: Ident,
    pub ty: Ty,
}

///Call to some `ident` with `args`
pub struct Call {
    pub ident: Ident,
    pub args: SmallVec<[AlgeExpr; 3]>,
}
