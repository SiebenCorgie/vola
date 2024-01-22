use smallvec::SmallVec;
use vola_common::Span;

use crate::common::Ident;

pub struct CSGOp {
    span: Span,
    op: Ident,
    ///Sub trees this CSGOp evaluates. Mostly this is either 0, 1 or 2.
    sub_trees: SmallVec<[CSGRegion; 2]>,
}

pub enum CSGStmt {
    //Simple let statement, something like `let a = 3 + b;`
    // where "3 + a" is factored out to a callable algebraic function `alge tmp(a: s) -> s{ 3 + a }`
    LetStmt {
        decl_name: Ident,
        callable: Ident,
        arguments: SmallVec<[Ident; 2]>,
    },
}

///Region of some CSG operation
pub struct CSGRegion {
    ///Statements that are _executed_ before `op` is evaluated.
    stmts: SmallVec<[CSGStmt; 3]>,
    //The final op of this tree
    op: Box<CSGOp>,
}
