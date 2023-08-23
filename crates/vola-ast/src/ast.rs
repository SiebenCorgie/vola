///All possible nodes in an AST.
pub enum AstNode {
    Keyword,
    Stmt,
    Block,
    OpNode,

    AlgeExpr,

    Field,
    Prim,
    Op,
    AlgeFn,
}

pub struct Ast {}
