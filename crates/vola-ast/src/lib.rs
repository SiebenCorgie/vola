//! # Vola-AST
//!
//! Vola's *A*bstract *S*yntax *T*ree.
//!
//!
//! The AST is designed to be easily transformed into SSA 3-address code. This allows us to transform it into
//! MLIR Code, or directly into SPIR-V, depending on the chosen back/middle end.
//!
//!
//! The design somewhat reflects the design of the vola language syntax. There are two levels,
//! a `cobinatorical`, which applies operations to primitives, and, at the top level defines a `field`.
//!
//! Each of those primitives (and arguments to operations) are defined by an algebraic expression. That could be
//! a simple value, lets say `5`, or an expression, lets say `5+a`.
//!
//! The AST thefore is split into two as well. The toplevel `combinatorical`, and a lower level, more traditional
//! algebraic AST. For simplicity we call the combinatorical AST just **AST**.
//!
//! This allows a compiler to first resolve the evaluation time of each algebraic expression, before compiling down to
//! the actual equation.
//!
//! NOTE(tendsin): Lets see if this actually works out this way :D

use std::path::Path;

use thiserror::Error;

mod parser;
pub use parser::parser;

#[derive(Debug, Error)]
pub enum AstError {
    #[error("Failed to load vola parser: {0}")]
    LanguageError(#[from] tree_sitter::LanguageError),
    #[error("{0}")]
    IoError(#[from] std::io::Error),
    #[error("Failed to parse from file")]
    ParseError,
}

///Type description
pub enum Ty {
    Scalar,
    Vector { count: usize },
    Mat { width: usize, height: usize },
}

//a string identifier
pub struct Identifier(String);
//A single digit
pub struct ImmVal(usize);
//a float immediate value
pub struct ImmFloat(ImmVal, ImmVal);

pub struct TypedIdent {
    ident: Identifier,
    ty: Option<Ty>,
}

pub enum UnOp {
    //The ! op
    Not,
    // The - op
    Neg,
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

///Algebraic expression.
pub struct AlgeExpr {}

///Let statements are our entry point from the
/// combinatorical level to the algebraic
pub struct LetStmt {
    identifier: TypedIdent,
    stmt: AlgeExpr,
}

pub struct AlgeAst {
    identifier: Identifier,
    args: Vec<TypedIdent>,
    stmts: Vec<LetStmt>,
    ret: AlgeExpr,
}

pub enum PrimStmt {
    PrimDef(Identifier),
    LetStmt(LetStmt),
    PrimAssignment {
        prim: Identifier,
        field: Identifier,
        expr: AlgeExpr,
    },
}

pub struct PrimAst {
    identifier: Identifier,
    args: Vec<TypedIdent>,
    stmts: Vec<PrimStmt>,
    //Always needs to return a identifier to a primitive
    ret: Identifier,
}

pub enum Assignment {
    FieldAssignment {
        prim: Identifier,
        field: Identifier,
        ///Possible binary operation on former value
        op: Option<BinOp>,
        expr: AlgeExpr,
    },
    AtAssignment {
        op: Option<BinOp>,
        expr: AlgeExpr,
    },
}

pub enum OpStmt {
    PrimDef {
        identifier: Identifier,
        assignment: Option<OpTree>,
    },
    LetStmt(LetStmt),
    Assignment(Assignment),
}

pub struct OpAst {
    identifier: Identifier,
    prims: Vec<Identifier>,
    args: Vec<TypedIdent>,
    stmts: Vec<OpStmt>,
    ret: OpTree,
}

pub enum OpTree {
    Ident(Identifier),
    ///Calls into a primitive definition
    PrimCall {
        ident: Identifier,
        args: Vec<AlgeExpr>,
    },
    Op {
        op_ident: Identifier,
        sub_trees: Vec<OpTree>,
        args: Vec<AlgeExpr>,
    },
}

pub enum FieldStmt {
    PrimDef {
        identifier: Identifier,
        op_tree: OpTree,
    },
    LetStmt(LetStmt),
}

/// A field is defined as a set of statements, where each is either an arithmetic
/// let expression, a primitive definition or a so called Op-Tree (tree of OPs)
///
/// In addition the field must return an op tree (which is used to build the final evaluation order).
pub struct FieldAst {
    identifier: Identifier,
    args: Vec<TypedIdent>,
    ///Order of statements
    statments: Vec<FieldStmt>,
    return_tree: OpTree,
}

pub struct Ast {
    fields: Vec<FieldAst>,
    ops: Vec<OpAst>,
    prims: Vec<PrimAst>,
    alges: Vec<AlgeAst>,
}

impl Ast {
    ///Creates an empty Ast with the given root node
    fn empty() -> Self {
        Ast {
            fields: Vec::new(),
            ops: Vec::new(),
            prims: Vec::new(),
            alges: Vec::new(),
        }
    }

    ///Tries to parse `file` using `tree-sitter-vola` into an [Ast].
    fn from_file(file: impl AsRef<Path>) -> Result<Self, AstError> {
        let mut parser = parser()?;
        let text = std::fs::read_to_string(file)?;
        let syn_tree = parser.parse(text, None).ok_or(AstError::ParseError)?;

        //TODO transform to ast

        let mut ast = Ast::empty();

        Ok(ast)
    }
}
