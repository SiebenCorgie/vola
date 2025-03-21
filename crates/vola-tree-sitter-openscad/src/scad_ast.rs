//! Before parsing into the Vola-AST, we parse into the SCAD AST.
//! This allows us to _move-around_ things in order to statisfy Vola's semantics, without
//! breaking Scad's

use std::path::PathBuf;

use tree_sitter::Node;
use vola_ast::{
    VolaAst,
    alge::{BinaryOp, UnaryOp},
    common::{Comment, Ident},
};
use vola_common::Span;

use crate::{ParserCtx, error::ParserError, util::ScadLiteral};

#[derive(Debug)]
pub struct ScadTopLevel {
    pub main: ScadBlock,
    pub modules: Vec<ScadModule>,
    //TODO: Add functions,
}

impl ScadTopLevel {
    pub fn empty(ctx: &ParserCtx, toplevel_node: &Node) -> Self {
        Self {
            main: ScadBlock {
                span: ctx.span(toplevel_node),
                stmts: Vec::new(),
            },
            modules: Vec::new(),
        }
    }

    pub fn normalize(&mut self) -> Result<(), Vec<ParserError>> {
        let mut errors = Vec::with_capacity(0);
        for module in &mut self.modules {
            if let Err(e) = module.block.normalize() {
                errors.push(e);
            }
        }
        if let Err(e) = self.main.normalize() {
            errors.push(e)
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }

    pub fn unroll_csg(&mut self) -> Result<(), Vec<ParserError>> {
        todo!()
    }

    pub fn into_vola_ast(mut self) -> Result<VolaAst, Vec<ParserError>> {
        todo!()
    }
}

#[derive(Debug)]
pub struct ScadBlock {
    pub span: Span,
    pub stmts: Vec<ScadStmt>,
}

#[derive(Debug)]
pub struct ScadModule {
    pub span: Span,
    pub name: Ident,
    pub args: Vec<ScadParameter>,
    pub block: ScadBlock,
}

#[derive(Debug)]
pub enum ScadParameter {
    ByIdent { span: Span, ident: Ident },
    ByAssignment(ScadAssignment),
}

#[derive(Debug)]
pub struct ScadAssignment {
    pub span: Span,
    pub var: Ident,
    pub expr: Box<ScadExpr>,
}

#[derive(Debug)]
pub enum ChainElement {
    Call(ScadCall),
    Block(ScadBlock),
}

#[derive(Debug)]
pub enum ScadStmt {
    ///Implicit union of ScadStmts based on a given range
    ForBlock {
        range_start: ScadExpr,
        range_end: ScadExpr,
        increment: ScadExpr,
        block: ScadBlock,
        is_intersect: bool,
    },
    Chain {
        chain: Vec<ChainElement>,
    },
    Assign(ScadAssignment),
    IfBlock {
        condition: ScadExpr,
        consequence: ScadBlock,
        alternative: Option<ScadBlock>,
    },
    //Either a let, or assign statement, followed by a _bodied_ block
    Overwrite {
        overwrites: Vec<ScadAssignment>,
        block: ScadBlock,
    },
    IncludeStmt(PathBuf),
    Comment(Comment),
    Assert,
    None,
}

#[derive(Debug)]
pub enum ScadArg {
    Expr(ScadExpr),
    Assign(ScadAssignment),
}

#[derive(Debug)]
pub struct ScadCall {
    pub function: Box<ScadExpr>,
    pub args: Vec<ScadArg>,
}

#[derive(Debug)]
pub enum ScadExpr {
    Binary {
        op: BinaryOp,
        left: Box<Self>,
        right: Box<Self>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Self>,
    },
    Ternary {
        condition: Box<Self>,
        consequence: Box<Self>,
        alternative: Box<Self>,
    },
    //Let or assign expression
    Overwrite {
        overwrites: Vec<ScadAssignment>,
        block: ScadBlock,
    },
    Call(ScadCall),
    //Contains _normal_ index, and dotIndex expression
    Index {
        value: Box<Self>,
        index: Box<Self>,
    },
    Assert,
    Literal(ScadLiteral),
    Var(Ident),
}
