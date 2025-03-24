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

    pub fn into_vola_ast(self) -> Result<VolaAst, Vec<ParserError>> {
        //convert all _modules_ into functions with csg-return argument.
        //
        //convert the main function into that _as well :D, but infer the
        //body as a function with no arguments, and only the csg return expression.

        let mut errors = Vec::new();
        let mut ast = VolaAst::empty();

        //Pre_insert the _openscad_library_ we _assume_ exists.
        ast.entries.push(vola_ast::TopLevelNode {
            span: Span::empty(),
            ct_args: Vec::with_capacity(0),
            entry: vola_ast::AstEntry::Comment(vola_ast::common::Comment {
                span: Span::empty(),
                content: "//Vola implementation of OpenScad CSG operations and primitives"
                    .to_owned(),
            }),
        });
        ast.entries.push(vola_ast::TopLevelNode {
            span: Span::empty(),
            ct_args: Vec::with_capacity(0),
            entry: vola_ast::AstEntry::Module(vola_ast::Module {
                span: Span::empty(),
                path: smallvec::smallvec![Ident("openscad_library".to_owned())],
            }),
        });

        for module in self.modules {
            let span = module.span.clone();
            match crate::convert::emit_module_as_function(module) {
                Err(e) => errors.push(e),
                Ok(f) => ast.entries.push(vola_ast::TopLevelNode {
                    span,
                    ct_args: Vec::with_capacity(0),
                    entry: vola_ast::AstEntry::Func(f),
                }),
            }
        }

        match crate::convert::emit_block_as_main_function(self.main) {
            Err(e) => errors.push(e),
            Ok(f) => ast.entries.push(vola_ast::TopLevelNode {
                //NOTE: we cant't build a span for main, because main is made up from anything that we discovered _along-the-way_
                span: Span::empty(),
                ct_args: Vec::with_capacity(0),
                entry: vola_ast::AstEntry::Func(f),
            }),
        }

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(ast)
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScadBlock {
    pub span: Span,
    pub stmts: Vec<ScadStmt>,
}

#[derive(Debug, Clone)]
pub struct ScadModule {
    pub span: Span,
    pub name: Ident,
    pub args: Vec<ScadParameter>,
    pub block: ScadBlock,
}

#[derive(Debug, Clone)]
pub enum ScadParameter {
    ByIdent { span: Span, ident: Ident },
    ByAssignment(ScadAssignment),
}

#[derive(Debug, Clone)]
pub struct ScadAssignment {
    pub span: Span,
    pub var: Ident,
    pub expr: Box<ScadExpr>,
}

#[derive(Debug, Clone)]
pub enum ChainElement {
    Call(ScadCall),
    Block(ScadBlock),
}

#[derive(Debug, Clone)]
#[allow(unused)]
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
        span: Span,
        chain: Vec<ChainElement>,
    },
    Assign(ScadAssignment),
    IfBlock {
        head_span: Span,
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

#[derive(Debug, Clone)]
pub enum ScadArg {
    Expr(ScadExpr),
    Assign(ScadAssignment),
}

#[derive(Debug, Clone)]
pub struct ScadCall {
    pub span: Span,
    pub function: Ident,
    pub args: Vec<ScadArg>,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum ScadExpr {
    Binary {
        span: Span,
        op: BinaryOp,
        left: Box<Self>,
        right: Box<Self>,
    },
    Unary {
        span: Span,
        op: UnaryOp,
        expr: Box<Self>,
    },
    Ternary {
        span: Span,
        condition: Box<Self>,
        consequence: Box<Self>,
        alternative: Box<Self>,
    },
    //Let or assign expression
    Overwrite {
        span: Span,
        overwrites: Vec<ScadAssignment>,
        block: ScadBlock,
    },
    Call(ScadCall),
    //Contains _normal_ index, and dotIndex expression
    Index {
        span: Span,
        value: Box<Self>,
        index: Box<Self>,
    },
    Assert,
    Literal {
        span: Span,
        lit: ScadLiteral,
    },
    Var {
        span: Span,
        var: Ident,
    },
}
