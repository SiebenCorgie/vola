use std::env::consts::OS;

use tree_sitter::Node;
use vola_ast::{
    alge::{Expr, ExprTy, Func},
    common::{Block, Ident, Literal, Stmt},
};
use vola_common::Span;

use crate::{ParserCtx, error::ParserError, statement::ScadStmt};

///Parses the identifier into a simple string based ident
pub fn identifier(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<Ident, ParserError> {
    if node.kind() != "identifier" {
        return Err(ParserError::MalformedNode(format!(
            "expected ident, got {:?}",
            node.kind()
        )));
    }

    let ident = node.utf8_text(data).map_err(|e| ParserError::from(e))?;
    Ok(Ident(ident.to_owned()))
}

///So openScad _literal_ is a little strange, since function-values are treated as literals?
///Anyways, we just Declare that here, and let the _user_ handle the rest.
pub enum ScadLiteral {
    Function(Func),
    Range {
        start: Expr,
        end: Expr,
        increment: Option<Expr>,
    },
    Undef,
    List(Vec<Expr>),
    Decimal(i32),
    Float(f64),
    Boolean(bool),
}

impl ScadLiteral {
    pub fn into_expr_ty(self) -> Result<ExprTy, ParserError> {
        match self {
            Self::Function(f) => Err(ParserError::UnsupportedScadFeature(
                "function-variable".to_owned(),
            )),
            Self::Undef => Err(ParserError::UnsupportedScadFeature(
                "undefined-value".to_owned(),
            )),
            Self::Range { .. } => Err(ParserError::NoExpr),
            Self::List(elements) => Ok(ExprTy::List(elements)),
            Self::Decimal(d) => {
                if d.is_negative() {
                    Ok(ExprTy::Unary {
                        op: vola_ast::alge::UnaryOp::Neg,
                        operand: Box::new(Expr {
                            span: Span::empty(),
                            expr_ty: ExprTy::Literal(Literal::IntegerLiteral(d.abs() as usize)),
                        }),
                    })
                } else {
                    Ok(ExprTy::Literal(Literal::IntegerLiteral(d as usize)))
                }
            }
            Self::Float(f) => Ok(ExprTy::Literal(Literal::FloatLiteral(f))),
            Self::Boolean(b) => Ok(ExprTy::Literal(Literal::BoolLiteral(b))),
        }
    }
}

pub fn literal(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadLiteral, ParserError> {
    match node.kind() {
        "string" => Err(ParserError::UnsupportedScadFeature("string".to_owned())),
        "number" => number(ctx, data, node),
        "undef" => Ok(ScadLiteral::Undef),
        "range" => {
            let start = crate::expr::expr(
                ctx,
                data,
                node.child_by_field_name("start").as_ref().unwrap(),
            )?;
            let end = crate::expr::expr(
                ctx,
                data,
                node.child_by_field_name("start").as_ref().unwrap(),
            )?;
            let increment = if let Some(increment) = node.child_by_field_name("increment").as_ref()
            {
                Some(crate::expr::expr(ctx, data, increment)?)
            } else {
                None
            };
            Ok(ScadLiteral::Range {
                start,
                end,
                increment,
            })
        }
        "list" => {
            println!("Impl List");
            Ok(ScadLiteral::List(Vec::with_capacity(0)))
        }
        other => Err(ParserError::Unexpected(format!(
            "Expected some kind of literal, got {}",
            other
        ))),
    }
}

pub fn number(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadLiteral, ParserError> {
    match node.kind() {
        "decimal" => {
            //try to parse the decimal as int
            let int: i32 = node
                .utf8_text(data)
                .map_err(|e| ParserError::Utf8ParseError(e))?
                .parse()
                .map_err(|e| ParserError::from(e))?;
            Ok(ScadLiteral::Decimal(int))
        }
        "float" => {
            let float: f64 = node
                .utf8_text(data)
                .map_err(|e| ParserError::Utf8ParseError(e))?
                .parse()
                .map_err(|e| ParserError::from(e))?;
            Ok(ScadLiteral::Float(float))
        }
        other => Err(ParserError::Unexpected(format!(
            "Expected Float or decimal, got {}",
            other
        ))),
    }
}
