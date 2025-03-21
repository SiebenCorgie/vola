use tree_sitter::Node;
use vola_ast::{
    alge::{Expr, ExprTy},
    common::Ident,
};

use crate::{ParserCtx, error::ParserError, scad_ast::ScadExpr};

pub fn variable_name(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<Ident, ParserError> {
    match node.kind() {
        "identifier" => crate::util::identifier(ctx, data, node),
        "special_variable" => {
            let ident = crate::util::identifier(ctx, data, node.child(1).as_ref().unwrap())?;
            Ok(Ident(format!("${}", ident.0)))
        }
        other => Err(ParserError::MalformedNode(other.to_owned())),
    }
}

//parses the full scad expr.
pub fn expr(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadExpr, ParserError> {
    //NOTE: the syn-tree is _sometimes_ strange, so we have all kinds of escape hatches :/
    match node.kind() {
        "decimal" | "float" => {
            let num = crate::util::number(ctx, data, node)?;
            //found some kind of literal, unwrap it into an expression for vola, and return that
            Ok(num.into_expr())
        }
        "literal" => {
            let lit = crate::util::literal(ctx, data, node)?;
            Ok(lit.into_expr())
        }
        "identifier" => {
            let ident = crate::util::identifier(ctx, data, node)?;
            Ok(ScadExpr::Var(ident))
        }
        "special_variable" => {
            let ident = crate::util::identifier(ctx, data, node.child(1).as_ref().unwrap())?;
            Ok(ScadExpr::Var(Ident(format!("${}", ident.0))))
        }
        "parenthesized_expression"
        | "unary_expression"
        | "binary_expression"
        | "ternary_expression"
        | "let_expression"
        | "function_call"
        | "index_expression"
        | "dot_index_expression"
        | "assert_expression"
        | "assert_expression" => Ok(ScadExpr::Assert),
        "list" | "range" | "undef" | "number" | "string" | "boolean" => {
            Ok(crate::util::literal(ctx, data, node)?.into_expr())
        }
        other => Err(ParserError::Unexpected(format!(
            "Unexpected node-kind in expression location: {}",
            other
        ))),
    }
}
