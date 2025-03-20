use tree_sitter::Node;
use vola_ast::{
    alge::{Expr, ExprTy},
    common::{Ident, Literal},
};

use crate::{ParserCtx, error::ParserError};

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

//parses the full expression.
pub fn expr(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<Expr, ParserError> {
    //NOTE: the syn-tree is _sometimes_ strange, so we have all kinds of escape hatches :/
    let expr_ty = match node.kind() {
        "decimal" | "float" => {
            let num = crate::util::number(ctx, data, node)?;
            //found some kind of literal, unwrap it into an expression for vola, and return that
            num.into_expr_ty()?
        }
        "literal" => {
            let lit = crate::util::literal(ctx, data, node)?;
            lit.into_expr_ty()?
        }
        "identifier" => {
            let ident = crate::util::identifier(ctx, data, node)?;
            ExprTy::Ident(ident)
        }
        "special_variable" => {
            let ident = crate::util::identifier(ctx, data, node.child(1).as_ref().unwrap())?;
            ExprTy::Ident(Ident(format!("${}", ident.0)))
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
        | "assert_expression" => ExprTy::Ident(Ident("teddy".to_string())),
        "list" | "range" | "undef" | "number" | "string" => {
            crate::util::literal(ctx, data, node)?.into_expr_ty()?
        }
        other => {
            return Err(ParserError::Unexpected(format!(
                "Unexpected node-kind in expression location: {}",
                other
            )));
        }
    };

    Ok(Expr {
        span: ctx.span(node),
        expr_ty,
    })
}
