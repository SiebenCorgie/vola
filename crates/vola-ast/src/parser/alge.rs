//! Algebra related parser portion.

use vola_common::{ErrorReporter, Span};

use crate::{
    alge::{AlgeExpr, AlgeExprTy, LetStmt},
    common::Ident,
    error::ParserError,
};

use super::FromTreeSitter;

impl FromTreeSitter for AlgeExpr {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        println!("TODO: Alge impl!");

        ParserError::assert_node_no_error(reporter, node)?;
        Ok(AlgeExpr {
            span: Span::from(node),
            expr_ty: AlgeExprTy::Ident(Ident("AlgeExpr".to_owned())),
        })
    }
}

impl FromTreeSitter for LetStmt {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        println!("Impl Let stmt!");

        ParserError::assert_node_no_error(reporter, node)?;
        Ok(LetStmt {
            span: Span::from(node),
            decl_name: Ident("SomeLetStmt".to_owned()),
            expr: AlgeExpr {
                span: Span::empty(),
                expr_ty: AlgeExprTy::Ident(Ident("SomeAlgeExprInLet".to_owned())),
            },
        })
    }
}
