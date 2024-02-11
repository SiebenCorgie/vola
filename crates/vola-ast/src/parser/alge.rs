//! Algebra related parser portion.

use vola_common::{ErrorReporter, Span};

use crate::{
    alge::{AlgeExpr, AlgeExprTy},
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

        Ok(AlgeExpr {
            span: Span::from(node),
            expr_ty: AlgeExprTy::Ident(Ident("AlgeExpr".to_owned())),
        })
    }
}
