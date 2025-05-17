/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use smallvec::SmallVec;
use vola_ast::{
    alge::Expr,
    common::{Block, Branch, Comment, Ident, Loop, Stmt},
};
use vola_common::VolaError;

use crate::{error::ParserError, FromTreeSitter, ParserCtx};

impl FromTreeSitter for Loop {
    fn parse(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, VolaError<ParserError>>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "theta_expr")?;
        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "for")?;

        let lv_identifier = Ident::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "in")?;
        let bound_lower = Expr::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "..")?;

        let step = if let Some(stepexp) = node.child_by_field_name("stepexpr") {
            Some(Expr::parse(ctx, dta, &stepexp)?)
        } else {
            None
        };

        let bound_upper = Expr::parse(ctx, dta, &children.next().unwrap())?;
        let body = Box::new(Block::parse(ctx, dta, &children.next().unwrap())?);

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(Loop {
            span: ctx.span(node),
            iteration_variable_ident: lv_identifier,
            bound_lower,
            bound_upper,
            step,
            body,
        })
    }
}

impl FromTreeSitter for Branch {
    fn parse(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, VolaError<ParserError>>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "gamma_expr")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        //consume the first if, which must always be there. Then iterate over all
        //_optional_ else_if branches, finally
        // check if there is a unconditional within the loop, which always must be the last
        //part.

        let mut unconditional = None;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "if")?;
        let conditional = (
            Expr::parse(ctx, dta, &children.next().unwrap())?,
            Box::new(Block::parse(ctx, dta, &children.next().unwrap())?),
        );

        if let Some(next_node) = children.next() {
            match next_node.kind() {
                "else" => {
                    unconditional =
                        Some(Box::new(Block::parse(ctx, dta, &children.next().unwrap())?));
                }
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_string(),
                        expected: "else, or nothing".to_owned(),
                    };
                    return Err(VolaError::error_here(err, ctx.span(&next_node), "here"));
                }
            }
        }

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(Branch {
            span: ctx.span(node),
            conditional,
            unconditional,
        })
    }
}

impl FromTreeSitter for Block {
    fn parse(
        ctx: &mut crate::ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, VolaError<ParserError>>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "block")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "{")?;

        let mut stmts = SmallVec::new();
        let mut retexpr = None;
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                "comment" => stmts.push(Stmt::Comment(Comment::parse(ctx, dta, &next_node)?)),
                "stmt" => stmts.push(Stmt::parse(ctx, dta, &next_node)?),
                "expr" => {
                    //must be the last, and there fore return expression.
                    let expr = Expr::parse(ctx, dta, &next_node)?;
                    //There might be some trailing comments left

                    let mut was_closing = false;
                    while let Some(trailing_node) = children.next() {
                        match trailing_node.kind() {
                            "comment" => {}
                            "}" => {
                                was_closing = children.next().is_none();
                                break;
                            }
                            any => {
                                let err = ParserError::UnexpectedAstNode {
                                    kind: any.to_owned(),
                                    expected: "\"}\" or comment".to_owned(),
                                };
                                return Err(VolaError::error_here(
                                    err,
                                    ctx.span(&trailing_node),
                                    "This should be either a \"}\" or a comment ...",
                                )
                                .with_label(
                                    expr.span,
                                    "... since this was identified as the return expression.",
                                ));
                            }
                        }
                    }

                    if !was_closing {
                        let err = ParserError::LevelNotEmpty;
                        return Err(VolaError::error_here(err, ctx.span(&next_node), "This should be the last statement of the block, since it's not assigned, and not a control flow expression. Therefore it must be a return expression."));
                    }
                    retexpr = Some(expr);
                    break;
                }
                //looks like its ending
                "}" => break,
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: " \"}\" | comment | stmt | expr".to_owned(),
                    };
                    return Err(VolaError::error_here(err, ctx.span(&next_node), "here"));
                }
            }
        }

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(Block {
            span: ctx.span(node),
            stmts,
            retexpr,
        })
    }
}
