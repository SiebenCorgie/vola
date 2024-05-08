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
    common::{Block, Stmt},
};
use vola_common::{ariadne::Label, error::error_reporter, report};

use crate::{error::ParserError, FromTreeSitter};

impl FromTreeSitter for Block {
    fn parse(
        ctx: &mut crate::ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, crate::error::ParserError>
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
                "comment" => continue,
                "stmt" => stmts.push(Stmt::parse(ctx, dta, &next_node)?),
                "expr" => {
                    //must be the last, and there fore return expression.
                    let expr = Expr::parse(ctx, dta, &next_node)?;
                    //try to consume the closing }, otherwise error out
                    let was_closing =
                        ParserError::consume_expected_node_string(ctx, dta, children.next(), "}")
                            .is_ok();

                    if !was_closing {
                        let err = ParserError::LevelNotEmpty;
                        report(
                            error_reporter(err.clone(), ctx.span(&next_node))
                                .with_label(Label::new(ctx.span(&next_node))
                                    .with_message("This should be the last statement of the block, since it's not assigned, and not a control flow expression. Therefore it must be a return expression.")
                                ).finish()
                        );
                        return Err(err);
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
                    report(
                        error_reporter(err.clone(), ctx.span(&next_node))
                            .with_label(Label::new(ctx.span(&next_node)).with_message("here"))
                            .finish(),
                    );
                    return Err(err);
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
