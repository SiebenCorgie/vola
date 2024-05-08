/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use smallvec::{smallvec, SmallVec};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use vola_ast::{
    alge::{EvalExpr, Expr},
    common::{Block, Call, Ident, TypedIdent},
    csg::{AccessDesc, CSGConcept, CSGNodeDef, CSGNodeTy, CsgStmt, ScopedCall},
};

use super::{FromTreeSitter, ParserCtx};
use crate::{common::parse_alge_ty, error::ParserError};

impl FromTreeSitter for AccessDesc {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "access_desc")?;

        //There are two types of access_decs
        //1: (eval, eval, eval)
        //2. eval
        //find that by checking for (

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        let first_node = children.next().unwrap();

        let ad = match first_node.kind() {
            "(" => {
                //case 1.
                let mut evals = SmallVec::new();
                loop {
                    let next_node = children.next().unwrap();
                    match next_node.kind() {
                        "eval_expr" => evals.push(EvalExpr::parse(ctx, dta, &next_node)?),
                        ")" => break,
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: next_node.kind().to_owned(),
                                expected: "eval_expr | \",\"".to_owned(),
                            };

                            report(
                                error_reporter(err.clone(), ctx.span(&next_node))
                                    .with_label(
                                        Label::new(ctx.span(&next_node)).with_message("here"),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    }
                    //parse either the end, or the ,
                    let next_node = children.next().unwrap();
                    match next_node.kind() {
                        "," => {}
                        ")" => break,
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: next_node.kind().to_owned(),
                                expected: "eval_expr | \",\"".to_owned(),
                            };

                            report(
                                error_reporter(err.clone(), ctx.span(&next_node))
                                    .with_label(
                                        Label::new(ctx.span(&next_node)).with_message("here"),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    }
                }

                if evals.len() == 0 {
                    let err = ParserError::NoChildAvailable;

                    report(
                        error_reporter(err.clone(), ctx.span(&node))
                            .with_label(Label::new(ctx.span(&node)).with_message(
                                "expected this tuple to have at least one eval expression",
                            ))
                            .finish(),
                    );
                    return Err(err);
                }
                AccessDesc {
                    span: ctx.span(node),
                    evals,
                }
            }
            "eval_expr" => AccessDesc {
                span: ctx.span(node),
                evals: smallvec![EvalExpr::parse(ctx, dta, &first_node)?],
            },
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: node.kind().to_owned(),
                    expected: "eval_expr | \"(\"".to_owned(),
                };

                report(
                    error_reporter(err.clone(), ctx.span(&node))
                        .with_label(Label::new(ctx.span(&node)).with_message("The access description is either a single eval statement, or a tupel of eval statements"))
                        .finish(),
                );
                return Err(err);
            }
        };

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(ad)
    }
}

impl FromTreeSitter for CsgStmt {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "csg")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "csg")?;

        let csg_ident = Ident::parse(ctx, dta, &children.next().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "=")?;

        let expr = Expr::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(CsgStmt {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            decl_name: csg_ident,
            expr,
        })
    }
}

impl FromTreeSitter for ScopedCall {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "scope_call")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        let call = Call::parse(ctx, dta, &children.next().unwrap())?;

        let mut blocks = Vec::with_capacity(2);
        while let Some(next_node) = children.next() {
            blocks.push(Block::parse(ctx, dta, &next_node)?);
        }

        if blocks.len() == 0 {
            let err = ParserError::NoChildAvailable;
            report(
                error_reporter(err.clone(), ctx.span(&node))
                    .with_label(
                        Label::new(ctx.span(&node))
                            .with_message("expected at least one block after this scoped call"),
                    )
                    .finish(),
            );
            return Err(err);
        }

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(ScopedCall {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            call,
            blocks,
        })
    }
}

impl FromTreeSitter for CSGNodeDef {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        if node.kind() != "def_entity" && node.kind() != "def_operation" {
            let err = ParserError::UnexpectedAstNode {
                kind: node.kind().to_owned(),
                expected: "def_entity | def_operation".to_owned(),
            };

            report(
                error_reporter(err.clone(), ctx.span(&node))
                    .with_label(Label::new(ctx.span(&node)).with_message("in this region"))
                    .finish(),
            );
            return Err(err);
        }

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        let ty_node = children.next().unwrap();
        let ty = match ty_node.kind() {
            "entity" => {
                ParserError::consume_expected_node_string(ctx, dta, Some(ty_node), "entity")?;
                CSGNodeTy::Entity
            }
            "operation" => {
                ParserError::consume_expected_node_string(ctx, dta, Some(ty_node), "operation")?;
                CSGNodeTy::Operation
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: ty_node.kind().to_owned(),
                    expected: "entity | operation".to_owned(),
                };
                report(
                    error_reporter(err.clone(), ctx.span(&ty_node))
                        .with_label(Label::new(ctx.span(&ty_node)).with_message("in this region"))
                        .finish(),
                );
                return Err(err);
            }
        };

        //parse the identifier
        let name = Ident::parse(ctx, dta, children.next().as_ref().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "(")?;

        let mut args = SmallVec::new();
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                ")" => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ")")?;
                    break;
                }
                "typed_arg" => args.push(TypedIdent::parse(ctx, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "typed_arg | )".to_owned(),
                    };

                    report(
                        error_reporter(err.clone(), ctx.span(&next_node))
                            .with_label(
                                Label::new(ctx.span(&next_node)).with_message("in this region"),
                            )
                            .finish(),
                    );
                    return Err(err);
                }
            }

            let next_node = children.next().unwrap();
            match next_node.kind() {
                "," => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ",")?;
                }
                ")" => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ")")?;
                    break;
                }
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "\",\" or )".to_owned(),
                    };
                    report(
                        error_reporter(err.clone(), ctx.span(&next_node))
                            .with_label(
                                Label::new(ctx.span(&next_node)).with_message("in this region"),
                            )
                            .finish(),
                    );
                    return Err(err);
                }
            }
        }

        ParserError::consume_expected_node_string(ctx, dta, children.next(), ";")?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(CSGNodeDef {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            ty,
            name,
            args,
        })
    }
}

impl FromTreeSitter for CSGConcept {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "def_concept")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "concept")?;

        let name = Ident::parse(ctx, dta, children.next().as_ref().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), ":")?;

        let next_node = children.next().unwrap();
        let arg = match next_node.kind() {
            //No src ty
            "->" => {
                ParserError::consume_expected_node_string(ctx, dta, Some(next_node), "->")?;
                SmallVec::new()
            }
            //single src ty
            "alge_type" => {
                let ty = parse_alge_ty(ctx, dta, &next_node)?;
                //Consume the expected -> now
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "->")?;
                smallvec![ty]
            }
            //multiple src types
            "(" => {
                let mut tys = SmallVec::new();
                while let Some(next_node) = children.next() {
                    match next_node.kind() {
                        ")" => {
                            ParserError::consume_expected_node_string(
                                ctx,
                                dta,
                                Some(next_node),
                                ")",
                            )?;
                            break;
                        }
                        "alge_type" => tys.push(parse_alge_ty(ctx, dta, &next_node)?),
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: next_node.kind().to_owned(),
                                expected: ") | alge_ty".to_owned(),
                            };
                            report(
                                error_reporter(err.clone(), ctx.span(&next_node))
                                    .with_label(
                                        Label::new(ctx.span(&next_node))
                                            .with_message("in this region"),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    }

                    let next_node = children.next().unwrap();
                    match next_node.kind() {
                        "," => {
                            ParserError::consume_expected_node_string(
                                ctx,
                                dta,
                                Some(next_node),
                                ",",
                            )?;
                        }
                        ")" => {
                            ParserError::consume_expected_node_string(
                                ctx,
                                dta,
                                Some(next_node),
                                ")",
                            )?;
                            break;
                        }
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: next_node.kind().to_owned(),
                                expected: "\",\" or )".to_owned(),
                            };
                            report(
                                error_reporter(err.clone(), ctx.span(&next_node))
                                    .with_label(
                                        Label::new(ctx.span(&next_node))
                                            .with_message("in this region"),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    }
                }

                ParserError::consume_expected_node_string(ctx, dta, children.next(), "->")?;
                tys
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: next_node.kind().to_owned(),
                    expected: "-> | alge_ty | (".to_owned(),
                };
                report(
                    error_reporter(err.clone(), ctx.span(&next_node))
                        .with_label(Label::new(ctx.span(&next_node)).with_message("in this region"))
                        .finish(),
                );
                return Err(err);
            }
        };
        let result_ty = parse_alge_ty(ctx, dta, children.next().as_ref().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), ";")?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(CSGConcept {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            name,
            src_ty: arg,
            dst_ty: result_ty,
        })
    }
}
