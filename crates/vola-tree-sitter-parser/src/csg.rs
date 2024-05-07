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
    alge::AlgeExpr,
    common::{Call, Ident, TypedIdent},
    csg::{AccessDesc, CSGBinding, CSGConcept, CSGNodeDef, CSGNodeTy, CSGOp},
};

use super::{FromTreeSitter, ParserCtx};
use crate::{common::parse_alge_ty, error::ParserError};

impl FromTreeSitter for AccessDesc {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "access_decl")?;

        let src_tree = Ident::parse(ctx, dta, &node.child(0).unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, node.child(1), ".")?;
        let concept_call = Call::parse(ctx, dta, &node.child(2).unwrap())?;

        ParserError::assert_node_no_error(ctx, node)?;
        Ok(AccessDesc {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            tree_ref: src_tree,
            call: concept_call,
        })
    }
}

impl FromTreeSitter for CSGBinding {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "csg_binding")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "csg")?;

        let csg_ident = Ident::parse(ctx, dta, &children.next().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "=")?;

        let tree = CSGOp::parse(ctx, dta, &children.next().unwrap())?;

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(CSGBinding {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            decl_name: csg_ident,
            tree,
        })
    }
}

impl FromTreeSitter for CSGOp {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        if node.kind() != "csg_unary"
            && node.kind() != "csg_binary"
            && node.kind() != "fn_call"
            && node.kind() != "identifier"
        {
            let err = ParserError::UnexpectedAstNode {
                kind: node.kind().to_owned(),
                expected: "csg_unary | csg_binary | fn_call | identifier".to_owned(),
            };

            report(
                error_reporter(err.clone(), ctx.span(&node))
                    .with_label(Label::new(ctx.span(&node)).with_message("On this string"))
                    .finish(),
            );
            return Err(err);
        }

        //Right now there are three kinds of CSGOPs
        // 1. csg_unary / single sub tree,
        // 2. csg_binary / two sub trees,
        // 3. fn_call / primitive call,
        //
        // NOTE: This might change at some point, if we want to allow an arbitrary amount of sub-trees.
        //       But for now we mirror miniSDF's language in this point.
        //
        //
        // Parsing wise, we always first parse a fn_call structure, then, depending on the kind
        // either 0, 1 or 2 sub trees.

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        //If this is a identifier only call, early return with just that.
        // This is the case when referencing a local variable.
        if node.kind() == "identifier" {
            let ident = Ident::parse(ctx, dta, node)?;
            ParserError::assert_ast_level_empty(ctx, children.next())?;
            ParserError::assert_node_no_error(ctx, node)?;
            return Ok(CSGOp {
                span: Span::from(node).with_file_maybe(ctx.get_file()),
                op: ident,
                args: SmallVec::new(),
                sub_trees: Vec::with_capacity(0),
                is_local_reference: true,
            });
        }

        let ident = Ident::parse(ctx, dta, &children.next().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "(")?;

        let mut params = SmallVec::new();
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                ")" => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ")")?;
                    break;
                }
                "alge_expr" => params.push(AlgeExpr::parse(ctx, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "alge_expr".to_owned(),
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
                ")" => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ")")?;
                    break;
                }
                "," => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ",")?;
                }
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: " \",\" or )".to_owned(),
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

        //now, depending on the actual node kind, parse one, two or zero sub trees
        let sub_trees = match node.kind() {
            "csg_unary" => {
                let mut subtrees = Vec::with_capacity(1);
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "{")?;
                subtrees.push(CSGOp::parse(ctx, dta, &children.next().unwrap())?);
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "}")?;
                subtrees
            }
            "csg_binary" => {
                let mut subtrees = Vec::with_capacity(2);
                //left tree
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "{")?;
                subtrees.push(CSGOp::parse(ctx, dta, &children.next().unwrap())?);
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "}")?;
                //right tree
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "{")?;
                subtrees.push(CSGOp::parse(ctx, dta, &children.next().unwrap())?);
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "}")?;

                subtrees
            }
            "fn_call" => Vec::with_capacity(0),
            _ => {
                //NOTE this should error at fn entry already.
                panic!("Failed at subtree parsing, should error earlier");
            }
        };

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(CSGOp {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            op: ident,
            args: params,
            sub_trees,
            is_local_reference: false,
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
