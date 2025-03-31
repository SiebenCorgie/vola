/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use smallvec::SmallVec;
use vola_common::{Span, VolaError};

use vola_ast::{
    alge::Expr,
    common::{Block, Call, Ident, Ty, TypedIdent},
    csg::{CSGConcept, CsgDef, CsgStmt, CsgTy, ScopedCall},
};

use super::{FromTreeSitter, ParserCtx};
use crate::error::ParserError;

impl FromTreeSitter for CsgStmt {
    fn parse(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, VolaError<ParserError>>
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
    fn parse(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, VolaError<ParserError>>
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
            return Err(VolaError::error_here(
                err,
                ctx.span(node),
                "expected at least one block after this scoped call",
            ));
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

impl FromTreeSitter for CsgDef {
    fn parse(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, VolaError<ParserError>>
    where
        Self: Sized,
    {
        if node.kind() != "def_entity" && node.kind() != "def_operation" {
            let err = ParserError::UnexpectedAstNode {
                kind: node.kind().to_owned(),
                expected: "def_entity | def_operation".to_owned(),
            };
            return Err(VolaError::error_here(err, ctx.span(node), "in this region"));
        }

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        let ty_node = children.next().unwrap();
        let ty = match ty_node.kind() {
            "entity" => {
                ParserError::consume_expected_node_string(ctx, dta, Some(ty_node), "entity")?;
                CsgTy::Entity
            }
            "operation" => {
                ParserError::consume_expected_node_string(ctx, dta, Some(ty_node), "operation")?;
                CsgTy::Operation
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: ty_node.kind().to_owned(),
                    expected: "entity | operation".to_owned(),
                };
                return Err(VolaError::error_here(
                    err,
                    ctx.span(&ty_node),
                    "in this region",
                ));
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
                    return Err(VolaError::error_here(
                        err,
                        ctx.span(&next_node),
                        "in this region",
                    ));
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
                    return Err(VolaError::error_here(
                        err,
                        ctx.span(&next_node),
                        "in this region",
                    ));
                }
            }
        }

        ParserError::consume_expected_node_string(ctx, dta, children.next(), ";")?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(CsgDef {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            ty,
            name,
            args,
        })
    }
}

impl FromTreeSitter for CSGConcept {
    fn parse(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, VolaError<ParserError>>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "def_concept")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "concept")?;

        let name = Ident::parse(ctx, dta, children.next().as_ref().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), ":")?;

        let src_ty = Ty::parse(ctx, dta, children.next().as_ref().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "->")?;

        let dst_ty = Ty::parse(ctx, dta, children.next().as_ref().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), ";")?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(CSGConcept {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            name,
            src_ty,
            dst_ty,
        })
    }
}
