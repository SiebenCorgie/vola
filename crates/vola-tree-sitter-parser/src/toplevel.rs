/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use smallvec::SmallVec;
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use vola_ast::{
    alge::{AlgeFunc, ImplBlock},
    common::{Block, CTArg, Call, Ident, Ty, TypedIdent},
    csg::{CSGConcept, CSGNodeDef, ExportFn, FieldDef},
    AstEntry, Module,
};

use crate::error::ParserError;
use crate::{FromTreeSitter, ParserCtx};

impl FromTreeSitter for AstEntry {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        //Collects all compile-time attributes. They are appended to a toplevel construct,
        // whenever needed.
        //        let mut attrib_collector = Vec::new();
        match node.kind() {
            "field_decl" => {
                let field_def = FieldDef::parse(ctx, dta, node)?;
                Ok(AstEntry::FieldDefine(field_def))
            }
            "field_export" => {
                let field_export = ExportFn::parse(ctx, dta, node)?;
                Ok(AstEntry::ExportFn(field_export))
            }
            "def_entity" | "def_operation" => {
                let def = CSGNodeDef::parse(ctx, dta, node)?;
                Ok(AstEntry::CSGNodeDef(def))
            }
            "def_concept" => {
                let def = CSGConcept::parse(ctx, dta, node)?;
                Ok(AstEntry::Concept(def))
            }
            "impl_block" => Ok(AstEntry::ImplBlock(ImplBlock::parse(ctx, dta, node)?)),
            "module" => {
                let module = Module::parse(ctx, dta, node)?;
                Ok(AstEntry::Module(module))
            }
            "fn" => {
                let fndef = AlgeFunc::parse(ctx, dta, node)?;
                Ok(AstEntry::AlgeFunc(fndef))
            }
            _ => {
                let err = ParserError::UnknownAstNode {
                    kind: node.kind().to_owned(),
                };
                report(
                    error_reporter(err.clone(), ctx.span(node))
                        .with_label(Label::new(ctx.span(node)).with_message("here"))
                        .finish(),
                );
                Err(err)
            }
        }
    }
}

impl FromTreeSitter for AlgeFunc {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "fn")?;

        let mut cursor = node.walk();
        let mut children = node.children(&mut cursor);

        ParserError::consume_expected_node_kind(ctx, children.next(), "fn")?;

        let name = Ident::parse(ctx, dta, &children.next().unwrap())?;

        ParserError::consume_expected_node_kind(ctx, children.next(), "(")?;
        let mut args = SmallVec::new();
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                //found the end
                ")" => break,
                "typed_arg" => {
                    let arg = TypedIdent::parse(ctx, dta, &next_node)?;
                    args.push(arg);
                }
                //part of the list, just ignore
                "," => {}
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "typed_arg".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(node)).finish());
                    return Err(err);
                }
            }
        }

        ParserError::consume_expected_node_kind(ctx, children.next(), "->")?;
        let return_type = Ty::parse(ctx, dta, children.next().as_ref().unwrap())?;
        let block = Block::parse(ctx, dta, children.next().as_ref().unwrap())?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(AlgeFunc {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            name,
            args,
            return_type,
            block,
        })
    }
}

impl FromTreeSitter for FieldDef {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "field_decl")?;

        let mut cursor = node.walk();
        let mut children = node.children(&mut cursor);

        ParserError::consume_expected_node_kind(ctx, children.next(), "define")?;

        let field_ident = if let Some(child) = children.next() {
            Ident::parse(ctx, dta, &child)
        } else {
            let err = ParserError::NoChildAvailable;

            report(
                error_reporter(err, ctx.span(node))
                    .with_label(Label::new(ctx.span(node)).with_message("here"))
                    .finish(),
            );
            Err(ParserError::NoChildAvailable)
        }?;

        ParserError::consume_expected_node_kind(ctx, children.next(), "(")?;
        let mut args = SmallVec::new();
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                //found the end
                ")" => break,
                "typed_arg" => {
                    let arg = TypedIdent::parse(ctx, dta, &next_node)?;
                    args.push(arg);
                }
                //part of the list, just ignore
                "," => {}
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "typed_arg".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(node)).finish());
                    return Err(err);
                }
            }
        }

        let block = Block::parse(ctx, dta, &children.next().unwrap())?;

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(FieldDef {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            name: field_ident,
            args,
            block,
        })
    }
}

impl FromTreeSitter for ExportFn {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "field_export")?;

        let mut cursor = node.walk();
        let mut children = node.children(&mut cursor);

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "export")?;

        let field_ident = Ident::parse(ctx, dta, children.next().as_ref().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "(")?;

        let mut args = SmallVec::new();

        while let Some(next_node) = children.next() {
            match next_node.kind() {
                //found the end
                ")" => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ")")?;
                    break;
                }
                "typed_arg" => {
                    let arg = TypedIdent::parse(ctx, dta, &next_node)?;
                    args.push(arg);
                }
                //part of the list, just ignore
                "," => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ",")?;
                }
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "typed_arg".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(node)).finish());
                    return Err(err);
                }
            }
        }

        let block = Block::parse(ctx, dta, &children.next().unwrap())?;

        ParserError::assert_node_no_error(ctx, node)?;
        Ok(ExportFn {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            name: field_ident,
            args,
            block,
        })
    }
}

impl FromTreeSitter for CTArg {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "ct_attrib")?;
        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "#")?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "[")?;

        //NOTE: Right now we reuse the call parser.
        let call = Call::parse(ctx, dta, &children.next().unwrap())?;
        let Call { span, ident, args } = call;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "]")?;

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(CTArg { span, ident, args })
    }
}
