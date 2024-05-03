/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use smallvec::SmallVec;
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{
    alge::{AlgeExpr, AlgeFunc, AlgeStmt, ImplBlock, LetStmt},
    common::{Ident, Ty, TypedIdent},
    csg::{AccessDesc, CSGBinding, CSGConcept, CSGNodeDef, CSGOp, CSGStmt, ExportFn, FieldDef},
    error::ParserError,
    AstEntry, Module,
};

use super::{FromTreeSitter, ParserCtx};

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
                report(error_reporter(err.clone(), ctx.span(node)).finish());
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
        ParserError::consume_expected_node_kind(ctx, children.next(), "{")?;

        let block_node = children.next().unwrap();
        let mut stmts = Vec::new();
        let mut return_expr = None;

        match block_node.kind() {
            "block" => {
                let mut walker = block_node.walk();
                let mut children = block_node.children(&mut walker);

                while let Some(next_node) = children.next() {
                    match next_node.kind() {
                        "comment" => {}
                        "alge_expr" => {
                            //must be the last thing in the block
                            let ret = AlgeExpr::parse(ctx, dta, &next_node)?;
                            return_expr = Some(ret);
                            ParserError::assert_ast_level_empty(ctx, children.next())?;
                            ParserError::assert_node_no_error(ctx, &next_node)?;

                            break;
                        }
                        "let_stmt" | "assign_stmt" => {
                            stmts.push(AlgeStmt::parse(ctx, dta, &next_node)?);
                            ParserError::consume_expected_node_string(
                                ctx,
                                dta,
                                children.next(),
                                ";",
                            )?;
                        }
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: next_node.kind().to_owned(),
                                expected: "alge_stmt | alge_expr  | comment".to_owned(),
                            };
                            report(error_reporter(err.clone(), ctx.span(&next_node)).finish());
                            return Err(err);
                        }
                    }
                }

                ParserError::assert_ast_level_empty(ctx, children.next())?;
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: block_node.kind().to_owned(),
                    expected: "block".to_owned(),
                };
                report(error_reporter(err.clone(), ctx.span(&block_node)).finish());
                return Err(err);
            }
        }

        let return_expr = match return_expr {
            None => {
                let err = ParserError::NoAlgeExprAtEnd;
                report(error_reporter(err.clone(), Span::empty()).finish());
                return Err(err);
            }
            Some(r) => r,
        };

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "}")?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(AlgeFunc {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            name,
            args,
            return_type,
            stmts,
            return_expr,
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

        //We should start with a block now
        ParserError::consume_expected_node_kind(ctx, children.next(), "{")?;

        //Parse the block. We only allow csg stmts and comments, but need to end with a dangling csg stmt
        let mut stmts = Vec::new();
        let mut ret = None;
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                "comment" => {}
                "let_stmt" => {
                    //take away the ;
                    stmts.push(CSGStmt::LetStmt(LetStmt::parse(ctx, dta, &next_node)?));
                    ParserError::consume_expected_node_string(ctx, dta, children.next(), ";")?;
                }
                "csg_binding" => {
                    stmts.push(CSGStmt::CSGBinding(CSGBinding::parse(
                        ctx, dta, &next_node,
                    )?));
                    //take away the ;
                    ParserError::consume_expected_node_string(ctx, dta, children.next(), ";")?;
                }

                "csg_unary" | "csg_binary" | "fn_call" => {
                    ret = Some(CSGOp::parse(ctx, dta, &next_node)?);
                    break;
                }

                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "comment | let_stmt | csg_binding | access_decl".to_owned(),
                    };
                    report(
                        error_reporter(err.clone(), ctx.span(&next_node))
                            .with_label(
                                Label::new(ctx.span(&next_node)).with_message("Unknown syntax"),
                            )
                            .finish(),
                    );
                    return Err(err);
                }
            }
        }

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "}")?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        if ret.is_none() {
            let err = ParserError::NoCSGTreeAtDefineEnd;
            report(error_reporter(err.clone(), Span::empty()).finish());
            return Err(err);
        }

        Ok(FieldDef {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            name: field_ident,
            inputs: args,
            stmts,
            ret: ret.unwrap(),
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

        //We should start with a block now
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "{")?;

        let mut stmts = Vec::new();
        let mut access_descriptors = SmallVec::new();
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                "comment" => {}
                "let_stmt" => {
                    //take away the ;
                    stmts.push(CSGStmt::LetStmt(LetStmt::parse(ctx, dta, &next_node)?));
                    ParserError::consume_expected_node_string(ctx, dta, children.next(), ";")?;
                }
                "csg_binding" => {
                    stmts.push(CSGStmt::CSGBinding(CSGBinding::parse(
                        ctx, dta, &next_node,
                    )?));
                    //take away the ;
                    ParserError::consume_expected_node_string(ctx, dta, children.next(), ";")?;
                }

                //At this point, change into acces_decleration parsing, which must be the last part of the block
                "access_desc" => {
                    let mut access_decl_walker = next_node.walk();
                    let mut access_children = next_node.children(&mut access_decl_walker);
                    while let Some(next_node) = access_children.next() {
                        match next_node.kind() {
                            //start and , can be ignored
                            "(" => {
                                ParserError::consume_expected_node_string(
                                    ctx,
                                    dta,
                                    Some(next_node),
                                    "(",
                                )?;
                            }
                            "," => {
                                ParserError::consume_expected_node_string(
                                    ctx,
                                    dta,
                                    Some(next_node),
                                    ",",
                                )?;
                            }
                            "access_decl" => {
                                access_descriptors.push(AccessDesc::parse(ctx, dta, &next_node)?);
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
                                    expected: "access_decl | , | (".to_owned(),
                                };
                                report(error_reporter(err.clone(), ctx.span(node)).finish());
                                return Err(err);
                            }
                        }
                    }

                    ParserError::assert_ast_level_empty(ctx, access_children.next())?;
                    break;
                }
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "comment | let_stmt | csg_binding | access_decl".to_owned(),
                    };
                    report(
                        error_reporter(err.clone(), ctx.span(node))
                            .with_label(
                                Label::new(ctx.span(&next_node)).with_message("Unknown syntax"),
                            )
                            .finish(),
                    );
                    return Err(err);
                }
            }
        }

        //check that we acutally are at the end
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "}")?;

        if access_descriptors.len() == 0 {
            let err = ParserError::NoAccessDecs;
            report(error_reporter(err.clone(), Span::empty()).finish());
            return Err(err);
        }

        ParserError::assert_node_no_error(ctx, node)?;
        Ok(ExportFn {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            name: field_ident,
            inputs: args,
            stmts,
            access_descriptors,
        })
    }
}
