//! Algebra related parser portion.

use smallvec::SmallVec;
use vola_common::{report, Span};

use crate::{
    alge::{
        AlgeExpr, AlgeExprTy, AlgeStmt, AssignStmt, BinaryOp, EvalExpr, FieldAccessor, ImplBlock,
        LetStmt, UnaryOp,
    },
    common::{Call, Digit, Ident, Literal},
    error::ParserError,
};

use super::{FromTreeSitter, ParserCtx};

impl FromTreeSitter for AlgeExpr {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "alge_expr")?;

        let child_node = node.child(0).unwrap();
        let expr_ty = match child_node.kind() {
            "unary_expr" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                let operator_node = children.next().unwrap();
                let unop = match operator_node.kind() {
                    "-" => UnaryOp::Neg,
                    "!" => UnaryOp::Not,
                    _ => {
                        let err = ParserError::UnknownError(format!(
                            "Unknown Unary operation {}",
                            operator_node.kind()
                        ));
                        report(err.clone(), ctx.get_file());
                        return Err(err);
                    }
                };

                let sub_expr = AlgeExpr::parse(ctx, dta, &children.next().unwrap())?;

                AlgeExprTy::Unary {
                    op: unop,
                    operand: Box::new(sub_expr),
                }
            }
            "binary_expr" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                let sub_expr_l = AlgeExpr::parse(ctx, dta, &children.next().unwrap())?;

                let operator_node = children.next().unwrap();
                let binop = match operator_node.kind() {
                    "+" => BinaryOp::Add,
                    "-" => BinaryOp::Sub,
                    "*" => BinaryOp::Mul,
                    "/" => BinaryOp::Div,
                    "%" => BinaryOp::Mod,
                    _ => {
                        let err = ParserError::UnknownError(format!(
                            "Unknown Binary operation {}",
                            operator_node.kind()
                        ));
                        report(err.clone(), ctx.get_file());
                        return Err(err);
                    }
                };

                let sub_expr_r = AlgeExpr::parse(ctx, dta, &children.next().unwrap())?;

                AlgeExprTy::Binary {
                    left: Box::new(sub_expr_l),
                    right: Box::new(sub_expr_r),
                    op: binop,
                }
            }
            "eval_expr" => AlgeExprTy::EvalExpr(EvalExpr::parse(ctx, dta, &child_node)?),
            "(" => {
                //parse inner and return
                let inner = AlgeExpr::parse(ctx, dta, &node.child(1).unwrap())?;
                //make sure the brace is closed
                ParserError::assert_node_kind(ctx, &node.child(2).unwrap(), ")")?;
                ParserError::assert_node_no_error(ctx, node)?;
                return Ok(inner);
            }
            "integer_literal" | "float_literal" => {
                AlgeExprTy::Literal(Literal::parse(ctx, dta, &child_node)?)
            }
            "field_access" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                let src_ident = Ident::parse(ctx, dta, &children.next().unwrap())?;
                let mut accessors = SmallVec::new();

                if let Some(next_node) = children.next() {
                    if next_node.kind() == "field_access_list" {
                        let mut fields_walker = next_node.walk();
                        let mut fields = next_node.children(&mut fields_walker);
                        while let Some(next_field) = fields.next() {
                            match next_field.kind() {
                                //ignore dots
                                "." => {}
                                "digit" | "identifier" => {
                                    accessors.push(FieldAccessor::parse(ctx, dta, &next_field)?);
                                }
                                _ => {
                                    let err = ParserError::UnexpectedAstNode {
                                        span: ctx.span(&next_field).into(),
                                        kind: next_field.kind().to_owned(),
                                        expected: "digit | identifier | .".to_owned(),
                                    };
                                    report(err.clone(), ctx.get_file());
                                    return Err(err);
                                }
                            }
                        }
                    } else {
                        let err = ParserError::UnexpectedAstNode {
                            span: ctx.span(&next_node).into(),
                            kind: next_node.kind().to_owned(),
                            expected: "field_access_list".to_owned(),
                        };
                        report(err.clone(), ctx.get_file());
                        return Err(err);
                    }
                }

                if accessors.len() == 0 {
                    let err = ParserError::NoAccessedField;
                    report(err.clone(), ctx.get_file());
                    return Err(err);
                }

                ParserError::assert_ast_level_empty(ctx, children.next())?;

                AlgeExprTy::FieldAccess {
                    src: src_ident,
                    accessors,
                }
            }
            "identifier" => AlgeExprTy::Ident(Ident::parse(ctx, dta, &child_node)?),
            "fn_call" => AlgeExprTy::Call(Box::new(Call::parse(ctx, dta, &child_node)?)),
            "list" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                ParserError::consume_expected_node_string(ctx, dta, children.next(), "[")?;

                let mut list = Vec::new();
                while let Some(next_node) = children.next() {
                    //First must be an alge expr.
                    match next_node.kind() {
                        "alge_expr" => list.push(AlgeExpr::parse(ctx, dta, &next_node)?),
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                span: ctx.span(&next_node).into(),
                                kind: next_node.kind().to_owned(),
                                expected: "alge_expr ".to_owned(),
                            };
                            report(err.clone(), ctx.get_file());
                            return Err(err);
                        }
                    }

                    //It follows either a ] or a ,
                    let next_node = children.next().unwrap();
                    match next_node.utf8_text(dta).unwrap() {
                        //ignore seperator
                        "," => {}
                        "]" => break,
                        _ => {
                            let err = ParserError::UnexpectedNodeValue {
                                span: ctx.span(&next_node).into(),
                                val: next_node
                                    .utf8_text(dta)
                                    .unwrap_or("couldn't parse")
                                    .to_owned(),
                                exp: ", or ] ".to_owned(),
                            };
                            report(err.clone(), ctx.get_file());
                            return Err(err);
                        }
                    }
                }

                ParserError::assert_ast_level_empty(ctx, children.next())?;
                AlgeExprTy::List(list)
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    span: ctx.span(&child_node).into(),
                    kind: child_node.kind().to_owned(),
                    expected: " unary expression| binary expression | eval expression | (alge expression) | literal | field access | identifier | call | list ".to_owned(),
                };
                report(err.clone(), ctx.get_file());
                return Err(err);
            }
        };

        ParserError::assert_node_no_error(ctx, node)?;
        Ok(AlgeExpr {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            expr_ty,
        })
    }
}

impl FromTreeSitter for FieldAccessor {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        match node.kind() {
            "digit" => Ok(FieldAccessor::Digit {
                span: Span::from(node).with_file_maybe(ctx.get_file()),
                digit: Digit::parse(ctx, dta, node)?.0,
            }),
            "identifier" => Ok(FieldAccessor::Ident {
                span: Span::from(node).with_file_maybe(ctx.get_file()),
                ident: Ident::parse(ctx, dta, node)?,
            }),
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    span: ctx.span(&node).into(),
                    kind: node.kind().to_owned(),
                    expected: "digit | identifier".to_owned(),
                };
                report(err.clone(), ctx.get_file());
                Err(err)
            }
        }
    }
}

impl FromTreeSitter for LetStmt {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "let_stmt")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "let")?;
        let ident = Ident::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "=")?;
        let alge_expr = AlgeExpr::parse(ctx, dta, &children.next().unwrap())?;

        ParserError::assert_node_no_error(ctx, node)?;
        Ok(LetStmt {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            decl_name: ident,
            expr: alge_expr,
        })
    }
}

impl FromTreeSitter for AssignStmt {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "assign_stmt")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        let ident = Ident::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "=")?;
        let alge_expr = AlgeExpr::parse(ctx, dta, &children.next().unwrap())?;

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(AssignStmt {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            dst: ident,
            expr: alge_expr,
        })
    }
}

impl FromTreeSitter for EvalExpr {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "eval_expr")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "eval")?;
        let evaluator = Ident::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), ".")?;
        let concept = Ident::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "(")?;

        let mut eval_params = Vec::new();
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                ")" => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ")")?;
                    break;
                }
                "alge_expr" => eval_params.push(AlgeExpr::parse(ctx, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        span: ctx.span(&next_node).into(),
                        kind: next_node.kind().to_owned(),
                        expected: ") | alge_expr  ".to_owned(),
                    };
                    report(err.clone(), ctx.get_file());
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
                        span: ctx.span(&next_node).into(),
                        kind: next_node.kind().to_owned(),
                        expected: ") | , ".to_owned(),
                    };
                    report(err.clone(), ctx.get_file());
                    return Err(err);
                }
            }
        }
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(EvalExpr {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            evaluator,
            concept,
            params: eval_params,
        })
    }
}

impl FromTreeSitter for ImplBlock {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "impl_block")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "impl")?;
        let dst = Ident::parse(ctx, dta, children.next().as_ref().unwrap())?;

        //Parse operands
        let next_node = children.next().unwrap();

        let operands = match next_node.kind() {
            "<" => {
                let mut operands = SmallVec::new();
                while let Some(next_node) = children.next() {
                    match next_node.kind() {
                        "identifier" => operands.push(Ident::parse(ctx, dta, &next_node)?),
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                span: ctx.span(&next_node).into(),
                                kind: next_node.kind().to_owned(),
                                expected: "identifier".to_owned(),
                            };
                            report(err.clone(), ctx.get_file());
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
                        ">" => {
                            ParserError::consume_expected_node_string(
                                ctx,
                                dta,
                                Some(next_node),
                                ">",
                            )?;
                            break;
                        }
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                span: ctx.span(&next_node).into(),
                                kind: next_node.kind().to_owned(),
                                expected: " , or >".to_owned(),
                            };
                            report(err.clone(), ctx.get_file());
                            return Err(err);
                        }
                    }
                }

                ParserError::consume_expected_node_string(ctx, dta, children.next(), "for")?;
                operands
            }
            "for" => SmallVec::new(),
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    span: ctx.span(&next_node).into(),
                    kind: next_node.kind().to_owned(),
                    expected: "for | < ".to_owned(),
                };
                report(err.clone(), ctx.get_file());
                return Err(err);
            }
        };

        let concept = Ident::parse(ctx, dta, children.next().as_ref().unwrap())?;

        //Parse the renaming if there is any
        let next_node = children.next().unwrap();
        let concept_arg_naming = match next_node.kind() {
            "(" => {
                let mut renaming = SmallVec::new();
                while let Some(next_node) = children.next() {
                    match next_node.kind() {
                        "identifier" => renaming.push(Ident::parse(ctx, dta, &next_node)?),
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                span: ctx.span(&next_node).into(),
                                kind: next_node.kind().to_owned(),
                                expected: "identifier".to_owned(),
                            };
                            report(err.clone(), ctx.get_file());
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
                                span: ctx.span(&next_node).into(),
                                kind: next_node.kind().to_owned(),
                                expected: " , or ) ".to_owned(),
                            };
                            report(err.clone(), ctx.get_file());
                            return Err(err);
                        }
                    }
                }

                ParserError::consume_expected_node_string(ctx, dta, children.next(), "{")?;
                renaming
            }
            "{" => SmallVec::new(),
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    span: ctx.span(&next_node).into(),
                    kind: next_node.kind().to_owned(),
                    expected: "{ | ( ".to_owned(),
                };
                report(err.clone(), ctx.get_file());
                return Err(err);
            }
        };

        //Now parse the block

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
                                span: ctx.span(&next_node).into(),
                                kind: next_node.kind().to_owned(),
                                expected: "alge_stmt | alge_expr  | comment".to_owned(),
                            };
                            report(err.clone(), ctx.get_file());
                            return Err(err);
                        }
                    }
                }

                ParserError::assert_ast_level_empty(ctx, children.next())?;
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    span: ctx.span(&block_node).into(),
                    kind: block_node.kind().to_owned(),
                    expected: "block".to_owned(),
                };
                report(err.clone(), ctx.get_file());
                return Err(err);
            }
        }

        let return_expr = match return_expr {
            None => {
                let err = ParserError::NoAlgeExprAtEnd;
                report(err.clone(), ctx.get_file());
                return Err(err);
            }
            Some(r) => r,
        };

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "}")?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        /*
                println!(
                    "Parsed:\ndst {:?}\noperands: {:?}\nconcept: {:?}\nargs: {:?}\nstmts: {:#?}",
                    dst, operands, concept, concept_arg_naming, stmts
                );
        */

        Ok(ImplBlock {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            dst,
            operands,
            concept,
            concept_arg_naming,
            stmts,
            return_expr,
        })
    }
}

impl FromTreeSitter for AlgeStmt {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        match node.kind() {
            "let_stmt" => Ok(Self::Let(LetStmt::parse(ctx, dta, node)?)),
            "assign_stmt" => Ok(Self::Assign(AssignStmt::parse(ctx, dta, node)?)),
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    span: ctx.span(&node).into(),
                    kind: node.kind().to_owned(),
                    expected: "let_stmt | assign_stmt".to_owned(),
                };
                report(err.clone(), ctx.get_file());
                return Err(err);
            }
        }
    }
}
