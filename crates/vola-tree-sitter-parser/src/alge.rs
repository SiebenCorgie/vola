/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Algebra related parser portion.

use smallvec::SmallVec;
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use vola_ast::{
    alge::{
        AssignStmt, BinaryOp, EvalExpr, Expr, ExprTy, FieldAccessor, ImplBlock, LetStmt, UnaryOp,
    },
    common::{Block, Call, Digit, GammaExpr, Ident, Literal, Stmt, ThetaExpr},
    csg::{AccessDesc, CsgStmt, ScopedCall},
};

use super::{FromTreeSitter, ParserCtx};
use crate::error::ParserError;

impl FromTreeSitter for Expr {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "expr")?;

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
                        report(error_reporter(err.clone(), ctx.span(&operator_node)).finish());
                        return Err(err);
                    }
                };

                let sub_expr = Expr::parse(ctx, dta, &children.next().unwrap())?;

                ExprTy::Unary {
                    op: unop,
                    operand: Box::new(sub_expr),
                }
            }
            "binary_expr" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                let sub_expr_l = Expr::parse(ctx, dta, &children.next().unwrap())?;

                let operator_node = children.next().unwrap();
                let binop = match operator_node.kind() {
                    "+" => BinaryOp::Add,
                    "-" => BinaryOp::Sub,
                    "*" => BinaryOp::Mul,
                    "/" => BinaryOp::Div,
                    "%" => BinaryOp::Mod,

                    "<" => BinaryOp::Lt,
                    ">" => BinaryOp::Gt,
                    "<=" => BinaryOp::Lte,
                    ">=" => BinaryOp::Gte,
                    "==" => BinaryOp::Eq,
                    "!=" => BinaryOp::NotEq,

                    "||" => BinaryOp::Or,
                    "&&" => BinaryOp::And,
                    _ => {
                        let err = ParserError::UnknownError(format!(
                            "Unknown Binary operation {}",
                            operator_node.kind()
                        ));
                        report(
                            error_reporter(err.clone(), ctx.span(&operator_node))
                                .with_label(
                                    Label::new(ctx.span(&operator_node)).with_message("here"),
                                )
                                .finish(),
                        );
                        return Err(err);
                    }
                };

                let sub_expr_r = Expr::parse(ctx, dta, &children.next().unwrap())?;

                ExprTy::Binary {
                    left: Box::new(sub_expr_l),
                    right: Box::new(sub_expr_r),
                    op: binop,
                }
            }
            "eval_expr" => ExprTy::EvalExpr(EvalExpr::parse(ctx, dta, &child_node)?),
            "(" => {
                //parse inner and return
                let inner = Expr::parse(ctx, dta, &node.child(1).unwrap())?;
                //make sure the brace is closed
                ParserError::assert_node_kind(ctx, &node.child(2).unwrap(), ")")?;
                ParserError::assert_node_no_error(ctx, node)?;
                return Ok(inner);
            }
            "digit" | "integer_literal" | "float_literal" => {
                ExprTy::Literal(Literal::parse(ctx, dta, &child_node)?)
            }
            "field_access" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                let src_ident = Ident::parse(ctx, dta, &children.next().unwrap())?;
                let mut accessors = SmallVec::new();

                //itterate over all following nodes interpreting them either as ident or digit
                while let Some(next_node) = children.next() {
                    //next should always be a "."
                    match next_node.kind() {
                        "." => {}
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: next_node.kind().to_owned(),
                                expected: "\".\"".to_owned(),
                            };

                            report(
                                error_reporter(err.clone(), ctx.span(&next_node))
                                    .with_label(
                                        Label::new(ctx.span(&next_node))
                                            .with_message("Expected this to be a \".\""),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    }
                    //if there was a dot, a digit or identfier has to follow.
                    //if there wasn't a node, the out loop would have aborted already.
                    let next_node = children.next().unwrap();
                    match next_node.kind() {
                        "digit" | "identifier" => {
                            accessors.push(FieldAccessor::parse(ctx, dta, &next_node)?);
                        }
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: next_node.kind().to_owned(),
                                expected: "digit | identifier | .".to_owned(),
                            };

                            report(
                                error_reporter(err.clone(), ctx.span(&next_node))
                                    .with_label(Label::new(ctx.span(&next_node)).with_message(
                                        "Expected this to be either a digit, or an identifier",
                                    ))
                                    .finish(),
                            );
                            return Err(err);
                        }
                    }
                }

                if accessors.len() == 0 {
                    let err = ParserError::NoAccessedField;
                    report(error_reporter(err.clone(), Span::empty()).finish());
                    return Err(err);
                }

                ParserError::assert_ast_level_empty(ctx, children.next())?;

                ExprTy::FieldAccess {
                    src: src_ident,
                    accessors,
                }
            }
            "identifier" => ExprTy::Ident(Ident::parse(ctx, dta, &child_node)?),
            "fn_call" => ExprTy::Call(Box::new(Call::parse(ctx, dta, &child_node)?)),
            "scope_call" => ExprTy::ScopedCall(Box::new(ScopedCall::parse(ctx, dta, &child_node)?)),
            "access_desc" => ExprTy::AccessExpr(AccessDesc::parse(ctx, dta, &child_node)?),
            "list" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                ParserError::consume_expected_node_string(ctx, dta, children.next(), "[")?;

                let mut list = Vec::new();
                while let Some(next_node) = children.next() {
                    //First must be an alge expr.
                    match next_node.kind() {
                        "expr" => list.push(Expr::parse(ctx, dta, &next_node)?),
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: next_node.kind().to_owned(),
                                expected: "expr ".to_owned(),
                            };
                            report(
                                error_reporter(err.clone(), ctx.span(&next_node))
                                    .with_label(
                                        Label::new(ctx.span(&next_node))
                                            .with_message("Should be an algebraic expression"),
                                    )
                                    .finish(),
                            );
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
                                val: next_node
                                    .utf8_text(dta)
                                    .unwrap_or("couldn't parse")
                                    .to_owned(),
                                exp: ", or ] ".to_owned(),
                            };
                            report(
                                error_reporter(err.clone(), ctx.span(&next_node))
                                    .with_label(
                                        Label::new(ctx.span(&next_node))
                                            .with_message("Should be \",\" or \"]\""),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    }
                }

                ParserError::assert_ast_level_empty(ctx, children.next())?;
                ExprTy::List(list)
            }
            "gamma_expr" => ExprTy::GammaExpr(Box::new(GammaExpr::parse(ctx, dta, &child_node)?)),
            "theta_expr" => ExprTy::ThetaExpr(Box::new(ThetaExpr::parse(ctx, dta, &child_node)?)),
            "splat_expr" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);
                //Parse the _list_ of [expr ; count].
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "[")?;
                let subexpr = Expr::parse(ctx, dta, &children.next().unwrap())?;
                ParserError::consume_expected_node_string(ctx, dta, children.next(), ";")?;
                let count = Digit::parse(ctx, dta, &children.next().unwrap())?;
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "]")?;
                ParserError::assert_ast_level_empty(ctx, children.next())?;

                ExprTy::SplatExpr {
                    expr: Box::new(subexpr),
                    count: count.0,
                }
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: child_node.kind().to_owned(),
                    expected: "unary expression| binary expression | eval expression | (alge expression) | literal | field access | identifier | call | list | gamma_expr | theta_expr | splat_expr ".to_owned(),
                };
                report(
                    error_reporter(err.clone(), ctx.span(&child_node))
                        .with_label(Label::new(ctx.span(&child_node)).with_message("here"))
                        .finish(),
                );
                return Err(err);
            }
        };

        ParserError::assert_node_no_error(ctx, node)?;
        Ok(Expr {
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
                    kind: node.kind().to_owned(),
                    expected: "digit | identifier".to_owned(),
                };
                report(error_reporter(err.clone(), ctx.span(&node)).finish());
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
        ParserError::assert_node_kind(ctx, node, "let")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "let")?;
        let ident = Ident::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "=")?;
        let alge_expr = Expr::parse(ctx, dta, &children.next().unwrap())?;

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
        ParserError::assert_node_kind(ctx, node, "assign")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        let ident = Ident::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "=")?;
        let alge_expr = Expr::parse(ctx, dta, &children.next().unwrap())?;

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
                "expr" => eval_params.push(Expr::parse(ctx, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: ") | expr  ".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(&next_node)).finish());
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
                        expected: ") | , ".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(&next_node)).finish());
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
                                kind: next_node.kind().to_owned(),
                                expected: "identifier".to_owned(),
                            };
                            report(error_reporter(err.clone(), ctx.span(&next_node)).finish());
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
                                kind: next_node.kind().to_owned(),
                                expected: " , or >".to_owned(),
                            };
                            report(error_reporter(err.clone(), ctx.span(&next_node)).finish());
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
                    kind: next_node.kind().to_owned(),
                    expected: "for | < ".to_owned(),
                };
                report(
                    error_reporter(err.clone(), ctx.span(&next_node))
                        .with_label(Label::new(ctx.span(&next_node)).with_message("here"))
                        .finish(),
                );
                return Err(err);
            }
        };

        let concept = Ident::parse(ctx, dta, children.next().as_ref().unwrap())?;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "(")?;
        let mut concept_arg_naming = SmallVec::new();
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                //break the arg regnaming
                ")" => break,
                "identifier" => concept_arg_naming.push(Ident::parse(ctx, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: " ) or identifier ".to_owned(),
                    };
                    report(
                        error_reporter(err.clone(), ctx.span(&next_node))
                            .with_label(Label::new(ctx.span(&next_node)).with_message("here"))
                            .finish(),
                    );
                    return Err(err);
                }
            }

            //now there should be either a "," or end to the identifier list
            let next_node = children.next().unwrap();
            match next_node.kind() {
                ")" => break,
                "," => ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ",")?,
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: " ) or , ".to_owned(),
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

        //Now parse the block
        let block = Block::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(ImplBlock {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            dst,
            operands,
            concept,
            concept_arg_naming,
            block,
        })
    }
}

impl FromTreeSitter for Stmt {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "stmt")?;
        let stmtnode = node.child(0).unwrap();
        let stmt = match stmtnode.kind() {
            "let" => {
                let stmt = Self::Let(LetStmt::parse(ctx, dta, &stmtnode)?);
                ParserError::consume_expected_node_string(ctx, dta, node.child(1), ";")?;
                stmt
            }
            "assign" => {
                let stmt = Self::Assign(AssignStmt::parse(ctx, dta, &stmtnode)?);
                ParserError::consume_expected_node_string(ctx, dta, node.child(1), ";")?;
                stmt
            }
            "csg" => {
                let stmt = Self::Csg(CsgStmt::parse(ctx, dta, &stmtnode)?);
                ParserError::consume_expected_node_string(ctx, dta, node.child(1), ";")?;
                stmt
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: stmtnode.kind().to_owned(),
                    expected: "let_stmt | assign_stmt | csg_stmt | gamma_expr | theta_expr"
                        .to_owned(),
                };
                report(error_reporter(err.clone(), ctx.span(&stmtnode)).finish());
                return Err(err);
            }
        };

        ParserError::assert_node_no_error(ctx, node)?;

        Ok(stmt)
    }
}
