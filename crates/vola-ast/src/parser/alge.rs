//! Algebra related parser portion.

use smallvec::SmallVec;
use vola_common::{CommonError, ErrorReporter, Span};

use crate::{
    alge::{AlgeExpr, AlgeExprTy, BinaryOp, FieldAccessor, LetStmt, UnaryOp},
    common::{Call, Digit, Ident, Literal},
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
        ParserError::assert_node_kind(reporter, node, "alge_expr")?;

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
                        reporter
                            .push_error(CommonError::new(Span::from(&operator_node), err.clone()));
                        return Err(err);
                    }
                };

                let sub_expr = AlgeExpr::parse(reporter, dta, &children.next().unwrap())?;

                AlgeExprTy::Unary {
                    op: unop,
                    operand: Box::new(sub_expr),
                }
            }
            "binary_expr" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                let sub_expr_l = AlgeExpr::parse(reporter, dta, &children.next().unwrap())?;

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
                        reporter
                            .push_error(CommonError::new(Span::from(&operator_node), err.clone()));
                        return Err(err);
                    }
                };

                let sub_expr_r = AlgeExpr::parse(reporter, dta, &children.next().unwrap())?;

                AlgeExprTy::Binary {
                    left: Box::new(sub_expr_l),
                    right: Box::new(sub_expr_r),
                    op: binop,
                }
            }
            "eval_expr" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);
                ParserError::consume_expected_node_kind(reporter, children.next(), "eval")?;
                let evaluated = Ident::parse(reporter, dta, &children.next().unwrap())?;
                ParserError::consume_expected_node_kind(reporter, children.next(), ")")?;

                let mut eval_params = Vec::new();
                while let Some(next_node) = children.next() {
                    match next_node.kind() {
                        ")" => break,
                        "alge_expr" => {
                            eval_params.push(AlgeExpr::parse(reporter, dta, &next_node)?)
                        }
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: node.kind().to_owned(),
                                expected: ") | alge_expr | . ".to_owned(),
                            };
                            reporter.push_error(CommonError::new(Span::from(node), err.clone()));
                            return Err(err);
                        }
                    }
                }
                ParserError::assert_ast_level_empty(reporter, children.next())?;

                AlgeExprTy::EvalExpr {
                    evaluator: evaluated,
                    params: eval_params,
                }
            }
            "(" => {
                //parse inner and return
                let inner = AlgeExpr::parse(reporter, dta, &node.child(1).unwrap())?;
                //make sure the brace is closed
                ParserError::assert_node_kind(reporter, &node.child(2).unwrap(), ")")?;
                ParserError::assert_node_no_error(reporter, node)?;
                return Ok(inner);
            }
            "integer_literal" | "float_literal" => {
                AlgeExprTy::Literal(Literal::parse(reporter, dta, &child_node)?)
            }
            "field_access" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                let src_ident = Ident::parse(reporter, dta, &children.next().unwrap())?;
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
                                    accessors.push(FieldAccessor::parse(
                                        reporter,
                                        dta,
                                        &next_field,
                                    )?);
                                }
                                _ => {
                                    let err = ParserError::UnexpectedAstNode {
                                        kind: node.kind().to_owned(),
                                        expected: "digit | identifier | .".to_owned(),
                                    };
                                    reporter.push_error(CommonError::new(
                                        Span::from(node),
                                        err.clone(),
                                    ));
                                    return Err(err);
                                }
                            }
                        }
                    } else {
                        let err = ParserError::UnexpectedAstNode {
                            kind: node.kind().to_owned(),
                            expected: "field_access_list".to_owned(),
                        };
                        reporter.push_error(CommonError::new(Span::from(node), err.clone()));
                        return Err(err);
                    }
                }

                if accessors.len() == 0 {
                    let err = ParserError::NoAccessedField;
                    reporter.push_error(CommonError::new(Span::from(&child_node), err.clone()));
                    return Err(err);
                }

                ParserError::assert_ast_level_empty(reporter, children.next())?;

                AlgeExprTy::FieldAccess {
                    src: src_ident,
                    accessors,
                }
            }
            "identifier" => AlgeExprTy::Ident(Ident::parse(reporter, dta, &child_node)?),
            "fn_call" => AlgeExprTy::Call(Box::new(Call::parse(reporter, dta, &child_node)?)),
            "list" => {
                let mut walker = child_node.walk();
                let mut children = child_node.children(&mut walker);

                ParserError::consume_expected_node_kind(reporter, children.next(), "[")?;

                let mut list = Vec::new();
                while let Some(next_node) = children.next() {
                    match next_node.kind() {
                        //ignore seperator
                        "," => {}
                        "]" => break,
                        "alge_expr" => list.push(AlgeExpr::parse(reporter, dta, &next_node)?),
                        _ => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: node.kind().to_owned(),
                                expected: ", | ] | alge_expr ".to_owned(),
                            };
                            reporter.push_error(CommonError::new(Span::from(node), err.clone()));
                            return Err(err);
                        }
                    }
                }

                ParserError::assert_ast_level_empty(reporter, children.next())?;
                AlgeExprTy::List(list)
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: node.kind().to_owned(),
                    expected: " unary expression| binary expression | eval expression | (alge expression) | literal | field access | identifier | call | list ".to_owned(),
                };
                reporter.push_error(CommonError::new(Span::from(node), err.clone()));
                return Err(err);
            }
        };

        ParserError::assert_node_no_error(reporter, node)?;
        Ok(AlgeExpr {
            span: Span::from(node),
            expr_ty,
        })
    }
}

impl FromTreeSitter for FieldAccessor {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        match node.kind() {
            "digit" => Ok(FieldAccessor::Digit {
                span: Span::from(node),
                digit: Digit::parse(reporter, dta, node)?.0,
            }),
            "identifier" => Ok(FieldAccessor::Ident {
                span: Span::from(node),
                ident: Ident::parse(reporter, dta, node)?,
            }),
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: node.kind().to_owned(),
                    expected: "digit | identifier".to_owned(),
                };
                reporter.push_error(CommonError::new(Span::from(node), err.clone()));
                Err(err)
            }
        }
    }
}

impl FromTreeSitter for LetStmt {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(reporter, node, "let_stmt")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        ParserError::consume_expected_node_kind(reporter, children.next(), "let")?;
        let ident = Ident::parse(reporter, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_kind(reporter, children.next(), "=")?;
        let alge_expr = AlgeExpr::parse(reporter, dta, &children.next().unwrap())?;

        ParserError::assert_node_no_error(reporter, node)?;
        Ok(LetStmt {
            span: Span::from(node),
            decl_name: ident,
            expr: alge_expr,
        })
    }
}
