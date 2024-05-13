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
    alge::Expr,
    common::{Block, Call, Digit, GammaExpr, Ident, Literal, Ty, TypedIdent},
    Module,
};

use crate::{error::ParserError, FromTreeSitter, ParserCtx};

impl FromTreeSitter for Ident {
    fn parse(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, crate::error::ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "identifier")?;
        let ident = match node.utf8_text(dta) {
            Ok(parsed) => {
                if parsed.is_empty() {
                    let err = ParserError::EmptyParse {
                        kind: "identifier".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(node)).finish());
                    return Err(err);
                }
                parsed
            }
            Err(e) => {
                let err = ParserError::Utf8ParseError(e);
                report(error_reporter(err.clone(), Span::empty()).finish());
                return Err(err);
            }
        };

        Ok(Ident(ident.to_owned()))
    }
}

impl FromTreeSitter for Digit {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "digit")?;
        let node_text = match node.utf8_text(dta) {
            Err(e) => {
                let err = ParserError::Utf8ParseError(e);
                report(error_reporter(err.clone(), Span::empty()).finish());
                return Err(err);
            }
            Ok(s) => s.to_owned(),
        };

        if node_text.is_empty() {
            let err = ParserError::EmptyParse {
                kind: "digit".to_owned(),
            };
            report(
                error_reporter(err.clone(), ctx.span(node))
                    .with_label(Label::new(ctx.span(node)).with_message("here"))
                    .finish(),
            );
            return Err(err);
        }

        let int: usize = match node_text.parse() {
            Ok(f) => f,
            Err(e) => {
                let err = ParserError::ParseIntLiteral(e);
                report(error_reporter(err.clone(), Span::empty()).finish());
                return Err(err);
            }
        };

        Ok(Digit(int))
    }
}

impl FromTreeSitter for Ty {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        let node_text = match node.utf8_text(dta) {
            Err(e) => {
                let err = ParserError::Utf8ParseError(e);
                report(error_reporter(err.clone(), Span::empty()).finish());
                return Err(err);
            }
            Ok(s) => s.to_owned(),
        };

        if node_text.is_empty() {
            let err = ParserError::EmptyParse {
                kind: "type".to_owned(),
            };
            report(error_reporter(err.clone(), ctx.span(node)).finish());
            return Err(err);
        }

        //First check for the abstract "CSG" type
        if node_text == "CSG" {
            return Ok(Ty::CSGTree);
        }
        //Otherwise this should be a alge_type
        parse_alge_ty(ctx, dta, node)
    }
}

pub fn parse_alge_ty(
    ctx: &mut ParserCtx,
    dta: &[u8],
    node: &tree_sitter::Node,
) -> Result<Ty, ParserError> {
    ParserError::assert_node_kind(ctx, node, "alge_type")?;

    match node.child(0).unwrap().kind() {
        "t_scalar" => {
            //Scalar is easy
            Ok(Ty::Scalar)
        }
        "t_nat" => Ok(Ty::Nat),
        "t_vec" => {
            //try to parse the first digit
            let width = Digit::parse(ctx, dta, &node.child(0).unwrap().child(1).unwrap())?;
            Ok(Ty::Vec { width: width.0 })
        }
        "t_mat" => {
            let width = Digit::parse(ctx, dta, &node.child(0).unwrap().child(1).unwrap())?;
            ParserError::consume_expected_node_kind(
                ctx,
                node.child(0).unwrap().child(2).clone(),
                "x",
            )?;
            let height = Digit::parse(ctx, dta, &node.child(0).unwrap().child(3).unwrap())?;

            Ok(Ty::Matrix {
                width: width.0,
                height: height.0,
            })
        }
        "t_tensor" => {
            let mut walker = node.child(0).unwrap().walk();
            let mut children = node.child(0).unwrap().children(&mut walker);

            ParserError::consume_expected_node_kind(ctx, children.next(), "tensor")?;
            ParserError::consume_expected_node_kind(ctx, children.next(), "<")?;

            let mut dim = SmallVec::new();
            while let Some(next_node) = children.next() {
                match next_node.kind() {
                    "digit" => dim.push(Digit::parse(ctx, dta, &next_node)?.0),
                    x => {
                        let err = ParserError::UnexpectedAstNode {
                            kind: x.to_owned(),
                            expected: "digit or \">\"".to_owned(),
                        };
                        report(error_reporter(err.clone(), Span::empty()).finish());
                        return Err(err);
                    }
                };

                if let Some(next) = children.next() {
                    match next.kind() {
                        "," => {}
                        ">" => {
                            break;
                        }
                        x => {
                            let err = ParserError::UnexpectedAstNode {
                                kind: x.to_owned(),
                                expected: ", or \">\"".to_owned(),
                            };
                            report(error_reporter(err.clone(), ctx.span(node)).finish());
                            return Err(err);
                        }
                    }
                }
            }

            ParserError::assert_ast_level_empty(ctx, children.next())?;

            Ok(Ty::Tensor { dim })
        }
        _ => {
            let err = ParserError::UnexpectedAstNode {
                kind: node.child(0).unwrap().kind().to_string(),
                expected: "t_scalar | t_vec | t_mat | t_tensor".to_owned(),
            };
            report(error_reporter(err.clone(), ctx.span(node)).finish());
            Err(err)
        }
    }
}

impl FromTreeSitter for Literal {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        match node.kind() {
            "float_literal" => {
                //NOTE: Right now we use the rust str->f64 parser instead of parsing both digits.
                // In theory this is slightly different to the actual float-literal definition. But
                // is a nice shortcut for now, to catch malformed floats.

                let node_text = match node.utf8_text(dta) {
                    Err(e) => {
                        let err = ParserError::Utf8ParseError(e);
                        report(error_reporter(err.clone(), Span::empty()).finish());
                        return Err(err);
                    }
                    Ok(s) => s.to_owned(),
                };

                if node_text.is_empty() {
                    let err = ParserError::EmptyParse {
                        kind: "float".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(node)).finish());
                    return Err(err);
                }

                let float: f64 = match node_text.parse() {
                    Ok(f) => f,
                    Err(e) => {
                        let err = ParserError::ParseFloatLiteral(e);
                        report(error_reporter(err.clone(), Span::empty()).finish());
                        return Err(err);
                    }
                };

                ParserError::assert_node_no_error(ctx, node)?;
                Ok(Literal::FloatLiteral(float))
            }
            "integer_literal" => {
                //reuse the digit parser, but unwrap the value
                let digit = Digit::parse(ctx, dta, &node.child(0).unwrap())?;
                ParserError::assert_node_no_error(ctx, node)?;
                Ok(Literal::IntegerLiteral(digit.0))
            }
            "digit" => {
                let digit = Digit::parse(ctx, dta, &node)?;
                ParserError::assert_node_no_error(ctx, node)?;
                Ok(Literal::IntegerLiteral(digit.0))
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: node.kind().to_string(),
                    expected: "integer_literal | float_literal".to_owned(),
                };
                report(error_reporter(err.clone(), ctx.span(node)).finish());
                Err(err)
            }
        }
    }
}
impl FromTreeSitter for TypedIdent {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "typed_arg")?;
        let ident = Ident::parse(ctx, dta, node.child(0).as_ref().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, node.child(1), ":")?;
        let ty = parse_alge_ty(ctx, dta, node.child(2).as_ref().unwrap())?;

        ParserError::assert_node_no_error(ctx, node)?;
        Ok(TypedIdent {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            ident,
            ty,
        })
    }
}

impl FromTreeSitter for Call {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "fn_call")?;
        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        let ident = Ident::parse(ctx, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, children.next(), "(")?;

        let mut args = SmallVec::new();

        while let Some(next_node) = children.next() {
            match next_node.kind() {
                ")" => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ")")?;
                    break;
                }
                "," => {
                    ParserError::consume_expected_node_string(ctx, dta, Some(next_node), ",")?;
                }
                "expr" => args.push(Expr::parse(ctx, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_string(),
                        expected: "expr".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(&next_node)).finish());
                    return Err(err);
                }
            }
        }

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;
        Ok(Call {
            span: Span::from(node).with_file_maybe(ctx.get_file()),
            ident,
            args,
        })
    }
}

impl FromTreeSitter for Module {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "module")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        ParserError::consume_expected_node_kind(ctx, children.next(), "module")?;
        let mut path = SmallVec::new();

        while let Some(next_node) = children.next() {
            match next_node.kind() {
                "identifier" => path.push(Ident::parse(ctx, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_string(),
                        expected: "identifier".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(&next_node)).finish());
                    return Err(err);
                }
            }

            let next_node = if let Some(n) = children.next() {
                n
            } else {
                let err = ParserError::NoChildAvailable;
                report(error_reporter(err.clone(), Span::empty()).finish());
                return Err(err);
            };
            match next_node.kind() {
                //can end
                ";" => break,
                //or continue
                "::" => {}
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_string(),
                        expected: "\"::\" or \";\"".to_owned(),
                    };
                    report(error_reporter(err.clone(), ctx.span(&next_node)).finish());
                    return Err(err);
                }
            }
        }

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(Module {
            path,
            span: Span::from(node).with_file_maybe(ctx.get_file()),
        })
    }
}

impl FromTreeSitter for GammaExpr {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
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

        let mut conditionals = SmallVec::new();
        let mut unconditional = None;

        ParserError::consume_expected_node_string(ctx, dta, children.next(), "if")?;
        conditionals.push((
            Expr::parse(ctx, dta, &children.next().unwrap())?,
            Block::parse(ctx, dta, &children.next().unwrap())?,
        ));

        while let Some(next_node) = children.next() {
            match next_node.kind() {
                "else if" => {
                    conditionals.push((
                        Expr::parse(ctx, dta, &children.next().unwrap())?,
                        Block::parse(ctx, dta, &children.next().unwrap())?,
                    ));
                }
                "else" => {
                    unconditional = Some(Block::parse(ctx, dta, &children.next().unwrap())?);
                    break;
                }
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_string(),
                        expected: "else or else-if".to_owned(),
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
        Ok(GammaExpr {
            span: ctx.span(node),
            conditionals,
            unconditional,
        })
    }
}
