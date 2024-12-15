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
    common::{Call, DataTy, Digit, Ident, Literal, Shape, Ty, TypedIdent},
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
                report(
                    error_reporter(err.clone(), Span::empty())
                        .with_label(
                            Label::new(ctx.span(node)).with_message("UTF-8 Parser error here"),
                        )
                        .finish(),
                );
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

impl FromTreeSitter for DataTy {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "data_type")?;

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

        //match the text
        match node_text.as_str() {
            "real" => Ok(DataTy::Real),
            "int" => Ok(DataTy::Integer),
            "complex" => Ok(DataTy::Complex),
            "quat" => Ok(DataTy::Quaternion),
            "bool" => Ok(DataTy::Bool),
            "none" => Ok(DataTy::Void),
            other => {
                let err = ParserError::UnexpectedAstNode {
                    kind: other.to_owned(),
                    expected: "data type".to_owned(),
                };
                report(error_reporter(err.clone(), ctx.span(node)).finish());
                return Err(err);
            }
        }
    }
}

impl FromTreeSitter for Shape {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "shape")?;

        match node.child(0).unwrap().kind() {
            "interval" => Ok(Self::Interval),
            "vec" => {
                let width = Digit::parse(ctx, dta, node.child(1).as_ref().unwrap())?;
                Ok(Self::Vec { width: width.0 })
            }
            "mat" => {
                //can either be mat3 or mat3x2,
                let first = Digit::parse(ctx, dta, node.child(1).as_ref().unwrap())?;
                let second = if let Some(succ) = node.child(2).as_ref() {
                    if succ.kind() != "x" {
                        let err = ParserError::UnexpectedAstNode {
                            kind: succ.kind().to_owned(),
                            expected: "x".to_owned(),
                        };
                        report(error_reporter(err.clone(), ctx.span(succ)).finish());
                        return Err(err);
                    }

                    //has successor, read the digit
                    Some(Digit::parse(ctx, dta, node.child(3).as_ref().unwrap())?)
                } else {
                    None
                };

                if let Some(second) = second {
                    Ok(Self::Matrix {
                        width: first.0,
                        height: second.0,
                    })
                } else {
                    //Square matrix
                    Ok(Self::Matrix {
                        width: first.0,
                        height: first.0,
                    })
                }
            }
            "tensor" => {
                //Is a tensor, opt for reader-style parsing
                let mut walker = node.walk();
                let mut children = node.children(&mut walker);

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

                Ok(Shape::Tensor { sizes: dim })
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: node.kind().to_string(),
                    expected: "shape description".to_owned(),
                };
                report(error_reporter(err.clone(), ctx.span(node)).finish());
                Err(err)
            }
        }
    }
}

impl FromTreeSitter for Ty {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_no_error(ctx, node)?;

        //we have 4 possibilities:
        // 1. just a data-type
        // 2. csg
        // 3. shape
        //  3.1 shape without data type, which is implicitly a shaped real
        //  3.2 shaped with data_type
        // 4. tuple with sub-types

        //Might have to unwrap the node, if its the _type_ kind.
        let root_node = if node.kind() == "type" {
            &node.child(0).unwrap()
        } else {
            node
        };
        match root_node.kind() {
            "data_type" => Ok(Self::Simple(DataTy::parse(ctx, dta, &root_node)?)),
            //NOTE: this way we make sure that CSGs are never shaped
            "csg" => Ok(Self::Simple(DataTy::Csg)),
            "(" => {
                //should be a tuple, use walker style
                let mut walker = node.walk();
                let mut children = node.children(&mut walker);
                ParserError::consume_expected_node_string(ctx, dta, children.next(), "(")?;
                let mut args = Vec::with_capacity(8);
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
                        "," => {
                            ParserError::consume_expected_node_string(
                                ctx,
                                dta,
                                Some(next_node),
                                ",",
                            )?;
                        }
                        _ => args.push(Ty::parse(ctx, dta, &next_node)?),
                    }
                }

                ParserError::assert_ast_level_empty(ctx, children.next())?;
                Ok(Ty::Tuple(args))
            }
            "shape" => {
                //either just a shape, or a shape followed by a data_type
                let shape = Shape::parse(ctx, dta, &root_node)?;
                let data_type = if node.child(1).map(|n| n.kind()) == Some("<") {
                    DataTy::parse(ctx, dta, node.child(2).as_ref().unwrap())?
                } else {
                    //Implicit real for un-constraint
                    DataTy::Real
                };

                Ok(Self::Shaped {
                    ty: data_type,
                    shape,
                })
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: root_node.kind().to_string(),
                    expected: "data_type | shape | csg | (".to_owned(),
                };
                report(
                    error_reporter(err.clone(), ctx.span(&root_node))
                        .with_label(Label::new(ctx.span(&root_node)).with_message("here"))
                        .finish(),
                );
                Err(err)
            }
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
            "int_literal" => {
                //reuse the digit parser, but unwrap the value
                let digit = Digit::parse(ctx, dta, &node.child(0).unwrap())?;
                ParserError::assert_node_no_error(ctx, node)?;
                Ok(Literal::IntegerLiteral(digit.0))
            }
            "bool_literal" => match node.child(0).unwrap().kind() {
                "true" => Ok(Literal::BoolLiteral(true)),
                "false" => Ok(Literal::BoolLiteral(false)),
                _other => {
                    let err = ParserError::ParseBoolFailed;
                    report(
                        error_reporter(err.clone(), ctx.span(node))
                            .with_label(
                                Label::new(ctx.span(node))
                                    .with_message("should be \"true\" or \"false\""),
                            )
                            .finish(),
                    );
                    return Err(err);
                }
            },
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: node.kind().to_string(),
                    expected: "integer_literal | float_literal".to_owned(),
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
impl FromTreeSitter for TypedIdent {
    fn parse(ctx: &mut ParserCtx, dta: &[u8], node: &tree_sitter::Node) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(ctx, node, "typed_arg")?;
        let ident = Ident::parse(ctx, dta, node.child(0).as_ref().unwrap())?;
        ParserError::consume_expected_node_string(ctx, dta, node.child(1), ":")?;
        let ty = Ty::parse(ctx, dta, node.child(2).as_ref().unwrap())?;

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
