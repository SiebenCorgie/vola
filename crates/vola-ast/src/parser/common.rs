use smallvec::SmallVec;
use vola_common::{report, Span};

use crate::{
    alge::AlgeExpr,
    common::{CTArg, Call, Digit, Ident, Literal, Ty, TypedIdent},
    error::ParserError,
};

use super::{FromTreeSitter, ParserCtx};

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
            Ok(parsed) => parsed,
            Err(e) => {
                let err = ParserError::Utf8ParseError(e);
                report(err.clone(), ctx.get_file());
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
                report(err.clone(), ctx.get_file());
                return Err(err);
            }
            Ok(s) => s.to_owned(),
        };

        let int: usize = match node_text.parse() {
            Ok(f) => f,
            Err(e) => {
                let err = ParserError::ParseIntLiteral(e);
                report(err.clone(), ctx.get_file());
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
                report(err.clone(), ctx.get_file());
                return Err(err);
            }
            Ok(s) => s.to_owned(),
        };

        //First check for the abstract "CSG" type
        if node_text == "CSG" {
            return Ok(Ty::CSGTree);
        }
        //Otherwise this should be a alge_type
        Self::parse_alge_ty(ctx, dta, node)
    }
}

impl Ty {
    pub fn parse_alge_ty(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError> {
        ParserError::assert_node_kind(ctx, node, "alge_type")?;

        match node.child(0).unwrap().kind() {
            "t_scalar" => {
                //Scalar is easy
                Ok(Ty::Scalar)
            }
            "t_vec" => {
                //try to parse the first digit
                let width = Digit::parse(ctx, dta, &node.child(0).unwrap().child(1).unwrap())?;
                Ok(Ty::Vec { width: width.0 })
            }
            "t_mat" => {
                todo!()
            }
            "t_tensor" => {
                todo!()
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    span: ctx.span(&node).into(),
                    kind: node.child(0).unwrap().kind().to_string(),
                    expected: "t_scalar | t_vec | t_mat | t_tensor".to_owned(),
                };
                report(err.clone(), ctx.get_file());
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
                        report(err.clone(), ctx.get_file());
                        return Err(err);
                    }
                    Ok(s) => s.to_owned(),
                };

                let float: f64 = match node_text.parse() {
                    Ok(f) => f,
                    Err(e) => {
                        let err = ParserError::ParseFloatLiteral(e);
                        report(err.clone(), ctx.get_file());
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
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    span: ctx.span(&node).into(),
                    kind: node.kind().to_string(),
                    expected: "integer_literal | float_literal".to_owned(),
                };
                report(err.clone(), ctx.get_file());
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
        ParserError::consume_expected_node_kind(ctx, node.child(1), ":")?;
        let ty = Ty::parse_alge_ty(ctx, dta, node.child(2).as_ref().unwrap())?;

        ParserError::assert_node_no_error(ctx, node)?;
        Ok(TypedIdent {
            span: Span::from(node),
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
        ParserError::consume_expected_node_kind(ctx, children.next(), "(")?;

        let mut args = SmallVec::new();

        while let Some(next_node) = children.next() {
            match next_node.kind() {
                ")" => break,
                "," => {}
                "alge_expr" => args.push(AlgeExpr::parse(ctx, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        span: ctx.span(&next_node).into(),
                        kind: next_node.kind().to_string(),
                        expected: "alge_expr".to_owned(),
                    };
                    report(err.clone(), ctx.get_file());
                    return Err(err);
                }
            }
        }

        ParserError::assert_node_no_error(ctx, node)?;
        Ok(Call {
            span: Span::from(node),
            ident,
            args,
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

        ParserError::consume_expected_node_kind(ctx, children.next(), "#")?;
        ParserError::consume_expected_node_kind(ctx, children.next(), "[")?;

        //NOTE: Right now we reuse the call parser.
        let call = Call::parse(ctx, dta, &children.next().unwrap())?;

        let Call { span, ident, args } = call;

        ParserError::consume_expected_node_kind(ctx, children.next(), "]")?;

        ParserError::assert_ast_level_empty(ctx, children.next())?;
        ParserError::assert_node_no_error(ctx, node)?;

        Ok(CTArg { span, ident, args })
    }
}
