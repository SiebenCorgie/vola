use smallvec::SmallVec;
use vola_common::{CommonError, Span};

use crate::{
    alge::AlgeExpr,
    common::{Call, Digit, Ident, Literal, Ty, TypedIdent},
    csg::AccessDesc,
    error::ParserError,
};

use super::FromTreeSitter;

impl FromTreeSitter for Ident {
    fn parse(
        reporter: &mut vola_common::ErrorReporter<crate::error::ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, crate::error::ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(reporter, node, "identifier")?;
        let ident = match node.utf8_text(dta) {
            Ok(parsed) => parsed,
            Err(e) => {
                let err = ParserError::Utf8ParseError(e);
                reporter.push_error(CommonError::new_on_node(node, err.clone()));
                return Err(err);
            }
        };

        println!("Ident: {ident}");

        Ok(Ident(ident.to_owned()))
    }
}

impl FromTreeSitter for Digit {
    fn parse(
        reporter: &mut vola_common::ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(reporter, node, "digit")?;
        let node_text = match node.utf8_text(dta) {
            Err(e) => {
                let err = ParserError::Utf8ParseError(e);
                reporter.push_error(CommonError::new_on_node(node, err.clone()));
                return Err(err);
            }
            Ok(s) => s.to_owned(),
        };

        println!("Parse Digit: {}", node_text);

        let int: usize = match node_text.parse() {
            Ok(f) => f,
            Err(e) => {
                let err = ParserError::ParseIntLiteral(e);
                reporter.push_error(CommonError::new_on_node(node, err.clone()));
                return Err(err);
            }
        };

        Ok(Digit(int))
    }
}

impl FromTreeSitter for Ty {
    fn parse(
        reporter: &mut vola_common::ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        let node_text = match node.utf8_text(dta) {
            Err(e) => {
                let err = ParserError::Utf8ParseError(e);
                reporter.push_error(CommonError::new_on_node(node, err.clone()));
                return Err(err);
            }
            Ok(s) => s.to_owned(),
        };

        //First check for the abstract "CSG" type
        if node_text == "CSG" {
            return Ok(Ty::CSGTree);
        }
        //Otherwise this should be a alge_type
        Self::parse_alge_ty(reporter, dta, node)
    }
}

impl Ty {
    pub fn parse_alge_ty(
        reporter: &mut vola_common::ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError> {
        ParserError::assert_node_kind(reporter, node, "alge_type")?;

        match node.child(0).unwrap().kind() {
            "t_scalar" => {
                //Scalar is easy
                Ok(Ty::Scalar)
            }
            "t_vec" => {
                //try to parse the first digit
                let width = Digit::parse(reporter, dta, &node.child(0).unwrap().child(1).unwrap())?;
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
                    kind: node.child(0).unwrap().kind().to_string(),
                    expected: "t_scalar | t_vec | t_mat | t_tensor".to_owned(),
                };
                reporter.push_error(CommonError::new(Span::from(node), err.clone()));
                Err(err)
            }
        }
    }
}

impl FromTreeSitter for Literal {
    fn parse(
        reporter: &mut vola_common::ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
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
                        reporter.push_error(CommonError::new_on_node(node, err.clone()));
                        return Err(err);
                    }
                    Ok(s) => s.to_owned(),
                };

                println!("Try parsing {node_text}");

                let float: f64 = match node_text.parse() {
                    Ok(f) => f,
                    Err(e) => {
                        let err = ParserError::ParseFloatLiteral(e);
                        reporter.push_error(CommonError::new_on_node(node, err.clone()));
                        return Err(err);
                    }
                };

                Ok(Literal::FloatLiteral(float))
            }
            "integer_literal" => {
                //reuse the digit parser, but unwrap the value
                let digit = Digit::parse(reporter, dta, &node.child(0).unwrap())?;
                Ok(Literal::IntegerLiteral(digit.0))
            }
            _ => {
                let err = ParserError::UnexpectedAstNode {
                    kind: node.kind().to_string(),
                    expected: "integer_literal | float_literal".to_owned(),
                };
                reporter.push_error(CommonError::new(Span::from(node), err.clone()));
                Err(err)
            }
        }
    }
}
impl FromTreeSitter for TypedIdent {
    fn parse(
        reporter: &mut vola_common::ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(reporter, node, "typed_arg")?;
        let ident = Ident::parse(reporter, dta, node.child(0).as_ref().unwrap())?;
        ParserError::consume_expected_node_kind(reporter, node.child(1), ":")?;
        let ty = Ty::parse_alge_ty(reporter, dta, node.child(2).as_ref().unwrap())?;

        Ok(TypedIdent { ident, ty })
    }
}

impl FromTreeSitter for Call {
    fn parse(
        reporter: &mut vola_common::ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(reporter, node, "fn_call");
        let mut walker = node.walk();
        let mut children = node.children(&mut walker);

        let ident = Ident::parse(reporter, dta, &children.next().unwrap())?;
        ParserError::consume_expected_node_kind(reporter, children.next(), "(")?;

        let mut args = SmallVec::new();

        while let Some(next_node) = children.next() {
            match next_node.kind() {
                ")" => break,
                "alge_expr" => args.push(AlgeExpr::parse(reporter, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_string(),
                        expected: "alge_expr".to_owned(),
                    };
                    reporter.push_error(CommonError::new(Span::from(&next_node), err.clone()));
                    return Err(err);
                }
            }
        }

        Ok(Call { ident, args })
    }
}
