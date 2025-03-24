use tree_sitter::Node;
use vola_ast::{alge::Func, common::Ident};
use vola_common::Span;

use crate::{ParserCtx, error::ParserError, scad_ast::ScadExpr};

///Parses the identifier into a simple string based ident
pub fn identifier(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<Ident, ParserError> {
    if node.kind() != "identifier" {
        return Err(ParserError::MalformedNode(format!(
            "expected ident, got {:?}",
            node.kind()
        )));
    }

    let ident = node.utf8_text(data).map_err(|e| ParserError::from(e))?;
    Ok(Ident(ident.to_owned()))
}

///So openScad _literal_ is a little strange, since function-values are treated as literals?
///Anyways, we just Declare that here, and let the _user_ handle the rest.
#[derive(Debug, Clone)]
pub enum ScadLiteral {
    Function(Func),
    Range {
        start: Box<ScadExpr>,
        end: Box<ScadExpr>,
        increment: Option<Box<ScadExpr>>,
    },
    Undef,
    List(Vec<ScadExpr>),
    Decimal(i32),
    Float(f64),
    Boolean(bool),
    String(String),
}

impl ScadLiteral {
    pub fn into_expr(self, span: Span) -> ScadExpr {
        ScadExpr::Literal { lit: self, span }
    }

    pub fn vec_n(value: f64, count: usize) -> Self {
        ScadLiteral::List(vec![
            ScadExpr::Literal {
                span: Span::empty(),
                lit: ScadLiteral::Float(value)
            };
            count
        ])
    }
}

pub fn list(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<Vec<ScadExpr>, ParserError> {
    if node.kind() != "list" {
        return Err(ParserError::MalformedNode(format!("expected list")));
    }

    let mut walker = node.walk();
    let mut childern = node.children(&mut walker);
    let mut elements = Vec::new();
    let start_parent = childern.next().unwrap();
    if start_parent.kind() != "[" {
        return Err(ParserError::MalformedNode(format!(
            "expected \"[\", got {}",
            start_parent.kind()
        )));
    }

    while let Some(next) = childern.next() {
        match next.kind() {
            "each" | "list_comprehension" => {
                return Err(ParserError::UnsupportedScadFeature(next.kind().to_string()));
            }
            other => {
                let expr = crate::expr::expr(ctx, data, &next).map_err(|_e| {
                    ParserError::MalformedNode(
                        "Expected list element to be an expression".to_owned(),
                    )
                })?;
                elements.push(expr);
            }
        }

        let next = childern.next().unwrap();
        //check if we must / should break
        match next.kind() {
            "," => {}
            "]" => break,
            other => {
                return Err(ParserError::MalformedNode(format!(
                    "expected ',' or ']', got {}",
                    other
                )));
            }
        }
    }

    if childern.next().is_some() {
        return Err(ParserError::MalformedNode(
            "There should be no further values".to_owned(),
        ));
    }

    Ok(elements)
}

pub fn literal(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadLiteral, ParserError> {
    match node.kind() {
        "string" => {
            let mut string = node.utf8_text(data).unwrap().to_string();
            string.remove_matches("\"");
            Ok(ScadLiteral::String(string))
        }
        "number" => number(ctx, data, node),
        "undef" => Ok(ScadLiteral::Undef),
        "range" => {
            let start = crate::expr::expr(
                ctx,
                data,
                node.child_by_field_name("start").as_ref().unwrap(),
            )?;
            let end = crate::expr::expr(
                ctx,
                data,
                node.child_by_field_name("start").as_ref().unwrap(),
            )?;
            let increment = if let Some(increment) = node.child_by_field_name("increment").as_ref()
            {
                Some(crate::expr::expr(ctx, data, increment)?)
            } else {
                None
            };
            Ok(ScadLiteral::Range {
                start: Box::new(start),
                end: Box::new(end),
                increment: increment.map(|f| Box::new(f)),
            })
        }
        "list" => Ok(ScadLiteral::List(list(ctx, data, node)?)),
        "boolean" => match node.utf8_text(data)? {
            "true" => Ok(ScadLiteral::Boolean(true)),
            "false" => Ok(ScadLiteral::Boolean(false)),
            other => Err(ParserError::MalformedNode(format!(
                "Unknown boolean literal: '{}'",
                other
            ))),
        },
        other => Err(ParserError::Unexpected(format!(
            "Expected some kind of literal, got {}",
            other
        ))),
    }
}

pub fn number(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadLiteral, ParserError> {
    match node.kind() {
        "decimal" => {
            //try to parse the decimal as int
            let int: i32 = node
                .utf8_text(data)
                .map_err(|e| ParserError::Utf8ParseError(e))?
                .parse()
                .map_err(|e| ParserError::from(e))?;
            Ok(ScadLiteral::Decimal(int))
        }
        "float" => {
            let float: f64 = node
                .utf8_text(data)
                .map_err(|e| ParserError::Utf8ParseError(e))?
                .parse()
                .map_err(|e| ParserError::from(e))?;
            Ok(ScadLiteral::Float(float))
        }
        other => Err(ParserError::Unexpected(format!(
            "Expected Float or decimal, got {}",
            other
        ))),
    }
}
