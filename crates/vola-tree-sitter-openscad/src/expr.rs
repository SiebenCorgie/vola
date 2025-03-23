use tree_sitter::Node;
use vola_ast::common::Ident;

use crate::{
    ParserCtx,
    assignment::assignment,
    error::ParserError,
    report_here,
    scad_ast::{ScadAssignment, ScadExpr},
    stmt::{argument_sequence, block},
    warn_here,
};

pub fn variable_name(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<Ident, ParserError> {
    match node.kind() {
        "identifier" => crate::util::identifier(ctx, data, node),
        "special_variable" => {
            let ident = crate::util::identifier(ctx, data, node.child(1).as_ref().unwrap())?;
            Ok(Ident(format!("${}", ident.0)))
        }
        other => Err(ParserError::MalformedNode(other.to_owned())),
    }
}

pub fn parenthesized_assignements(
    ctx: &mut ParserCtx,
    data: &[u8],
    node: &Node,
) -> Result<Vec<ScadAssignment>, ParserError> {
    let mut walker = node.walk();
    let mut children = node.children(&mut walker);

    let first = children.next().unwrap();
    if first.kind() != "(" {
        return Err(ParserError::MalformedNode(format!(
            "Expected '(', got {}",
            first.kind()
        )));
    }

    let mut assignments = Vec::new();
    while let Some(next) = children.next() {
        match next.kind() {
            "assignment" => {
                let a = assignment(ctx, data, &next)?;
                assignments.push(a);
            }
            //empty list
            ")" => break,
            _other => {
                report_here("expected ')' or assignment", ctx.span(&next));
                return Err(ParserError::MalformedNode(
                    "expected ')' or assignment".to_owned(),
                ));
            }
        }

        match children.next().unwrap().kind() {
            ")" => break,
            "," => {}
            _other => {
                report_here("expected ')' or ','", ctx.span(&next));
                return Err(ParserError::MalformedNode("expected ')' or ','".to_owned()));
            }
        }
    }

    Ok(assignments)
}

//parses the full scad expr.
pub fn expr(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadExpr, ParserError> {
    //NOTE: the syn-tree is _sometimes_ strange, so we have all kinds of escape hatches :/
    match node.kind() {
        "decimal" | "float" => {
            let num = crate::util::number(ctx, data, node)?;
            //found some kind of literal, unwrap it into an expression for vola, and return that
            Ok(num.into_expr())
        }
        "literal" => {
            let lit = crate::util::literal(ctx, data, node)?;
            Ok(lit.into_expr())
        }
        "identifier" => {
            let ident = crate::util::identifier(ctx, data, node)?;
            Ok(ScadExpr::Var {
                var: ident,
                span: ctx.span(node),
            })
        }
        "special_variable" => {
            let ident = crate::util::identifier(ctx, data, node.child(1).as_ref().unwrap())?;
            Ok(ScadExpr::Var {
                var: Ident(format!("${}", ident.0)),
                span: ctx.span(node),
            })
        }
        "unary_expression" => {
            let op = match node.child(0).as_ref().unwrap().kind() {
                "-" => vola_ast::alge::UnaryOp::Neg,
                "!" => vola_ast::alge::UnaryOp::Not,
                "+" => {
                    //NOTE: we don't support explicit _positive sign_,
                    //therfore juts return the sub expression
                    return expr(ctx, data, node.child(1).as_ref().unwrap());
                }
                other => {
                    report_here(
                        format!("unsupported unary expression: {other}"),
                        ctx.span(node),
                    );
                    return Err(ParserError::MalformedNode(
                        "unsupported unary operation".to_owned(),
                    ));
                }
            };

            let sub_expression = expr(ctx, data, node.child(1).as_ref().unwrap())?;
            Ok(ScadExpr::Unary {
                span: ctx.span(node),
                op,
                expr: Box::new(sub_expression),
            })
        }
        "binary_expression" => {
            let left = expr(
                ctx,
                data,
                node.child_by_field_name("left").as_ref().unwrap(),
            )?;
            let right = expr(
                ctx,
                data,
                node.child_by_field_name("left").as_ref().unwrap(),
            )?;
            let op = match node.child(1).as_ref().unwrap().kind() {
                "||" => vola_ast::alge::BinaryOp::Or,
                "&&" => vola_ast::alge::BinaryOp::And,
                "==" => vola_ast::alge::BinaryOp::Eq,
                "!=" => vola_ast::alge::BinaryOp::NotEq,
                "<" => vola_ast::alge::BinaryOp::Lt,
                ">" => vola_ast::alge::BinaryOp::Gt,
                "<=" => vola_ast::alge::BinaryOp::Lte,
                ">=" => vola_ast::alge::BinaryOp::Gte,
                "+" => vola_ast::alge::BinaryOp::Add,
                "-" => vola_ast::alge::BinaryOp::Sub,
                "*" => vola_ast::alge::BinaryOp::Mul,
                "/" => vola_ast::alge::BinaryOp::Div,
                "%" => vola_ast::alge::BinaryOp::Mod,
                //"^" => vola_ast::alge::BinaryOp::?, // Only in grammar, not spec?
                other => {
                    report_here(
                        format!("unsupported unary expression: {other}"),
                        ctx.span(node),
                    );
                    return Err(ParserError::MalformedNode(
                        "unsupported unary operation".to_owned(),
                    ));
                }
            };

            Ok(ScadExpr::Binary {
                span: ctx.span(node),
                op,
                left: Box::new(left),
                right: Box::new(right),
            })
        }
        "parenthesized_expression" => expr(ctx, data, node.child(1).as_ref().unwrap()),

        "ternary_expression" => {
            let condition = expr(
                ctx,
                data,
                node.child_by_field_name("condition").as_ref().unwrap(),
            )?;
            let consequence = expr(
                ctx,
                data,
                node.child_by_field_name("consequence").as_ref().unwrap(),
            )?;
            let alternative = expr(
                ctx,
                data,
                node.child_by_field_name("alternative").as_ref().unwrap(),
            )?;
            Ok(ScadExpr::Ternary {
                span: ctx.span(node),
                condition: Box::new(condition),
                consequence: Box::new(consequence),
                alternative: Box::new(alternative),
            })
        }
        "let_expression" => {
            let body = block(
                ctx,
                data,
                node.child_by_field_name("body").as_ref().unwrap(),
            )?;
            let overwrites =
                parenthesized_assignements(ctx, data, node.child(1).as_ref().unwrap())?;

            Ok(ScadExpr::Overwrite {
                span: ctx.span(node),
                overwrites,
                block: body,
            })
        }
        "function_call" => {
            let function = match expr(
                ctx,
                data,
                node.child_by_field_name("function").as_ref().unwrap(),
            )? {
                ScadExpr::Var { span: _, var } => var,
                _other => {
                    report_here(
                        "function call's name should be a identifier",
                        ctx.span(node),
                    );
                    return Err(ParserError::MalformedNode(
                        "function call's name should be identifier".to_owned(),
                    ));
                }
            };

            match function.0.as_ref() {
                "echo" | "render" | "children" | "assert" | "is_undef" | "is_bool" | "is_num"
                | "is_string" | "is_list" | "is_function" => {
                    warn_here(
                        format!("function '{}' not supported. Ignoring", function.0),
                        ctx.span(node),
                    );
                    return Err(ParserError::Ignored);
                }
                _ => {}
            }

            let args = argument_sequence(
                ctx,
                data,
                node.child_by_field_name("arguments").as_ref().unwrap(),
            )?;

            Ok(ScadExpr::Call(crate::scad_ast::ScadCall {
                span: ctx.span(node),
                function,
                args,
            }))
        }
        "index_expression" | "dot_index_expression" | "assert_expression" => {
            report_here("expression not (yet) supported", ctx.span(node));
            Err(ParserError::UnsupportedScadFeature(
                "not-yet-supported".to_owned(),
            ))
        }
        "list" | "range" | "undef" | "number" | "string" | "boolean" => {
            Ok(crate::util::literal(ctx, data, node)?.into_expr())
        }
        other => {
            report_here(format!("unexpected '{other}'"), ctx.span(node));
            Err(ParserError::Unexpected(format!(
                "Unexpected node-kind in expression location: {}",
                other
            )))
        }
    }
}
