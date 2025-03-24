use tree_sitter::Node;

use crate::{
    ParserCtx,
    assignment::{self, assignment},
    error::ParserError,
    expr::expr,
    report_here,
    scad_ast::{ScadBlock, ScadExpr, ScadModule, ScadParameter},
    stmt::{block, stmt},
    util::identifier,
};

pub fn parameter_delerations(
    ctx: &mut ParserCtx,
    data: &[u8],
    node: &Node,
) -> Result<Vec<ScadParameter>, ParserError> {
    if node.kind() != "parameters_declaration" {
        report_here("expected parameters_decleration", ctx.span(node));
        return Err(ParserError::MalformedNode(
            "not parameters declaration".to_owned(),
        ));
    }

    let mut walker = node.walk();
    let mut children = node.children(&mut walker);

    let first = children.next().unwrap();
    if first.kind() != "(" {
        report_here("expected '('", ctx.span(&first));
    }

    let mut parameter = Vec::new();
    while let Some(next) = children.next() {
        match next.kind() {
            //empty case
            ")" => break,
            "assignment" => {
                let assign = assignment(ctx, data, &next)?;
                parameter.push(ScadParameter::ByAssignment(assign));
            }
            _other => {
                if let Ok(ident) = identifier(ctx, data, &next) {
                    parameter.push(ScadParameter::ByIdent {
                        span: ctx.span(&next),
                        ident,
                    });
                } else {
                    report_here(
                        "parameter must be assignment or identifier",
                        ctx.span(&next),
                    );
                    return Err(ParserError::MalformedNode(
                        "parameter not identifier or assignment".to_owned(),
                    ));
                }
            }
        }

        let follow = children.next().unwrap();
        match follow.kind() {
            "," => {}
            ")" => break,
            other => {
                report_here(
                    format!("expected ',' or ')', got {other}"),
                    ctx.span(&follow),
                );
                return Err(ParserError::Unexpected("expected ',' or ')'".to_owned()));
            }
        }
    }

    Ok(parameter)
}

pub fn module_decl(
    ctx: &mut ParserCtx,
    data: &[u8],
    node: &Node,
) -> Result<ScadModule, ParserError> {
    let name = identifier(
        ctx,
        data,
        node.child_by_field_name("name").as_ref().unwrap(),
    )?;
    let args = parameter_delerations(
        ctx,
        data,
        node.child_by_field_name("parameters").as_ref().unwrap(),
    )?;
    //try to convert from block, if that fails, it might be a single stmt
    //module

    let block = match block(
        ctx,
        data,
        node.child_by_field_name("body").as_ref().unwrap(),
    ) {
        Ok(block) => block,
        Err(be) => {
            let blockspan = ctx.span(node.child_by_field_name("body").as_ref().unwrap());
            match stmt(
                ctx,
                data,
                node.child_by_field_name("body").as_ref().unwrap(),
            ) {
                Ok(stmt) => ScadBlock {
                    span: blockspan,
                    stmts: vec![stmt],
                },
                Err(_ee) => {
                    report_here("Could not parse this module", blockspan);
                    return Err(be);
                }
            }
        }
    };

    Ok(ScadModule {
        span: ctx.span(node),
        name,
        args,
        block,
    })
}

pub fn function_decl(
    ctx: &mut ParserCtx,
    data: &[u8],
    node: &Node,
) -> Result<ScadExpr, ParserError> {
    Err(ParserError::UnsupportedScadFeature(
        "functions not (yet) supported!".to_owned(),
    ))
}
