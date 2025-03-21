use tree_sitter::Node;

use crate::{
    ParserCtx,
    error::ParserError,
    scad_ast::{ScadArg, ScadBlock, ScadCall, ScadExpr, ScadStmt},
};

pub fn argument_sequence(
    ctx: &mut ParserCtx,
    data: &[u8],
    node: &Node,
) -> Result<Vec<ScadArg>, ParserError> {
    if node.kind() != "arguments" {
        return Err(ParserError::MalformedNode(format!("expected arguments")));
    }

    let mut walker = node.walk();
    let mut childern = node.children(&mut walker);
    let mut args = Vec::new();
    let start_parent = childern.next().unwrap();
    if start_parent.kind() != "(" {
        return Err(ParserError::MalformedNode(format!(
            "expected \"(\", got {}",
            start_parent.kind()
        )));
    }

    while let Some(next) = childern.next() {
        match next.kind() {
            //NOTE: "argument" is, again, a little strange, so we basically pares _assignment_, if that
            //      ain't it, we try an expression, and otherwise remap the error
            "assignment" => {
                let assign = crate::assignment::assignment(ctx, data, &next)?;
                args.push(ScadArg::Assign(assign));
            }
            other => {
                let expr = crate::expr::expr(ctx, data, &next).map_err(|_e| {
                    ParserError::MalformedNode(
                        "Argument was no assignment, but also no expression".to_owned(),
                    )
                })?;
                args.push(ScadArg::Expr(expr));
            }
        }

        let next = childern.next().unwrap();
        //check if we must / should break
        match next.kind() {
            "," => {}
            ")" => break,
            other => {
                return Err(ParserError::MalformedNode(format!(
                    "expected ',' or ')', got {}",
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

    Ok(args)
}

///Parses the module_call flavour of function calls.
pub fn module_call(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadCall, ParserError> {
    let function_ident = crate::util::identifier(
        ctx,
        data,
        node.child_by_field_name("name").as_ref().unwrap(),
    )?;
    let args = argument_sequence(
        ctx,
        data,
        node.child_by_field_name("arguments").as_ref().unwrap(),
    )?;

    Ok(ScadCall {
        function: Box::new(ScadExpr::Var(function_ident)),
        args,
    })
}

pub fn chain(
    ctx: &mut ParserCtx,
    target_chain: &mut Vec<ScadCall>,
    data: &[u8],
    node: &Node,
) -> Result<(), ParserError> {
    match node.kind() {
        //Ignore modifier, in that case just recurse
        "modifier_chain" => chain(ctx, target_chain, data, node.child(1).as_ref().unwrap()),
        "transform_chain" => {
            //parse the module call, then recurse
            let call = module_call(ctx, data, node.child(0).as_ref().unwrap())?;
            target_chain.push(call);
            chain(ctx, target_chain, data, node.child(1).as_ref().unwrap())
        }
        //break recursion on ;
        ";" => return Ok(()),
        other => Err(ParserError::Unexpected(format!(
            "Expected modifier or transform, got {}",
            node.kind()
        ))),
    }
}

///Parses a (chain) of statements and appends them to _block_
pub fn stmt(
    ctx: &mut ParserCtx,
    block: &mut ScadBlock,
    data: &[u8],
    node: &Node,
) -> Result<ScadStmt, ParserError> {
    match node.kind() {
        "modifier_chain" | "transform_chain" => {
            let mut target_chain = Vec::new();
            chain(ctx, &mut target_chain, data, node)?;
            Ok(ScadStmt::Chain {
                chain: target_chain,
            })
        }
        other => Err(ParserError::Unexpected(format!("{}", node.kind()))),
    }
}
