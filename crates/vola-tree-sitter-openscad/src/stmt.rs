use std::path::{Path, PathBuf};

use tree_sitter::Node;
use vola_common::Span;

use crate::{
    ParserCtx,
    comment::comment,
    error::ParserError,
    expr::{expr, parenthesized_assignements, parenthesized_expression},
    report_here,
    scad_ast::{ChainElement, ScadArg, ScadBlock, ScadCall, ScadExpr, ScadStmt},
    warn_here,
};

pub fn file_use_stmt(
    ctx: &mut ParserCtx,
    is_use: bool,
    data: &[u8],
    node: &Node,
) -> Result<(), ParserError> {
    if is_use {
        if node.kind() != "use_statement" || node.child(0).as_ref().unwrap().kind() != "use" {
            return Err(ParserError::Unexpected("expected use statement".to_owned()));
        }
    } else {
        if node.kind() != "include_statement" || node.child(0).as_ref().unwrap().kind() != "include"
        {
            return Err(ParserError::Unexpected(
                "expected include statement".to_owned(),
            ));
        }
    }

    let path_string = node
        .child(1)
        .as_ref()
        .unwrap()
        .utf8_text(data)
        .map_err(|e| ParserError::from(e))?;
    let path = PathBuf::from(path_string);
    //try to build the _actual_ path.
    //if its absolute, that already done, otherwise try to resolve locally to the context,
    //or locally to the SCAD var
    let path = if path.is_absolute() {
        path
    } else {
        let ctx_local = Path::new(ctx.src_file.as_str()).join(path.clone());
        if ctx_local.exists() {
            ctx_local
        } else {
            if let Ok(var) = std::env::var("OPENSCADPATH") {
                let var_local = Path::new(&var).join(path.clone());
                if var_local.exists() {
                    var_local
                } else {
                    return Err(ParserError::FSError(format!(
                        "Could not find file {path:?}, not relative to the source file ({}) nor relative to 'OPENSCADPATH'",
                        ctx.src_file
                    )));
                }
            } else {
                return Err(ParserError::FSError(format!(
                    "Could not find file {path:?}, not relative to the source file ({}) nor relative to 'OPENSCADPATH'",
                    ctx.src_file
                )));
            }
        }
    };
    if is_use {
        ctx.resolve_uses.push(path);
    } else {
        ctx.resolve_includes.push(path);
    }
    Ok(())
}

pub fn block(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadBlock, ParserError> {
    let mut walker = node.walk();
    let mut children = node.children(&mut walker);

    let mut block = Vec::new();

    let start = if let Some(child) = children.next() {
        child
    } else {
        return Err(ParserError::MalformedNode(
            "no start '{' on block".to_owned(),
        ));
    };
    if start.kind() != "{" {
        return Err(ParserError::MalformedNode("expected '{'".to_owned()));
    }

    while let Some(next) = children.next() {
        match next.kind() {
            "}" => break,
            "module_declaration" => {
                report_here(
                    "inline module decleration not (yet) supported. Consider moving that outside the parent module",
                    ctx.span(&next),
                );
                return Err(ParserError::UnsupportedScadFeature(
                    "inline module deceleration".to_owned(),
                ));
            }
            other => {
                //we assume _some-kind_ of statement in the blocks
                match stmt(ctx, data, &next) {
                    Ok(stmt) => block.push(stmt),
                    Err(e) => {
                        //TODO: handle all the strange stuff
                        report_here(
                            format!(
                                "Expected some kind of statement, but was {other}: {}",
                                next.utf8_text(data).unwrap()
                            ),
                            ctx.span(&next),
                        );
                        return Err(e);
                    }
                }
            }
        }
    }

    Ok(ScadBlock {
        span: ctx.span(node),
        stmts: block,
    })
}

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
            //NOTE: this can happen for empty arguments.
            ")" => break,
            other => {
                let expr = crate::expr::expr(ctx, data, &next).map_err(|e| {
                    report_here(format!("Could not build argument: {e}"), ctx.span(&next));
                    ParserError::MalformedNode(format!(
                        "Argument was no assignment, but also no expression, was: '{}'",
                        other
                    ))
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
        span: ctx.span(node),
        function: function_ident,
        args,
    })
}

pub fn chain(
    ctx: &mut ParserCtx,
    target_chain: &mut Vec<ChainElement>,
    data: &[u8],
    node: &Node,
) -> Result<(), ParserError> {
    match node.kind() {
        //Ignore modifier, in that case just recurse
        "modifier_chain" => chain(ctx, target_chain, data, node.child(1).as_ref().unwrap()),
        "transform_chain" => {
            //parse the module call, then recurse
            let call = module_call(ctx, data, node.child(0).as_ref().unwrap())?;

            match call.function.0.as_ref() {
                "echo" | "render" | "children" | "assert" | "is_undef" | "is_bool" | "is_num"
                | "is_string" | "is_list" | "is_function" => {
                    warn_here(
                        format!("function '{}' not supported. Ignoring...", call.function.0),
                        ctx.span(node),
                    );
                    return Err(ParserError::Ignored);
                }
                _ => {}
            }

            target_chain.push(ChainElement::Call(call));
            chain(ctx, target_chain, data, node.child(1).as_ref().unwrap())
        }
        //A Union block can also terminate the chain
        "union_block" => {
            let block = block(ctx, data, node)?;
            target_chain.push(ChainElement::Block(block));
            Ok(())
        }
        //break recursion on ;
        ";" => return Ok(()),
        other => Err(ParserError::Unexpected(format!(
            "Expected modifier or transform, got {other}",
        ))),
    }
}

pub fn overwrite_block(
    ctx: &mut ParserCtx,
    data: &[u8],
    node: &Node,
) -> Result<ScadStmt, ParserError> {
    //Is there a semantic difference?
    let _is_assign = match node.child(0).as_ref().unwrap().kind() {
        "let" => false,
        "assign" => true,
        other => {
            report_here("Expected 'let' or 'assign'".to_owned(), ctx.span(node));
            return Err(ParserError::MalformedNode(
                "expected 'let' or 'assign'".to_owned(),
            ));
        }
    };

    let overwrites = parenthesized_assignements(ctx, data, node.child(1).as_ref().unwrap())?;
    let block = block(ctx, data, node.child(2).as_ref().unwrap())?;

    Ok(ScadStmt::Overwrite { overwrites, block })
}

pub fn ifblock(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadStmt, ParserError> {
    let mut conditions = parenthesized_expression(
        ctx,
        data,
        node.child_by_field_name("condition").as_ref().unwrap(),
    )?;

    let head_span = ctx.span(node.child_by_field_name("condition").as_ref().unwrap());
    if conditions.is_empty() {
        report_here("'If' should have a condition", head_span);
        return Err(ParserError::MalformedNode("no condition".to_owned()));
    }
    if conditions.len() > 1 {
        report_here(
            "'If' should only have one condition. Consider using boolean operands like '&&' or '||' to build one expression",
            head_span,
        );
        return Err(ParserError::MalformedNode("multiple conditions".to_owned()));
    }
    let condition = conditions.remove(0);

    let mut block_or_exprblock = |node: &Node| match block(ctx, data, node) {
        Ok(block) => Ok(block),
        Err(_) => match stmt(ctx, data, node) {
            Ok(stmt) => Ok(ScadBlock {
                span: Span::empty(),
                stmts: vec![stmt],
            }),
            Err(e) => {
                report_here(
                    format!("Body should be statement or block: {e}"),
                    ctx.span(node),
                );
                Err(ParserError::MalformedNode(
                    "not block or statment".to_owned(),
                ))
            }
        },
    };

    let consequence =
        block_or_exprblock(node.child_by_field_name("consequence").as_ref().unwrap())?;
    let alternative = if let Some(_alternative) = node.child_by_field_name("alternative").as_ref() {
        //Dafuq?
        //println!("Alternative: {}", alternative.utf8_text(data).unwrap());
        //assert!(alternative.child(0).as_ref().unwrap().kind() == "else");
        Some(block_or_exprblock(node.child(4).as_ref().unwrap())?)
    } else {
        None
    };

    Ok(ScadStmt::IfBlock {
        head_span,
        condition,
        consequence,
        alternative,
    })
}

pub fn forblock(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadStmt, ParserError> {
    report_here("for-blocks not (yet) supported!", ctx.span(node));
    Err(ParserError::UnsupportedScadFeature("For blocks".to_owned()))
}

///Parses a (chain) of statements and appends them to _block_
pub fn stmt(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Result<ScadStmt, ParserError> {
    match node.kind() {
        "modifier_chain" | "transform_chain" | "union_block" => {
            let mut target_chain = Vec::new();
            match chain(ctx, &mut target_chain, data, node) {
                Ok(_) => {}
                Err(e) => {
                    if e == ParserError::Ignored {
                        return Err(e);
                    }
                    report_here(
                        format!("could not build this {}: {e}", node.kind()),
                        ctx.span(node),
                    );
                    return Err(e);
                }
            }
            Ok(ScadStmt::Chain {
                span: ctx.span(node),
                chain: target_chain,
            })
        }
        "include_statement" => {
            file_use_stmt(ctx, false, data, node)?;
            Ok(ScadStmt::None)
        }
        "assign_block" | "let_block" => overwrite_block(ctx, data, node),
        "if_block" => ifblock(ctx, data, node),
        "intersection_for_block" | "for_block" => forblock(ctx, data, node),
        "assert_statement" => Ok(ScadStmt::Assert),
        "comment" => Ok(ScadStmt::Comment(comment(ctx, data, node))),

        other => Err(ParserError::Unexpected(format!("Stmt: {other}"))),
    }
}
