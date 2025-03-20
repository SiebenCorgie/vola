use std::path::{Path, PathBuf};

use smallvec::SmallVec;
use tree_sitter::Node;
use vola_ast::{
    alge::{AssignStmt, Expr},
    common::{Call, Comment, Stmt},
    csg::CsgStmt,
};

use crate::{ParserCtx, error::ParserError};

///Parses the _use_ statement. This will effectively just concat the modules in the
///given path to our AST. Therefore we return _nothing_, but append the path to the `ctx`.
pub fn use_stmt(ctx: &mut ParserCtx, dta: &[u8], node: &Node) -> Result<(), ParserError> {
    let path = node.child(1).ok_or(ParserError::MalformedNode(
        "use statement's second part must be path!".to_owned(),
    ))?;
    let path_string = path.utf8_text(dta).map_err(|e| ParserError::from(e))?;
    let resolve_path = ctx.try_resolve_local_path(Path::new(path_string))?;
    ctx.resolve_uses.push(resolve_path);
    Ok(())
}

///Parses the _include_ statement. This will effectively just concat the modules in the
///given path to our AST. Therefore we return _nothing_, but append the path to the `ctx`.
pub fn include_stmt(ctx: &mut ParserCtx, dta: &[u8], node: &Node) -> Result<(), ParserError> {
    let path = node.child(1).ok_or(ParserError::MalformedNode(
        "include statement's second part must be path!".to_owned(),
    ))?;
    let path_string = path.utf8_text(dta).map_err(|e| ParserError::from(e))?;
    let resolve_path = ctx.try_resolve_local_path(Path::new(path_string))?;
    ctx.resolve_includes.push(resolve_path);
    Ok(())
}

pub enum ScadBoolOp {
    Union,
    Intersection,
    Difference,
}

pub enum ScadStmt {
    ///Implicit union of ScadStmts based on a given range
    ForBlock {
        range_start: Expr,
        range_end: Expr,
        increment: Expr,
        block: Vec<ScadStmt>,
        unifier: ScadBoolOp,
    },
    Chain {
        chain: Vec<Call>,
    },
    Assign(AssignStmt),
    IfBlock {
        condition: Expr,
        consequence: Expr,
        alternative: Option<Expr>,
    },
    //Either a let, or assign statement, followed by a _bodied_ block
    Overwrite {
        overwrites: Vec<AssignStmt>,
        block: Vec<ScadStmt>,
    },
    IncludeStmt(PathBuf),
    Comment(Comment),
}

///Parses a _chain_, which is a series of calls ended with a ';'
fn chain(
    ctx: &mut ParserCtx,
    chain_builder: &mut Vec<Call>,
    dta: &[u8],
    node: &Node,
) -> Result<(), ParserError> {
    match node.kind() {
        "modifier_chain" | "transform_chain" => {
            let modifier = module_call(ctx, dta, node.child(0).as_ref().unwrap())?;
            chain_builder.push(modifier);
            //now recurse to next element
            chain(ctx, chain_builder, dta, node.child(1).as_ref().unwrap())
        }
        ";" => return Ok(()),
        other => {
            return Err(ParserError::Unexpected(format!(
                "Expected modifier or transform chain, got {}",
                other,
            )));
        }
    }
}

pub fn stmt(
    ctx: &mut ParserCtx,
    parent_block: &mut Vec<ScadStmt>,
    dta: &[u8],
    node: &Node,
) -> Result<(), ParserError> {
    match node.kind() {
        "for_block" => {
            todo!()
        }
        "intersection_for_block" => {
            todo!()
        }
        "if_block" => {
            todo!()
        }
        "let_block" => {
            todo!()
        }
        "assign_block" => {
            todo!()
        }
        "union_block" => {
            todo!()
        }
        "transform_chain" | "modifier_chain" => {
            println!("Enter Transform!");
            //module call is basicall a CSG-Call
            let mut subchain = Vec::new();
            let next_node = chain(ctx, &mut subchain, dta, node)?;

            let stmt = ScadStmt::Chain { chain: subchain };
            parent_block.push(stmt);
            println!("Pushed transform!");
            Ok(())
        }
        "include_statement" => {
            let path = node
                .child(1)
                .as_ref()
                .unwrap()
                .utf8_text(dta)
                .map_err(|e| ParserError::from(e))?;
            let path_buf = PathBuf::from(path);
            parent_block.push(ScadStmt::IncludeStmt(path_buf));
            Ok(())
        }
        "assert_statement" => {
            ctx.deep_warnings.push((
                ctx.span(node),
                "Asserts not supported in Vola, emitting it as a comment".to_string(),
            ));

            parent_block.push(ScadStmt::Comment(Comment {
                span: ctx.span(node),
                content: node
                    .utf8_text(dta)
                    .map_err(|e| ParserError::from(e))?
                    .to_string(),
            }));

            Ok(())
        }
        other => Err(ParserError::MalformedNode(format!(
            "Expected Statement of any kind, got {}",
            other
        ))),
    }
}

pub fn module_call(ctx: &mut ParserCtx, dta: &[u8], node: &Node) -> Result<Call, ParserError> {
    if node.kind() != "module_call" {
        return Err(ParserError::Unexpected("module_call".to_owned()));
    }

    let called_ident =
        crate::util::identifier(ctx, dta, node.child_by_field_name("name").as_ref().unwrap())?;
    let args = arguments(
        ctx,
        dta,
        node.child_by_field_name("arguments").as_ref().unwrap(),
    )?;

    Ok(Call {
        span: ctx.span(node),
        ident: called_ident,
        args,
    })
}

pub fn arguments(
    ctx: &mut ParserCtx,
    dta: &[u8],
    node: &Node,
) -> Result<SmallVec<[Expr; 3]>, ParserError> {
    if node.kind() != "arguments" {
        return Err(ParserError::Unexpected(format!(
            "expected arguments, got {}",
            node.kind()
        )));
    }

    let mut args = SmallVec::new();

    let mut cursor = node.walk();
    let mut walker = node.children(&mut cursor);

    //should start / end with "(" / ")".
    if walker.next().unwrap().kind() != "(" {
        return Err(ParserError::MalformedNode("Expected \"(\"".to_owned()));
    }

    //now there should be a sequence of "expr" ","
    while let Some(next) = walker.next() {
        let expr = crate::expr::expr(ctx, dta, &next)?;
        //next should either be "," or ")"
        args.push(expr);

        match walker.next().unwrap().kind() {
            "," => {}
            ")" => break,
            other => {
                return Err(ParserError::Unexpected(format!(
                    "Expected ',' or ')', got {}",
                    other
                )));
            }
        }
    }

    if walker.next().is_some() {
        return Err(ParserError::MalformedNode(
            "Malformed arguments list".to_owned(),
        ));
    }

    Ok(args)
}
