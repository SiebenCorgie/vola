use tree_sitter::Node;
use vola_ast::alge::AssignStmt;

use crate::{ParserCtx, error::ParserError, scad_ast::ScadAssignment, util};

pub fn assignment(
    ctx: &mut ParserCtx,
    data: &[u8],
    node: &Node,
) -> Result<ScadAssignment, ParserError> {
    if node.kind() != "assignment" {
        return Err(ParserError::MalformedNode(format!(
            "Expected assignment, got {}",
            node.kind()
        )));
    }

    //first _thingy_ is either a _special_ variable, or a _normal_ variable.
    //we handle both the same way though
    let var_ident = crate::expr::variable_name(
        ctx,
        data,
        node.child_by_field_name("left").as_ref().unwrap(),
    )?;

    let expr = crate::expr::expr(
        ctx,
        data,
        node.child_by_field_name("right").as_ref().unwrap(),
    )?;

    Ok(ScadAssignment {
        span: ctx.span(node),
        var: var_ident,
        expr: Box::new(expr),
    })
}
