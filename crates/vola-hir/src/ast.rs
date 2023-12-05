use vola_ast::Ast;

use crate::{err::HirErr, VolaHir};

///Tries to build a basic VolaHir representation from the AST.
pub fn tranform_into_ast(ast: Ast) -> Result<VolaHir, HirErr> {
    //The transformer will create a lambda for each function-like
    // expression in our AST. Fields are exported, all other expressions not
    // (for now, TODO: maybe allow export for vola libraries?)
    //
    // While building those initial λ-Nodes, we create a lookup table that maps the names to the nodes.
    // This allows us to connect all context variables in a second pass.
    //
    // At that point the basic un-typed, un-validated RVSDG of the AST should be build.

    Err(HirErr::Any)
}
