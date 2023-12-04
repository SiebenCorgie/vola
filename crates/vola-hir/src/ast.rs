use vola_ast::Ast;

use crate::{err::HirErr, VolaHir};

pub fn tranform_into_ast(ast: Ast) -> Result<VolaHir, HirErr> {
    Err(HirErr::Any)
}
