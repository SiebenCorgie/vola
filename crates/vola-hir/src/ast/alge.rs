use rvsdg::{builder::OmegaBuilder, NodeRef};
use vola_ast::common::Alge;

use crate::{err::HirErr, HirEdge, HirOp};

pub fn build_alge_lambda(
    omega_builder: &mut OmegaBuilder<HirOp, HirEdge>,
    alge: Alge,
) -> Result<NodeRef, HirErr> {
    Err(HirErr::Any)
}
