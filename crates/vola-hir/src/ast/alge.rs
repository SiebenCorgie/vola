use ahash::AHashMap;
use rvsdg::{attrib::AttribStore, builder::OmegaBuilder, NodeRef};
use vola_ast::common::Alge;

use crate::{
    err::{HirErr, PassResult},
    types::HirTypeState,
    HirEdge, HirOp,
};

pub fn build_alge_lambda(
    type_states: &mut AttribStore<HirTypeState>,
    omega_builder: &mut OmegaBuilder<HirOp, HirEdge>,
    alge: Alge,
) -> PassResult<NodeRef> {
    //right now we never export alges, so we start building the lambda region right away
    let function_node = omega_builder.new_function(None, |lambda| {
        //Push all arguments as arguments to the lambda node
        for arg in &alge.args {
            let arg_location = lambda.add_argument();
            //If we have a type state, note that for that port
            if let Some(ty) = &arg.ty {
                type_states.push_attrib(arg_location, HirTypeState::Assigned(ty.clone().into()));
            }
        }
    });

    //label the lambda
    omega_builder.on_region(|reg| {
        reg.ctx_mut().push_label(
            rvsdg::label::LabelLoc::Node(function_node),
            alge.ident.imm.clone(),
        )
    });

    PassResult::Ok(function_node)
}
