use ahash::AHashMap;
use rvsdg::{
    builder::{OmegaBuilder, RegionBuilder},
    edge::{InputType, OutportLocation, OutputType},
    nodes::StructuralNode,
    NodeRef,
};
use vola_ast::{
    alge::{AlgeExpr, BinOp},
    common::Alge,
};
use vola_common::CommonError;

use crate::{
    err::{report_error, HirErr, PassResult},
    ops::HirOpTy,
    types::HirTypeState,
    HirEdge, HirOp, Querypool,
};

///Builds the alge expression in the given region(builder). Emits the out value
pub fn build_alge_expression_region<PN: StructuralNode>(
    query: &mut Querypool,
    region_builder: &mut RegionBuilder<HirOp, HirEdge, PN>,
    alge: AlgeExpr,
) -> PassResult<OutportLocation> {
    //Per definition the alge expression is a DAG that converges to a root node.
    // we explore from the DAG in reverse from the root node.
    //
    // named identifiers are tracked, and resolved in second pass.
    //
    // TODO:
    // - verify that no shadowed variables exist
    // - are we sure that no alge op has side effects?

    match alge {
        AlgeExpr::BinOp {
            op,
            left,
            right,
            span,
        } => {
            //step down to both sub ops, then emit our own op and connect sub_ops to our selfs.
            let left_out = build_alge_expression_region(query, region_builder, *left).unwrap();
            let right_out = build_alge_expression_region(query, region_builder, *right).unwrap();

            let hirop = match op {
                BinOp::Add => HirOpTy::Add,
                BinOp::Sub => HirOpTy::Sub,
                BinOp::Mul => HirOpTy::Mul,
                BinOp::Div => HirOpTy::Div,
                BinOp::Mod => HirOpTy::Mod,
            };

            let op = region_builder.insert_node(HirOp::new(span, hirop).with_inout(2, 1));
            region_builder
                .ctx_mut()
                .connect(
                    left_out,
                    op.as_inport_location(InputType::Input(0)),
                    HirEdge::Value,
                )
                .unwrap();
            region_builder
                .ctx_mut()
                .connect(
                    right_out,
                    op.as_inport_location(InputType::Input(2)),
                    HirEdge::Value,
                )
                .unwrap();
            PassResult::Ok(op.as_outport_location(OutputType::Output(0)))
        }
        AlgeExpr::Call { ident, args, span } => {
            // Call translates to the RVSDG apply node. However, for a call to be valid, we need to import the function
            // into our region. We let the RVSDG handle that, by using "RVSDG::import_into_region()" whenever we find the ident in the
            // context.
            if let Some(lambda_decl) = query.lookup_lambda_decl(&ident.imm) {
                todo!()
                /*
                //try to import the lambda decl into our region
                match region_builder.import_output(lambda_decl) {
                    Ok(node_port) => {
                        //TODO connect import port to a new apply node
                        todo!()
                    }
                    Err(e) => PassResult::Abort(HirErr::GraphError(e)),
                }*/
            } else {
                report_error(CommonError::new(
                    span,
                    HirErr::UnknownLambda(ident.imm.clone()),
                ));
                PassResult::Abort(HirErr::UnknownLambda(ident.imm))
            }
        }
        AlgeExpr::Float { float, span } => {
            todo!()
        }
        AlgeExpr::Identifier { ident, span } => {
            todo!()
        }
        AlgeExpr::Kw { kw, span } => {
            todo!()
        }
        AlgeExpr::List { list, span } => {
            todo!()
        }
        AlgeExpr::PrimAccess { ident, field, span } => {
            todo!()
        }
        AlgeExpr::UnaryOp { op, expr, span } => {
            todo!()
        }
    }
}

pub fn build_alge_lambda(
    query: &mut Querypool,
    omega_builder: &mut OmegaBuilder<HirOp, HirEdge>,
    alge: Alge,
) -> PassResult<NodeRef> {
    //right now we never export alges, so we start building the lambda region right away
    let (function_node, res) = omega_builder.new_function(false, |lambda| {
        //Push all arguments as arguments to the lambda node
        for arg in &alge.args {
            let arg_location = lambda.add_argument();
            //If we have a type state, note that for that port
            if let Some(ty) = &arg.ty {
                query
                    .type_states
                    .push_attrib(arg_location, HirTypeState::Assigned(ty.clone().into()));
            }
        }

        let expr_output =
            lambda.on_region(|reg| build_alge_expression_region(query, reg, alge.ret));

        if let PassResult::Abort(e) = expr_output {
            report_error(CommonError::new(alge.src, e.clone()));
            PassResult::Abort(e)
        } else {
            //Get the output, and connect it to the result
            let expression_output = expr_output.unwrap();
            let out_result = lambda.add_result();
            lambda
                .on_region(|reg| {
                    reg.ctx_mut()
                        .connect(expression_output, out_result, HirEdge::Value)
                })
                .unwrap();
            PassResult::Ok(())
        }
    });

    if let PassResult::Abort(e) = res {
        PassResult::Abort(e)
    } else {
        PassResult::Ok(function_node)
    }
}
