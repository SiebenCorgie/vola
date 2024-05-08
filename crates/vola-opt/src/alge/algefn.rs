/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use ahash::AHashMap;
use rvsdg::{
    region::RegionLocation,
    smallvec::{smallvec, SmallVec},
    NodeRef,
};
use vola_ast::{common::{Ident, Stmt}};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{
    alge::WkOp, ast::block_builder::{BlockBuilder, BlockBuilderConfig}, common::{LmdContext, Ty}, OptError, Optimizer
};

pub struct AlgeFn {
    pub span: Span,
    pub name: Ident,
    pub args: SmallVec<[(String, Ty); 3]>,
    pub retty: Ty,

    pub lambda: NodeRef,
    pub lambda_region: RegionLocation,
}


impl Optimizer {
    pub fn add_alge_fn(&mut self, alge_fn: vola_ast::alge::AlgeFunc) -> Result<NodeRef, OptError> {
        //The alge fn is conceptually similar to the field def.
        //however, we only allow alge expressions and statements. No Eval can be used
        //and the return expression must statify the return-type constrains (at some point ^^).

        if let Some(_special_node) = WkOp::try_parse(&alge_fn.name.0) {
            let err = OptError::Any{ 
                text: format!("Cannot use name \"{}\" for algebraic function, since a that would shadow a build-in function with the same name!", alge_fn.name.0), 
            };

            report(
                error_reporter(err.clone(), alge_fn.span.clone())
                    .with_label(Label::new(alge_fn.span.clone()).with_message("Consider renaming this function"))
                    .finish(),
            );
            return Err(err);
        }

        if let Some(existing_key) = self.alge_fn.get(&alge_fn.name.0) {
            let err = OptError::Any {
                text: format!("Cannot define \"{}\" multiple times!", alge_fn.name.0),
            };
            report(
                error_reporter(err.clone(), alge_fn.span.clone())
                    .with_label(Label::new(existing_key.span.clone()).with_message("fist defined here"))
                    .with_label(Label::new(alge_fn.span.clone()).with_message("Tried to redefine here"))
                    .finish(),
            );
            return Err(err);
        }

        let (lambda, lambda_region) = self.graph.on_omega_node(|omg| {
            let export_fn = std::env::var("VOLA_EXPORT_ALL").is_ok();
            omg.new_function(export_fn, |lmd_buider| {
                lmd_buider.on_region(|reg| reg.parent_location())
            })
        });

        let args = alge_fn
            .args
            .iter()
            .map(|arg| (arg.ident.0.clone(), Ty::from(arg.ty.clone())))
            .collect();

        let lmd_ctx =
            LmdContext::new_for_alge_fn(&mut self.graph, &mut self.typemap, lambda, &alge_fn);

        let mut block_builder = BlockBuilder{
            config: BlockBuilderConfig::alge_fn(),
            span: alge_fn.span.clone(),
            csg_operands: AHashMap::with_capacity(0),
            return_type: smallvec![alge_fn.return_type.clone().into()],
            lmd_ctx,
            region: lambda_region,
            opt: self
        };

        //build the block
        let output_port = block_builder.build_block(alge_fn.block)?;
        assert!(output_port.len() == 1);
        
        let result_index = self
            .graph
            .node_mut(lambda)
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        let result_edge = self
            .graph
            .on_region(&lambda_region, |reg| {
                reg.connect_to_result(
                    output_port[0].1,
                    rvsdg::edge::InputType::Result(result_index),
                )
                .unwrap()
            })
            .unwrap();

        if let Some(retty) = &output_port[0].0{
            let expected_ty: Ty = alge_fn.return_type.clone().into();
            assert!(&expected_ty == retty);
        }
        
        //set the result edge with the known result_type of the alge_fn
        self.graph
            .edge_mut(result_edge)
            .ty
            .set_type(alge_fn.return_type.clone().into());
        
        //After emitting, setup the AlgeFn description and return
        let algedef = AlgeFn {
            span: alge_fn.span.clone(),
            name: alge_fn.name.clone().into(),
            args,
            retty: alge_fn.return_type.clone().into(),
            lambda,
            lambda_region,
        };

        let algename = algedef.name.0.clone();
        self.names.set(algedef.lambda.into(), algename.clone());
        self.span_tags
            .set(algedef.lambda.into(), algedef.span.clone());

        //TODO: set the out-edge type?
        assert!(self.alge_fn.insert(algename, algedef).is_none());
        Ok(lambda)
    }
}
