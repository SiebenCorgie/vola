/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use ahash::AHashMap;
use rvsdg::{
    edge::{InputType, OutportLocation, OutputType},
    region::RegionLocation,
    smallvec::{smallvec, SmallVec},
    NodeRef,
};
use vola_ast::common::Ident;
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{
    ast::block_builder::{BlockBuilder, BlockBuilderConfig},
    common::{LmdContext, Ty},
    error::OptError,
    Optimizer,
};

#[derive(Clone, Debug)]
pub struct FieldDef {
    pub span: Span,
    pub name: Ident,
    pub input_signature: SmallVec<[(Ident, Ty); 3]>,

    ///The λ-Node of this fielddef
    pub lambda: NodeRef,
    ///Shortcut to the λ-Node's inner region.
    pub lambda_region: RegionLocation,
}

impl Optimizer {
    pub fn add_field_def(
        &mut self,
        fielddef: vola_ast::csg::FieldDef,
    ) -> Result<NodeRef, OptError> {
        //Before starting, make sure we don't shadow a csgdef
        // or concept.
        //
        // NOTE: we _could_ shadow a concept, since they are in different namespaces
        // entirly. But I think that wouldn't be nice usability wise.

        if let Some(conceptdef) = self.concepts.get(&fielddef.name.0) {
            let err = OptError::Any {
                text: format!(
                    "Cannot define a field \"{}\" if there is already a concept with that name!",
                    fielddef.name.0
                ),
            };

            report(
                error_reporter(err.clone(), conceptdef.span.clone())
                    .with_label(
                        Label::new(conceptdef.span.clone()).with_message("first defined here"),
                    )
                    .with_label(
                        Label::new(fielddef.span.clone()).with_message("tried to redifine here"),
                    )
                    .finish(),
            );
            return Err(err);
        }

        if let Some(csgdef) = self.csg_node_defs.get(&fielddef.name.0) {
            let err = OptError::Any {
                text: format!(
                    "Cannot define a field \"{}\" if there is already a entity or operation with that name!",
                    fielddef.name.0
                ),
            };
            report(
                error_reporter(err.clone(), csgdef.span.clone())
                    .with_label(Label::new(csgdef.span.clone()).with_message("first defined here"))
                    .with_label(
                        Label::new(fielddef.span.clone()).with_message("tried to redifine here"),
                    )
                    .finish(),
            );
            return Err(err);
        }

        //finally make sure there is no such field as well
        if let Some(fdef) = self.field_def.get(&fielddef.name.0) {
            let err = OptError::Any {
                text: format!(
                    "Cannot define a field \"{}\" if there is already another field with that name!",
                    fielddef.name.0
                ),
            };
            report(
                error_reporter(err.clone(), fdef.span.clone())
                    .with_label(Label::new(fdef.span.clone()).with_message("first defined here"))
                    .with_label(
                        Label::new(fielddef.span.clone()).with_message("tried to redifine here"),
                    )
                    .finish(),
            );
            return Err(err);
        }

        let mut input_signature = SmallVec::new();
        for typed_ident in fielddef.args.iter() {
            let ty: Ty = typed_ident.ty.clone().into();
            input_signature.push((typed_ident.ident.clone(), ty));
        }

        //Setup the λ-Region. Always export
        let (lambda, lambda_region) = self.graph.on_omega_node(|omg| {
            let export_field_def = std::env::var("VOLA_EXPORT_ALL").is_ok();
            omg.new_function(export_field_def, |lmd_builder| {
                lmd_builder.on_region(|reg| reg.parent_location())
            })
        });

        let lmd_ctx =
            LmdContext::new_for_fielddef(&mut self.graph, &mut self.typemap, lambda, &fielddef);

        let mut block_builder = BlockBuilder {
            config: BlockBuilderConfig::field_def(),
            span: fielddef.span.clone(),
            csg_operands: AHashMap::with_capacity(0),
            return_type: smallvec![Ty::CSGTree],
            lmd_ctx,
            region: lambda_region,
            opt: self,
        };

        let return_expr_port = block_builder.build_block(fielddef.block)?;
        assert!(return_expr_port.len() == 1);
        let result_index = self
            .graph
            .node_mut(lambda)
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        //and wire it to the output
        let edg = self
            .graph
            .on_region(&lambda_region, |reg| {
                reg.connect_to_result(return_expr_port[0].1, InputType::Result(result_index))
                    .unwrap()
            })
            .unwrap();

        if let Some(ty) = &return_expr_port[0].0 {
            //typedef the edge to a CSGTree
            self.graph.edge_mut(edg).ty.set_type(ty.clone());
        } else {
            log::warn!("field-def outport was not type!");
        }

        let def = FieldDef {
            span: fielddef.span,
            name: fielddef.name,
            input_signature,
            lambda,
            lambda_region,
        };

        let fieldef_name = def.name.0.clone();

        //carry over debug information
        self.names.set(def.lambda.into(), def.name.0.clone());
        self.span_tags.set(def.lambda.into(), def.span.clone());
        //now tag all argument and result edges as defined. ArgTys are specified by the input-signature.
        //the refult will always be a csg-tree

        for (input_idx, (_, inputty)) in def.input_signature.iter().enumerate() {
            for edg in self
                .graph
                .node(def.lambda)
                .node_type
                .unwrap_lambda_ref()
                .argument(input_idx)
                .unwrap()
                .edges
                .clone()
            {
                self.graph.edge_mut(edg).ty.set_type(inputty.clone());
            }
            //Just to be sure, add the port as well. Otherwise we lose the type information
            //if _no_ edge is connected.
            self.typemap.set(
                OutportLocation {
                    node: def.lambda,
                    output: OutputType::Argument(input_idx),
                }
                .into(),
                inputty.clone(),
            );
        }

        self.field_def.insert(fieldef_name, def);

        Ok(lambda)
    }
}
