use ahash::AHashMap;
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    smallvec::SmallVec,
    NodeRef, SmallColl,
};
use vola_ast::common::Ident;

use crate::{
    ast::block_builder::{BlockBuilder, BlockBuilderConfig},
    common::{LmdContext, Ty},
    error::OptError,
    Optimizer,
};
use vola_common::Span;

///All meta information to an export_fn.
pub struct ExportFn {
    pub span: Span,
    ///The input signature that is exported to the linkage-attributes
    #[allow(unused)]
    pub input_signature: SmallVec<[(Ident, Ty); 3]>,
    ///The outputs that are exported from the module.
    pub output_signature: SmallVec<[Ty; 3]>,

    ///Name of the function, also the one we use to export it.
    pub ident: Ident,

    ///The λ-Node of this concept
    pub lambda: NodeRef,
    ///Shortcut to the λ-Node's inner region.
    pub lambda_region: RegionLocation,
}

impl Optimizer {
    fn wire_access(
        &mut self,
        region: RegionLocation,
        ports: SmallColl<(Ty, OutportLocation)>,
    ) -> Result<(), OptError> {
        for (expected_idx, (ty, port)) in ports.into_iter().enumerate() {
            //add an result port to the lambda node
            let resultidx = self
                .graph
                .node_mut(region.node)
                .node_type
                .unwrap_lambda_mut()
                .add_result();
            assert!(resultidx == expected_idx);

            let edg = self
                .graph
                .on_region(&region, |reg| {
                    reg.connect_to_result(port, InputType::Result(resultidx))
                        .unwrap()
                })
                .unwrap();

            self.graph.edge_mut(edg).ty.set_type(ty.clone());
        }

        Ok(())
    }

    pub fn add_export_fn(
        &mut self,
        exportfn: vola_ast::csg::ExportFn,
    ) -> Result<NodeRef, OptError> {
        //TODO right now we can _kind_shadow_ concepts, operations, entities and filed
        // defs. So I guess it would be nice if we couldn't ?

        let mut input_signature = SmallVec::new();
        for typed_ident in exportfn.args.iter() {
            let ty: Ty = typed_ident.ty.clone().into();
            input_signature.push((typed_ident.ident.clone(), ty));
        }

        //Will be calculated later on
        let output_signature = SmallVec::new();

        //Setup the λ-Region. Always export
        let (lambda, lambda_region) = self.graph.on_omega_node(|omg| {
            omg.new_function(true, |lmd_builder| {
                lmd_builder.on_region(|reg| reg.parent_location())
            })
        });

        let lmd_ctx =
            LmdContext::new_for_exportfn(&mut self.graph, &mut self.typemap, lambda, &exportfn);

        let mut block_builder = BlockBuilder {
            config: BlockBuilderConfig::export_fn(),
            span: exportfn.span.clone(),
            csg_operands: AHashMap::with_capacity(0),
            return_type: SmallColl::new(),
            lmd_ctx,
            region: lambda_region,
            opt: self,
        };

        let outports: SmallColl<_> = block_builder
            .build_block(exportfn.block)?
            .into_iter()
            .map(|(ty, port)| (ty.expect("export must always be typed!"), port))
            .collect();

        self.wire_access(lambda_region, outports)?;

        let exportfn = ExportFn {
            span: exportfn.span.clone(),
            input_signature,
            output_signature,
            ident: exportfn.name.clone(),

            lambda,
            lambda_region,
        };

        let export_name = exportfn.ident.0.clone();
        //Set the name of this function
        self.names.set(exportfn.lambda.into(), export_name.clone());
        self.span_tags
            .set(exportfn.lambda.into(), exportfn.span.clone());

        //add port type information
        for (inpidx, (_, inputty)) in exportfn.input_signature.iter().enumerate() {
            self.typemap.set(
                OutportLocation {
                    node: exportfn.lambda,
                    output: OutputType::Argument(inpidx),
                }
                .into(),
                inputty.clone(),
            );

            for edg in self
                .graph
                .node(exportfn.lambda)
                .node_type
                .unwrap_lambda_ref()
                .argument(inpidx)
                .unwrap()
                .edges
                .clone()
            {
                self.graph.edge_mut(edg).ty.set_type(inputty.clone());
            }
        }

        for (outpidx, ty) in exportfn.output_signature.iter().enumerate() {
            self.typemap.set(
                InportLocation {
                    node: exportfn.lambda,
                    input: InputType::Result(outpidx),
                }
                .into(),
                ty.clone(),
            );

            if let Some(edg) = self
                .graph
                .node(exportfn.lambda)
                .node_type
                .unwrap_lambda_ref()
                .result(outpidx)
                .unwrap()
                .edge
                .clone()
            {
                self.graph.edge_mut(edg).ty.set_type(ty.clone());
            }
        }

        self.export_fn.insert(export_name, exportfn);

        Ok(lambda)
    }
}
