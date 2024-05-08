/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::{
    ast::block_builder::{BlockBuilder, BlockBuilderConfig, ReturnExpr},
    common::{LmdContext, Ty},
    error::OptError,
    Optimizer,
};
use ahash::AHashMap;
use rvsdg::{
    edge::{OutportLocation, OutputType},
    region::RegionLocation,
    smallvec::smallvec,
    NodeRef, SmallColl,
};
use vola_ast::{alge::ImplBlock, common::Ident, csg::CSGNodeTy};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ConceptImplKey {
    pub concept_name: String,
    ///Name of either a entity or operation definition.
    pub node_name: String,
}

///Contains the meta data of a concept implementation
pub struct ConceptImpl {
    pub span: Span,
    pub concept: Ident,
    #[allow(unused)]
    pub node_type: CSGNodeTy,

    ///All named operands an their index.
    #[allow(unused)]
    pub operands: AHashMap<String, usize>,

    ///The λ-Node of this concept
    pub lambda: NodeRef,
    ///Shortcut to the λ-Node's inner region.
    pub lambda_region: RegionLocation,
}

impl Optimizer {
    ///Adds this implementation block to the optimizer. Takes care of all the initial legalizing as well.
    pub fn add_impl_block(&mut self, implblock: ImplBlock) -> Result<NodeRef, OptError> {
        //First, check if an implementation for that concept and node already exists
        let key = ConceptImplKey {
            concept_name: implblock.concept.0.clone(),
            node_name: implblock.dst.0.clone(),
        };

        //Check that the impl block doesn't yet exist
        if let Some(existing_key) = self.concept_impl.get(&key) {
            let err = OptError::Any {
                text: format!(
                    "Tried to re-implement the concept {} for the entity or operation {}.",
                    implblock.concept.0, implblock.dst.0
                ),
            };

            report(
                error_reporter(err.clone(), implblock.span.clone())
                    .with_label(
                        Label::new(existing_key.span.clone()).with_message("first defined here"),
                    )
                    .with_label(Label::new(implblock.span.clone()).with_message("redefined here"))
                    .finish(),
            );
            return Err(err);
        }

        //Now try to retrieve the actual definition of the concept, and check that we are abiding to its definition
        let src_concept = if let Some(src_concept) = self.concepts.get(&implblock.concept.0) {
            src_concept
        } else {
            //Could not find the source concept, bail!
            let err = OptError::Any {
                text: format!("Concept {} is undefined!", implblock.concept.0),
            };

            report(
                error_reporter(err.clone(), implblock.span.clone())
                    .with_label(Label::new(implblock.span.clone()).with_message(&format!(
                        "Consider defining the concept {}",
                        implblock.concept.0
                    )))
                    .finish(),
            );
            return Err(err);
        };

        let src_csg_def = if let Some(src_csg_def) = self.csg_node_defs.get(&implblock.dst.0) {
            src_csg_def.clone()
        } else {
            let err = OptError::Any {
                text: format!(
                    "csg-node-type (entity or operation) {} is undefined!",
                    implblock.concept.0
                ),
            };

            report(
                error_reporter(err.clone(), implblock.span.clone())
                    .with_label(Label::new(implblock.span.clone()).with_message(&format!(
                        "Consider defining the entity or operation {}",
                        implblock.concept.0
                    )))
                    .finish(),
            );
            return Err(err);
        };

        //Check that the amount of arguments mirror the concept's argument count.
        if src_concept.src_ty.len() != implblock.concept_arg_naming.len() {
            let err = OptError::Any {
                text: format!(
                    "argument definition has {} arguments",
                    src_concept.src_ty.len(),
                ),
            };
            report(
                error_reporter(err.clone(), implblock.span.clone())
                    .with_label(Label::new(src_concept.span.clone()).with_message(&format!(
                        "concept defines {} arguments",
                        src_concept.src_ty.len()
                    )))
                    .with_label(Label::new(implblock.span.clone()).with_message(&format!(
                        "this should take {} arguments, not {}",
                        src_concept.src_ty.len(),
                        implblock.concept_arg_naming.len()
                    )))
                    .finish(),
            );
            return Err(err);
        }

        //Make sure that an entity has no sub operands
        if src_csg_def.ty == CSGNodeTy::Entity && implblock.operands.len() > 0 {
            let err = OptError::Any {
                text: format!(
                    "entity {} cannot have CSG operands. Only operations can have CSG operands!",
                    implblock.dst.0
                ),
            };
            report(
                error_reporter(err.clone(), implblock.span.clone())
                    .with_label(Label::new(implblock.span.clone()).with_message(
                        "Consider removing the operands, or implementing an \"operation\" instead",
                    ))
                    .finish(),
            );
            return Err(err);
        }

        //prebuild the concept_key so we can drop our src_concept references early.
        let concept_key = ConceptImplKey {
            concept_name: src_concept.name.0.clone(),
            node_name: src_csg_def.name.0.clone(),
        };

        //Build the operand->index map
        let operands = implblock
            .operands
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.0.clone(), idx))
            .collect::<AHashMap<_, _>>();

        //At this point we should have verified the overall signature. We therefore start building the actual λ-Node.
        // We do this by building the function-context helper that contains all concept-args as defined variables, as well as the
        // operation variables.

        //Create the lambda node. NOTE that we currently alway export. For debug purposes.
        let (lmd, lmd_region) = self.graph.on_omega_node(|omg| {
            let export_impl_block = std::env::var("VOLA_EXPORT_ALL").is_ok();
            omg.new_function(export_impl_block, |lmd_builder| {
                lmd_builder.on_region(|reg| reg.parent_location())
            })
        });

        //register the context ports in the same order as the operands map was build

        for (idx, _name) in implblock.operands.iter().enumerate() {
            let lmdcvidx = self
                .graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_context_variable();
            assert!(idx == lmdcvidx);
            //Preset the CVs with the CSGTree type. Since we don't known the exact node at the moment
            self.typemap.set(
                OutportLocation {
                    node: lmd,
                    output: OutputType::ContextVariableArgument(lmdcvidx),
                }
                .into(),
                Ty::CSGTree,
            );
        }

        let lmd_ctx = LmdContext::new_for_impl_block(
            &mut self.graph,
            &mut self.typemap,
            lmd.clone(),
            &implblock,
            &src_csg_def,
            &src_concept,
        );

        //This effectively builds us the type-signature of the impl block based on the calling-convention
        // that is realised in LmdContext::new_for_impl_block.
        // this means that first all csg-def args are supplied, followed by the args to the concept that is implemented.
        let impl_block_arg_signature = src_csg_def
            .args
            .iter()
            .map(|t| t.ty.clone())
            .chain(
                //NOTE: now chain the concept-args. This is we do that by indexing into the src_concept's definition
                implblock
                    .concept_arg_naming
                    .iter()
                    .enumerate()
                    .map(|(idx, _)| src_concept.src_ty[idx].clone()),
            )
            .collect::<SmallColl<_>>();
        let result_sig: Ty = src_concept.dst_ty.clone().into();

        let mut block_builder = BlockBuilder {
            config: BlockBuilderConfig::impl_block(src_csg_def.ty.clone()),
            span: implblock.span.clone(),
            csg_operands: operands.clone(),
            return_type: smallvec![src_concept.dst_ty.clone().into()],
            lmd_ctx,
            region: lmd_region,
            opt: self,
        };

        let return_expr_port = block_builder.build_block(implblock.block)?;
        assert!(return_expr_port.len() == 1);
        //hookup the return port and type it
        let result_index = self
            .graph
            .node_mut(lmd)
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        let result_edge = self
            .graph
            .on_region(&lmd_region, |reg| {
                reg.connect_to_result(
                    return_expr_port[0].1,
                    rvsdg::edge::InputType::Result(result_index),
                )
                .unwrap()
            })
            .unwrap();

        if let Some(retty) = &return_expr_port[0].0 {
            let expected_ty: Ty = result_sig.clone().into();
            assert!(&expected_ty == retty);
        }

        //set the result edge with the known result_type of the alge_fn
        self.graph
            .edge_mut(result_edge)
            .ty
            .set_type(result_sig.clone());

        //Now build the final impl struct
        let concept_impl = ConceptImpl {
            span: implblock.span.clone(),
            concept: implblock.concept.clone(),
            node_type: src_csg_def.ty.clone(),
            operands,

            lambda: lmd,
            lambda_region: lmd_region,
        };

        //After finishing (successfully) add the impl description to the Optimizer so we can
        // find that later on if needed and return
        //NOTE: _should be the same, but could also be changed at some point I guess_
        let lmd = concept_impl.lambda;

        //now append types to edges based on impl-block signature, as well add some general debug info for later.
        self.names.set(
            lmd.into(),
            format!("Impl {} for {}", src_csg_def.name.0, concept_impl.concept.0),
        );
        self.span_tags.set(lmd.into(), concept_impl.span.clone());

        //This iterator maps us the concept's argument types in order

        //now we chain first the csg-def's arg-types, followed by the concept args (which is our definition), to
        // setup the edge types accordingly
        for (inpidx, inputty) in impl_block_arg_signature.into_iter().enumerate() {
            for edg in self
                .graph
                .node(lmd)
                .node_type
                .unwrap_lambda_ref()
                .argument(inpidx)
                .unwrap()
                .edges
                .clone()
            {
                self.graph.edge_mut(edg).ty.set_type(inputty.clone().into());
            }
        }

        let old = self.concept_impl.insert(concept_key, concept_impl);
        assert!(old.is_none(), "Had an old concept + node combination already, should have been caught before adding it!");

        Ok(lmd)
    }
}
