/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use rvsdg::{region::RegionLocation, SmallColl};
use vola_ast::{
    alge::Func,
    common::CTArg,
    csg::{CSGConcept, CsgDef, ImplBlock},
};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{
    common::Ty,
    graph::auxiliary::{Function, Impl, ImplKey},
    OptError, Optimizer,
};

use super::block_build::BlockCtx;

impl Optimizer {
    pub(crate) fn add_concept(
        &mut self,
        span: Span,
        ct_args: Vec<CTArg>,
        concept: CSGConcept,
    ) -> Result<(), OptError> {
        if let Some(existing_concept) = self.concepts.get(&concept.name.0) {
            let err = OptError::Any {
                text: format!("Concept {} was already defined", existing_concept.name.0),
            };

            report(
                error_reporter(err.clone(), concept.span.clone())
                    .with_label(
                        Label::new(existing_concept.span.clone())
                            .with_message("first defined here"),
                    )
                    .with_label(Label::new(concept.span.clone()).with_message("redefined here"))
                    .finish(),
            );

            Err(err)
        } else {
            //No yet in collection, therefore push
            self.concepts.insert(concept.name.0.clone(), concept);
            Ok(())
        }
    }

    pub(crate) fn add_csgdef(
        &mut self,
        span: Span,
        ct_args: Vec<CTArg>,
        csgdef: CsgDef,
    ) -> Result<(), OptError> {
        //similar to the concept case, test if there is already one, if not, push
        if let Some(existing_csg) = self.csg_node_defs.get(&csgdef.name.0) {
            let err = OptError::Any {
                text: format!("Operation or Entity {} was already defined. \nNote that operations and entities share one name space.", existing_csg.name.0),
            };

            report(
                error_reporter(err.clone(), csgdef.span.clone())
                    .with_label(
                        Label::new(existing_csg.span.clone()).with_message("first defined here"),
                    )
                    .with_label(Label::new(csgdef.span.clone()).with_message("redefined here"))
                    .finish(),
            );

            Err(err)
        } else {
            //No yet in collection, therefore push
            self.csg_node_defs.insert(csgdef.name.0.clone(), csgdef);
            Ok(())
        }
    }

    pub(crate) fn add_func(
        &mut self,
        span: Span,
        ct_args: Vec<CTArg>,
        func: Func,
    ) -> Result<(), OptError> {
        //Parse the function signature, then launch block building
        let name = func.name.0.clone();
        if let Some(existing) = self.functions.get(&name) {
            let err = OptError::Any {
                text: format!("function \"{name}\" already exists!"),
            };
            report(
                error_reporter(err.clone(), span)
                    .with_label(
                        Label::new(existing.def_span.clone().into())
                            .with_message("first defined here"),
                    )
                    .with_label(Label::new(func.head_span().into()).with_message("redefined here"))
                    .finish(),
            );
        }

        let args: SmallColl<(String, Ty)> = func
            .args
            .iter()
            .map(|arg| (arg.ident.0.clone(), arg.clone().ty.into()))
            .collect();
        let region_span = func.span.clone();
        let def_span = func.head_span();
        let return_type: Ty = func.return_type.into();

        let mut initial_context = BlockCtx::empty();

        //setup the lambda node accordingly, as well as the block context, then launch the block builder.
        let lambda = self.graph.on_omega_node(|omg| {
            let (lmd, _) = omg.new_function(func.is_export, |lmd| {
                //setup the args an results
                for arg in &args {
                    let port = lmd.add_argument();
                    let existing = initial_context.defined_vars.insert(arg.0.clone(), port);
                    assert!(
                        existing.is_none(),
                        "encountered duplicated argument name: {}",
                        arg.0
                    );
                    //Also tag with the correct type already
                    self.typemap.set(port.into(), arg.1.clone());
                }
                //setup result
                let result_port = lmd.add_result();
                self.typemap.set(result_port.into(), return_type.clone());
                initial_context.result_connector = Some(result_port);
            });
            lmd
        });

        self.build_block(
            RegionLocation {
                node: lambda,
                region_index: 0,
            },
            func.block,
            initial_context,
        )?;

        //At this point everything should be hooked-up and typed. therfore we can return

        let interned = Function {
            name: func.name.0,
            region_span,
            def_span,
            lambda,
            args,
            return_type,
        };
        self.functions.insert(name, interned);

        Ok(())
    }

    pub(crate) fn add_implblock(
        &mut self,
        span: Span,
        ct_args: Vec<CTArg>,
        implblock: ImplBlock,
    ) -> Result<(), OptError> {
        //Impl blocks function similar to normal functions, but
        //we additionally import all operands at the first n-cvs

        //The concept that is implemented
        let concept = implblock.concept.0.clone();
        //the CSG node it is implemented for
        let csg = implblock.dst.0.clone();

        let implkey = ImplKey { concept, node: csg };

        if let Some(existing) = self.concept_impl.get(&implkey) {
            let err = OptError::Any {
                text: format!(
                    "implementation for {}::{} already exists!",
                    implkey.node, implkey.concept
                ),
            };
            report(
                error_reporter(err.clone(), span.clone())
                    .with_label(
                        Label::new(existing.def_span.clone().into())
                            .with_message("first defined here"),
                    )
                    .with_label(
                        Label::new(implblock.head_span().into()).with_message("redefined here"),
                    )
                    .finish(),
            );
        }

        //try to get the concept, so we can inver the arg-type and the return type
        let (arg_ty, return_ty) = if let Some(concept) = self.concepts.get(&implkey.concept) {
            (concept.src_ty.clone(), concept.dst_ty.clone())
        } else {
            let err = OptError::Any {
                text: format!("Concept \"{}\" was undefined!", implkey.concept),
            };
            report(
                error_reporter(err.clone(), span.clone().into())
                    .with_label(
                        Label::new(implblock.head_span()).with_message("For this implementation"),
                    )
                    .finish(),
            );
            return Err(err);
        };

        let mut initial_context = BlockCtx::empty();

        //setup the lambda node accordingly, as well as the block context, then launch the block builder.
        let lambda = self.graph.on_omega_node(|omg| {
            let (lmd, _) = omg.new_function(false, |lmd| {
                //setup all operands as CV-Vars, and the arg as ... arg
                for operand in &implblock.operands {
                    let (_, within) = lmd.add_context_variable();
                    let old = initial_context
                        .defined_vars
                        .insert(operand.0.clone(), within);
                    assert!(old.is_none());
                    //Directly set to CSG type
                    self.typemap.set(within.into(), Ty::CSG);
                }

                let port = lmd.add_argument();
                let existing = initial_context
                    .defined_vars
                    .insert(implblock.concept_arg_name.0.clone(), port);
                assert!(
                    existing.is_none(),
                    "encountered duplicated argument name: {}",
                    implblock.concept_arg_name.0
                );
                //Also tag with the correct type already
                self.typemap.set(port.into(), arg_ty.clone().into());

                //setup result
                let result_port = lmd.add_result();
                self.typemap
                    .set(result_port.into(), return_ty.clone().into());
                initial_context.result_connector = Some(result_port);
            });
            lmd
        });

        let def_span = implblock.head_span();

        self.build_block(
            RegionLocation {
                node: lambda,
                region_index: 0,
            },
            implblock.block,
            initial_context,
        )?;

        //At this point everything should be hooked-up and typed. therfore we can return

        let interned = Impl {
            region_span: span,
            def_span,
            concept: implblock.concept.0.clone(),
            lambda,
            subtrees: implblock.operands.into_iter().map(|a| a.0).collect(),
            arg: (implblock.concept_arg_name.0, arg_ty.into()),
            return_type: return_ty.into(),
        };
        self.concept_impl.insert(implkey, interned);

        Ok(())
    }
}
