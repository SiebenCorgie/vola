/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use rvsdg::{
    edge::{InputType, OutputType},
    region::RegionLocation,
    SmallColl,
};
use vola_ast::{
    alge::Func,
    common::CTArg,
    csg::{CsgConcept, CsgDef, ImplBlock},
};
use vola_common::{Span, VolaError};

use crate::{
    common::Ty,
    graph::auxiliary::{Function, Impl, ImplKey},
    OptEdge, OptError, Optimizer,
};

use super::block_build::BlockCtx;

impl Optimizer {
    ///Builds the block context for the omega node. This basically appends all named
    ///static data, as well as callable (i.e. non-impl) functions.
    fn omega_context(&self) -> Result<BlockCtx, VolaError<OptError>> {
        //NOTE: we currently have no static data, but there will be ~data~ at some point.
        let mut ctx = BlockCtx::empty(self.graph.toplevel_region());
        for func in self.functions.values() {
            //define the function as plain old value
            ctx.define_var(
                func.name.clone(),
                func.lambda
                    .as_outport_location(OutputType::LambdaDeclaration),
            )
            .map_err(|e| VolaError::error_here(e, func.def_span.clone(), "could not define var"))?
        }

        Ok(ctx)
    }

    pub(crate) fn add_concept(
        &mut self,
        _span: Span,
        _ct_args: Vec<CTArg>,
        concept: CsgConcept,
    ) -> Result<(), VolaError<OptError>> {
        if let Some(existing_concept) = self.concepts.get(&concept.name.0) {
            let err = OptError::Any {
                text: format!("Concept {} was already defined", existing_concept.name.0),
            };
            Err(
                VolaError::error_here(err, concept.span.clone(), "redefined here")
                    .with_label(existing_concept.span.clone(), "first defined here"),
            )
        } else {
            //No yet in collection, therefore push
            self.concepts.insert(concept.name.0.clone(), concept);
            Ok(())
        }
    }

    pub(crate) fn add_csgdef(
        &mut self,
        _span: Span,
        _ct_args: Vec<CTArg>,
        csgdef: CsgDef,
    ) -> Result<(), VolaError<OptError>> {
        //similar to the concept case, test if there is already one, if not, push
        if let Some(existing_csg) = self.csg_node_defs.get(&csgdef.name.0) {
            let err = OptError::Any {
                text: format!("Operation or Entity {} was already defined. \nNote that operations and entities share one name space.", existing_csg.name.0),
            };
            Err(
                VolaError::error_here(err, csgdef.span.clone(), "redefined here")
                    .with_label(existing_csg.span.clone(), "first defined here"),
            )
        } else {
            //No yet in collection, therefore push
            self.csg_node_defs.insert(csgdef.name.0.clone(), csgdef);
            Ok(())
        }
    }

    ///Creates the λ node in the omega region,  but does not fill it (yet).
    pub(crate) fn define_func(&mut self, func: &Func) -> Result<(), VolaError<OptError>> {
        //Parse the function signature, and build the entry in the lookup table
        let name = func.name.0.clone();
        if let Some(existing) = self.functions.get(&name) {
            let err = OptError::Any {
                text: format!("function \"{name}\" already exists!"),
            };
            return Err(
                VolaError::error_here(err, func.head_span(), "redefined here")
                    .with_label(existing.def_span.clone(), "first defined here"),
            );
        }

        //Also make sure that no concept or csg-def with that name exists
        if self.concepts.contains_key(&name) || self.csg_node_defs.contains_key(&name) {
            let err = OptError::Any {
                text: format!("function \"{name}\" can not be named after a concept, or CSG-Operation/Entitiy!"),
            };
            return Err(VolaError::error_here(err, func.span.clone(), "here"));
        }

        let args: SmallColl<(String, Ty)> = func
            .args
            .iter()
            .map(|arg| (arg.ident.0.clone(), arg.clone().ty.into()))
            .collect();
        let region_span = func.span.clone();
        let def_span = func.head_span();
        let return_type: Ty = func.return_type.clone().into();

        //setup the lambda node accordingly, as well as the block context, then launch the block builder.
        let lambda = self.graph.on_omega_node(|omg| {
            let (lmd, _) = omg.new_function(func.is_export, |lmd| {
                //setup the args an results
                for arg in &args {
                    let port = lmd.add_argument();
                    //Also tag with the correct type already
                    self.typemap.set(port.into(), arg.1.clone());
                }
                //setup result
                let result_port = lmd.add_result();
                self.typemap.set(result_port.into(), return_type.clone());
            });
            lmd
        });

        //At this point everything should be hooked-up and typed. therfore we can return
        self.names.set(lambda.into(), func.name.0.clone());
        self.span_tags.set(lambda.into(), func.head_span());
        let interned = Function {
            name: func.name.0.clone(),
            region_span,
            def_span,
            lambda,
            args,
            return_type,
        };
        self.functions.insert(name, interned);

        Ok(())
    }

    ///Uses the (already defined) λ-node and builds the code of the body.
    pub(crate) fn build_func_block(
        &mut self,
        _span: Span,
        _ct_args: Vec<CTArg>,
        func: Func,
    ) -> Result<(), VolaError<OptError>> {
        //Parse the function signature, then launch block building
        let name = func.name.0.clone();
        let return_span = func
            .block
            .retexpr
            .as_ref()
            .map(|retex| retex.span.clone())
            .unwrap_or(func.span.clone());
        //build the omega context
        let mut ctx = self.omega_context()?;

        //Find the λ-node, the open the context and define all args in terms of the λ's body-scope
        let lambda = {
            let existing = self
                .functions
                .get(&name)
                .expect("Function should have been defined already!");
            //parse all args into the ctx's scope. This scope won't be closed, so we also don't mutate.
            ctx.open_new_scope(existing.region(), false);
            //NOTE: on paper there shouldn't be any CVs _yet_
            for (argidx, arg) in existing.args.iter().enumerate() {
                let port = existing
                    .lambda
                    .as_outport_location(OutputType::Argument(argidx));
                let found_type = self
                    .typemap
                    .get(&port.into())
                    .expect("Expected port's type to be set already!");
                assert!(found_type == &arg.1);
                //matches, so setup
                ctx.define_var(arg.0.to_owned(), port).map_err(|e| {
                    VolaError::error_here(e, func.head_span(), "could not define argument")
                })?;
            }
            //set the active's scope writeout port
            assert!(self.graph[existing.lambda].result_types(0).len() == 1);
            //also setup the span we are working in
            ctx.block_span = existing.region_span.clone();
            existing.lambda
        };

        //At this point we are finished setting up all context info, so we can start
        //building the actual code

        self.build_block(
            RegionLocation {
                node: lambda,
                region_index: 0,
            },
            func.block,
            &mut ctx,
        )?;

        let function_body_scope = ctx.close_scope();
        //post_serialize, make sure there is a result, and hook it up
        if let Some(result_src) = function_body_scope.result {
            self.graph
                .connect(
                    result_src,
                    lambda.as_inport_location(InputType::Result(0)),
                    OptEdge::value_edge_unset(),
                )
                .map_err(|e| {
                    VolaError::error_here(
                        OptError::from(e),
                        return_span,
                        "could not find return type",
                    )
                })?;
        } else {
            return Err(VolaError::error_here(
                OptError::Any {
                    text: "No result value for function".to_owned(),
                },
                return_span,
                "this function should return a value",
            ));
        }

        //if that didn't fail, we now hve actual working code *_*
        Ok(())
    }

    pub(crate) fn add_implblock(
        &mut self,
        span: Span,
        _ct_args: Vec<CTArg>,
        implblock: ImplBlock,
    ) -> Result<(), VolaError<OptError>> {
        //Impl blocks function similar to normal functions, but
        //we additionally import all operands at the first n-cvs
        //
        //They are also not _simply_ callable, which is why we do λ-creation and
        //code emission in one go.
        let return_span = implblock
            .block
            .retexpr
            .as_ref()
            .map(|ret| ret.span.clone())
            .unwrap_or(implblock.head_span());
        //The concept that is implemented
        let concept = implblock.concept.0.clone();
        //the CSG node it is implemented for
        let csg = implblock.dst.0.clone();
        let implkey = ImplKey {
            concept,
            csgdef: csg,
        };

        if let Some(existing) = self.concept_impl.get(&implkey) {
            let err = OptError::Any {
                text: format!(
                    "implementation for {}::{} already exists!",
                    implkey.csgdef, implkey.concept
                ),
            };
            return Err(
                VolaError::error_here(err, implblock.head_span(), "redefined here")
                    .with_label(existing.def_span.clone(), "first defined here"),
            );
        }

        //try to get the concept, so we can inver the arg-type and the return type
        let (arg_ty, return_ty) = if let Some(concept) = self.concepts.get(&implkey.concept) {
            (concept.src_ty.clone(), concept.dst_ty.clone())
        } else {
            let err = OptError::Any {
                text: format!("Concept \"{}\" was undefined!", implkey.concept),
            };
            return Err(VolaError::error_here(err, span, "For this implementation"));
        };

        let csgdef = if let Some(csgdef) = self.csg_node_defs.get(&implblock.dst.0) {
            csgdef
        } else {
            let err = OptError::Any {
                text: format!("CSG \"{}\" was undefined!", implkey.csgdef),
            };
            return Err(VolaError::error_here(err, span, "For this implementation"));
        };

        //setup the omega context, then directly open the scope for our function
        let mut initial_context = self.omega_context()?;
        //NOTE: Use the toplevel region, but overwrite, once λ-creation is done
        initial_context.open_new_scope(self.graph.toplevel_region(), false);
        let mut args = SmallColl::new();
        //setup the lambda node accordingly, as well as the block context, then launch the block builder.
        let (lambda, result_port) = self.graph.on_omega_node(|omg| {
            let result = omg.new_function(false, |lmd| {
                //setup all operands as CV-Vars, and the arg as ... arg
                for operand in &implblock.operands {
                    let (outside, within) = lmd.add_context_variable();
                    if let Err(e) = initial_context.define_var(operand.0.clone(), within) {
                        let span = implblock.head_span();
                        return Err(VolaError::error_here(e, span, "here"));
                    }
                    //Directly set to CSG type
                    self.typemap.set(within.into(), Ty::CSG);
                    self.typemap.set(outside.into(), Ty::CSG);
                }

                //setup all implicit variables
                for arg in &csgdef.args {
                    let port = lmd.add_argument();
                    if let Err(e) = initial_context.define_var(arg.ident.0.clone(), port) {
                        let span = implblock.head_span();
                        return Err(VolaError::error_here(e, span, "here"));
                    }
                    //setup type
                    self.typemap.set(port.into(), arg.ty.clone().into());
                    //and push into args collection
                    args.push((arg.ident.0.clone(), arg.ty.clone().into()));
                }

                let port = lmd.add_argument();
                initial_context
                    .define_var(implblock.concept_arg_name.0.clone(), port)
                    .unwrap();

                //Also tag with the correct type already
                self.typemap.set(port.into(), arg_ty.clone().into());
                //Push the concept's arg as the last in the row
                args.push((implblock.concept_arg_name.0.clone(), arg_ty.into()));

                //setup result
                let result_port = lmd.add_result();
                self.typemap
                    .set(result_port.into(), return_ty.clone().into());
                Ok(result_port)
            });
            result
        });
        //unwrap the result port... result. Since an error might happen while setting up the λ
        let result_port = result_port?;
        //Correct the region-location as metioned above
        initial_context.active_scope.region = RegionLocation {
            node: lambda,
            region_index: 0,
        };

        let def_span = implblock.head_span();

        self.build_block(
            RegionLocation {
                node: lambda,
                region_index: 0,
            },
            implblock.block,
            &mut initial_context,
        )?;

        let function_body_scope = initial_context.close_scope();
        //post_serialize, make sure there is a result, and hook it up
        if let Some(result_src) = function_body_scope.result {
            self.graph
                .connect(result_src, result_port, OptEdge::value_edge_unset())
                .map_err(|e| {
                    VolaError::error_here(
                        OptError::from(e),
                        return_span,
                        "could not find return type",
                    )
                })?;
        } else {
            return Err(VolaError::error_here(
                OptError::Any {
                    text: "No result value for impl-block".to_owned(),
                },
                return_span,
                "this block should return a value",
            ));
        }

        //At this point everything should be hooked-up and typed. therfore we can return

        self.names.set(
            lambda.into(),
            format!("impl {} for {}", implkey.csgdef, implkey.concept),
        );

        let interned = Impl {
            region_span: span,
            def_span,
            concept: implblock.concept.0.clone(),
            lambda,
            subtrees: implblock.operands.into_iter().map(|a| a.0).collect(),
            args,
            return_type: return_ty.into(),
        };
        self.concept_impl.insert(implkey, interned);

        Ok(())
    }
}
