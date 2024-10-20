/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Type derivation pass.
//!
//! The pass is, who would have guessed, some form of λ-calculus.
//!
//! In practice we try to find a way from all _known_ types to a fully typed λ-node (not to be confused with the λ-calculus, yes naming is hard).
//! We do that by calling the [DialectNode](crate::DialectNode)'s `try_derive_type()` function.
//!
//! This lets each node-type implement its own derivation rule.
//!
//! The pass itself just tries to find a fix-point style resolution, which basically comes down to calling try-derive on all _just changed nodes_ till
//! either all are resolved, or nothing changes and we end in an _unresolved_ state.

use ahash::{AHashMap, AHashSet};
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::{ApplyNode, NodeType},
    region::RegionLocation,
    smallvec::SmallVec,
    util::abstract_node_type::AbstractNodeType,
    NodeRef,
};
use rvsdg_viewer::View;
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{
    alge::{algefn::AlgeFn, implblock::ConceptImpl},
    common::Ty,
    csg::{exportfn::ExportFn, fielddef::FieldDef},
    error::OptError,
    OptEdge, Optimizer, TypeState,
};

//NOTE: At the moment we rely on `eval` expressions being already tagged, as well as all inputs to an λ-Node being tagged as well.
// This basically lets us "push-down" all definitions. The only somewhat _hard_ nodes are the eval-nodes, since those will be replaced
// by call-sites at some point. However, since we knew the `concept` being used at that call site, we at-least know the return type, so we call walk
// over those.
//
impl Optimizer {
    ///Runs the type resolution pass on all nodes.
    pub fn type_derive(&mut self) -> Result<(), OptError> {
        //NOTE, we only need to run that on λ-Regions. There are 3 kinds for that
        // 1. ImplBlocks
        // 2. FieldDefs
        // 3. ExportFns
        //
        // Each of those has slightly different requirements.
        //
        // - Impl block _just_ uses the standard λ-resolution algorithm
        // - FieldDef: Uses the λ-resolution, then checks that the last result-connected node is a CSGTree node.
        // - ExportFn: Uses the λ-resolution, makes sure that all result connected nodes are one of the algebraic types.
        //
        //
        // Similar to the add_ast routine we _try_ all resolves, and collect errors before returning.

        //NOTE: Implblocks never have context dependencies, so we can always resolve them immediately.
        //      This also gives us a nice initial _resolved_set_

        #[cfg(feature = "log")]
        log::info!("type derive");

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_PRE_TYPE_DERIVE").is_ok() {
            self.push_debug_state("pre type derive");
        }

        let mut error_count = 0;
        let mut resolve_set = AHashSet::new();
        let implblocks = self
            .concept_impl
            .values()
            .map(|v| (v.lambda_region, v.span.clone()))
            .collect::<Vec<_>>();
        for (implblock, span) in implblocks {
            if let Err(_err) = self.derive_region(implblock, span.clone()) {
                error_count += 1;
            } else {
                //Add to resolve set
                let was_new = resolve_set.insert(implblock.node).clone();
                assert!(was_new);
            }
        }

        for implblock in self.concept_impl.values() {
            self.verify_imblblock(implblock)?;
        }

        //To guarantee that a region can resolve _context-dependent-nodes_ (mostly apply nodes atm),
        //We build a _context_dependecy_map_ first. This will allow us to check all λ-Node context dependencies
        //So we only try to resolve nodes where the context is already resolved.

        let fregs = self
            .field_def
            .values()
            .map(|fdef| (fdef.lambda_region.clone(), fdef.span.clone()))
            //Append all alge_fn to the dependency resolution as well
            .chain(
                self.alge_fn
                    .values()
                    .map(|algefn| (algefn.lambda_region.clone(), algefn.span.clone())),
            )
            .collect::<Vec<_>>();

        let exports = self
            .export_fn
            .values()
            .map(|exp| (exp.lambda_region.clone(), exp.span.clone()))
            .collect::<Vec<_>>();

        let mut context_dependeny_map = AHashMap::default();
        for def in fregs
            .iter()
            .map(|f| f.0.node)
            .chain(exports.iter().map(|e| e.0.node))
        {
            //For def, read out the context variables, and push all srcs
            let mut i = 0;
            let mut dependecy_set = AHashSet::default();
            while let Some(cvin) = self
                .graph
                .node(def)
                .node_type
                .unwrap_lambda_ref()
                .cv_input(i)
            {
                i += 1;
                if let Some(connection) = cvin.edge {
                    let src_node = self.graph.edge(connection).src().node;
                    dependecy_set.insert(src_node);
                }
            }
            if dependecy_set.len() > 0 {
                context_dependeny_map.insert(def, dependecy_set);
            }
        }

        //Now do the field defs by iterating work list till either nothing changes or empty.
        //we always check if all dependencies are met
        let mut worklist = fregs;
        'fielddef_resolution: loop {
            let mut changed_any = false;
            let mut local_work_list = Vec::new();
            std::mem::swap(&mut worklist, &mut local_work_list);
            for (def, srcspan) in local_work_list {
                //check if def's dependencies are met
                let dependecies_met = {
                    if let Some(dependecies) = context_dependeny_map.get(&def.node) {
                        let mut is_met = true;
                        for dep in dependecies {
                            if !resolve_set.contains(dep) {
                                is_met = false;
                                break;
                            }
                        }
                        is_met
                    } else {
                        //Has no dependencies, therefore we can always resolve
                        true
                    }
                };

                if dependecies_met {
                    //Try to resolve, since all dependencies are met
                    if let Err(_err) = self.derive_region(def, srcspan.clone()) {
                        error_count += 1;
                    } else {
                        //successfully resolved, so add to resolve map
                        changed_any = true;
                        let is_newly_inserted = resolve_set.insert(def.node);
                        assert!(
                            is_newly_inserted,
                            "the field-def {} shouldn't have been resolved yet...",
                            def.node
                        );
                    }
                } else {
                    //dependencies not yet met, push back again
                    worklist.push((def, srcspan));
                }
            }

            if !changed_any && !worklist.is_empty() {
                return Err(OptError::Any {
                    text: format!("Could not type resolve all field definitions!"),
                });
            }

            if worklist.is_empty() {
                break 'fielddef_resolution;
            }
        }

        for fdef in self.field_def.values() {
            self.verify_field_def(fdef)?;
        }

        for algefn in self.alge_fn.values() {
            self.verify_alge_fn(algefn)?;
        }

        #[cfg(feature = "log")]
        log::info!("Finished deriving dependent nodes, moving to exports...");
        //NOTE: Right now we don't use the dependency driven resolution we use for field defs,
        //      cause by definition all dependencies must be resolved that could be imported.
        //      However, since this is pre-alpha ware, we still check
        for (exp, span) in exports {
            if let Some(deps) = context_dependeny_map.get(&exp.node) {
                for dep in deps {
                    assert!(resolve_set.contains(dep));
                }
            }
            if let Err(_err) = self.derive_region(exp, span) {
                error_count += 1;
            }
        }
        for exp in self.export_fn.values() {
            self.verify_export_fn(exp)?;
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_TYPE_DERIVE").is_ok() {
            self.push_debug_state("post type derive");
        }

        #[cfg(feature = "log")]
        log::info!("Finished type derive");
        if error_count > 0 {
            Err(OptError::Any {
                text: format!("Type derivation did not end successfully!"),
            })
        } else {
            Ok(())
        }
    }

    //Verifies that the implblock has the correct input and output signature set on all edges
    fn verify_imblblock(&self, block: &ConceptImpl) -> Result<(), OptError> {
        //NOTE: we currently only check that the return type matches, since the input types are
        // always pre-set by the impblock builder, based on the concept's deceleration. So there would be some
        // kind of type conflict within the region.

        let expected_result_type: Ty = self
            .concepts
            .get(&block.concept.0)
            .unwrap()
            .dst_ty
            .clone()
            .try_into()
            .expect("Could not convert concept type into opttype");

        //Try to find the src node's span. For better error reporting
        let result_node_span = {
            if let Some(srcnode) = self
                .graph
                .region(&block.lambda_region)
                .unwrap()
                .result_src(&self.graph, 0)
            {
                if let NodeType::Simple(s) = &self.graph.node(srcnode.node).node_type {
                    Some(s.span.clone())
                } else {
                    None
                }
            } else {
                None
            }
        };

        //check the output for the correctly typed edge
        if let Some(result_edg) = self.graph.region(&block.lambda_region).unwrap().results[0].edge {
            match self.graph.edge(result_edg).ty.get_type() {
                None => {
                    let err = OptError::Any {
                        text: format!("impl-block's output was untyped"),
                    };
                    report(
                        error_reporter(err.clone(), block.span.clone())
                            .with_label(
                                Label::new(block.span.clone()).with_message("in this region"),
                            )
                            .finish(),
                    );
                    Err(err)
                }
                Some(t) => {
                    if *t != expected_result_type {
                        if let Some(s) = result_node_span {
                            let err = OptError::Any {
                                text: format!(
                                    "ImplBlock's output was of type {:?} but expected {:?}",
                                    t, expected_result_type
                                ),
                            };
                            report(
                                error_reporter(err.clone(), s.clone())
                                    .with_label(
                                        Label::new(s.clone()).with_message("Tried to return this"),
                                    )
                                    .finish(),
                            );
                            Err(err)
                        } else {
                            let err = OptError::Any {
                                text: format!(
                                    "ImplBlock's output was of type {:?} but expected {:?}",
                                    t, expected_result_type
                                ),
                            };
                            report(
                                error_reporter(err.clone(), block.span.clone())
                                    .with_label(
                                        Label::new(block.span.clone())
                                            .with_message("In this region"),
                                    )
                                    .finish(),
                            );
                            Err(err)
                        }
                    } else {
                        Ok(())
                    }
                }
            }
        } else {
            let err = OptError::Any {
                text: format!("ImplBlocks's output was not connected"),
            };
            report(
                error_reporter(err.clone(), block.span.clone())
                    .with_label(Label::new(block.span.clone()).with_message("In this region"))
                    .finish(),
            );
            Err(err)
        }
    }

    fn verify_field_def(&self, fdef: &FieldDef) -> Result<(), OptError> {
        //For the field def its pretty easy. We just have to check that it actually returns a CSGTree

        if let Some(result_edge) = self.graph.region(&fdef.lambda_region).unwrap().results[0].edge {
            if let Some(ty) = self.graph.edge(result_edge).ty.get_type() {
                if ty != &Ty::CSGTree {
                    let err = OptError::Any {
                        text: format!(
                            "field definition's output is expected to be a CSGTree, not {:?}",
                            ty
                        ),
                    };
                    report(
                        error_reporter(err.clone(), fdef.span.clone())
                            .with_label(
                                Label::new(fdef.span.clone()).with_message("In this region"),
                            )
                            .finish(),
                    );
                    Err(err)
                } else {
                    Ok(())
                }
            } else {
                let err = OptError::Any {
                    text: format!("field definition's output was not typed"),
                };
                report(
                    error_reporter(err.clone(), fdef.span.clone())
                        .with_label(Label::new(fdef.span.clone()).with_message("In this region"))
                        .finish(),
                );
                Err(err)
            }
        } else {
            let err = OptError::Any {
                text: format!("field definition's output was not connected"),
            };
            report(
                error_reporter(err.clone(), fdef.span.clone())
                    .with_label(Label::new(fdef.span.clone()).with_message("In this region"))
                    .finish(),
            );
            Err(err)
        }
    }

    fn verify_export_fn(&self, export: &ExportFn) -> Result<(), OptError> {
        //In this case, for every output, make sure that the type is correct as well.
        // NOTE that the expected result type is also derived from the accessed concept.
        // So this makes mostly sure that we in fact derived the correct output type.
        for (i, expected_ty) in export.output_signature.iter().enumerate() {
            if let Some(result_edge) = self
                .graph
                .region(&export.lambda_region)
                .unwrap()
                .results
                .get(i)
                .map(|res| res.edge)
                .flatten()
            {
                if let Some(ty) = self.graph.edge(result_edge).ty.get_type() {
                    if ty != expected_ty {
                        let err = OptError::Any {
                            text: format!(
                                "field definition's output {} is expected to be a {:?}, not {:?}",
                                i, expected_ty, ty
                            ),
                        };
                        report(
                            error_reporter(err.clone(), export.span.clone())
                                .with_label(
                                    Label::new(export.span.clone()).with_message("In this region"),
                                )
                                .finish(),
                        );
                        return Err(err);
                    }
                } else {
                    let err = OptError::Any {
                        text: format!("field-export's output[{}] was not typed", i),
                    };
                    report(
                        error_reporter(err.clone(), export.span.clone())
                            .with_label(
                                Label::new(export.span.clone()).with_message("In this region"),
                            )
                            .finish(),
                    );
                    return Err(err);
                }
            } else {
                let err = OptError::Any {
                    text: format!("field-export's output[{}] was not connected", i),
                };
                report(
                    error_reporter(err.clone(), export.span.clone())
                        .with_label(Label::new(export.span.clone()).with_message("In this region"))
                        .finish(),
                );
                return Err(err);
            }
        }

        Ok(())
    }

    fn verify_alge_fn(&self, algefn: &AlgeFn) -> Result<(), OptError> {
        //Make sure that the result-connected edge is of the right type,
        //and the argument-connected edges are correct as well
        for (argidx, (_name, argty)) in algefn.args.iter().enumerate() {
            for arg_connected_edge in &self
                .graph
                .node(algefn.lambda)
                .node_type
                .unwrap_lambda_ref()
                .argument(argidx)
                .unwrap()
                .edges
            {
                if let Some(ty) = self.graph.edge(*arg_connected_edge).ty.get_type() {
                    //if the types do not match, bail as well
                    if ty != argty {
                        let err = OptError::Any {
                            text: format!("Argument {argidx} is of typ \"{argty:?}\", but was used as \"{ty:?}\". This is a compiler bug!"),
                        };
                        report(
                            error_reporter(err.clone(), algefn.span.clone())
                                .with_label(Label::new(algefn.span.clone()).with_message(&format!(
                                    "user of argument {argidx} in this region"
                                )))
                                .finish(),
                        );
                        return Err(err);
                    }
                } else {
                    let err = OptError::Any {
                        text: format!("Argument {argidx} is of type \"{argty:?}\", but connected edge was untyped. This is a compiler bug!"),
                    };
                    report(
                        error_reporter(err.clone(), algefn.span.clone())
                            .with_label(
                                Label::new(algefn.span.clone()).with_message(&format!(
                                    "user of argument {argidx} in this region"
                                )),
                            )
                            .finish(),
                    );
                    return Err(err);
                }
            }
        }

        //Check that the result is set correctly
        if let Some(edg) = &self
            .graph
            .node(algefn.lambda)
            .node_type
            .unwrap_lambda_ref()
            .result(0)
            .unwrap()
            .edge
        {
            if let Some(ty) = self.graph.edge(*edg).ty.get_type() {
                if ty != &algefn.retty {
                    let err = OptError::Any {
                        text: format!(
                            "Result type was \"{ty:?}\", but expected {:?}!",
                            algefn.retty
                        ),
                    };
                    report(
                        error_reporter(err.clone(), algefn.span.clone())
                            .with_label(Label::new(algefn.span.clone()).with_message(&format!(
                                "result producer in this region should be {:?}",
                                algefn.retty
                            )))
                            .finish(),
                    );
                    return Err(err);
                }
            } else {
                let err = OptError::Any {
                    text: format!("Result is of type \"{:?}\", but connected edge was untyped. This is a compiler bug!", algefn.retty),
                };
                report(
                    error_reporter(err.clone(), algefn.span.clone())
                        .with_label(
                            Label::new(algefn.span.clone())
                                .with_message("result producer in this region"),
                        )
                        .finish(),
                );
                return Err(err);
            }
        } else {
            let err = OptError::Any {
                text: format!("Result is of type \"{:?}\", but there was no return value set. This is a compiler bug!", algefn.retty),
            };
            report(
                error_reporter(err.clone(), algefn.span.clone())
                    .with_label(
                        Label::new(algefn.span.clone())
                            .with_message("no result producer in this region"),
                    )
                    .finish(),
            );
            return Err(err);
        }

        Ok(())
    }

    fn try_apply_derive(
        &self,
        node: NodeRef,
        apply: &ApplyNode,
        region_src_span: &Span,
    ) -> Result<Option<Ty>, OptError> {
        #[cfg(feature = "log")]
        log::info!("Type derive Apply node");

        //for the apply node, we search for the src λ-Node, and call
        if let Some(calldecl_edge) = apply.get_callabel_decl().edge {
            let callable_src = self.graph.edge(calldecl_edge).src().clone();
            if let Some(calldef) = self.graph.find_callabel_def(callable_src) {
                //Now, to derive the output we first iterate the callable's arguments, and checkout the expected
                //types. After that we match them to the apply_nodes actual connected types. If they match we pass
                //the callables result argument

                let span = self
                    .span_tags
                    .get(&calldef.node.into())
                    .cloned()
                    .unwrap_or(Span::empty());
                let result_type = {
                    if let Some(ty) = self.find_type(
                        &InportLocation {
                            node: calldef.node,
                            input: InputType::Result(0),
                        }
                        .into(),
                    ) {
                        ty
                    } else {
                        let err = OptError::Any {
                            text: "This call's src function output type was not set!".to_owned(),
                        };
                        report(
                            error_reporter(err.clone(), span.clone())
                                .with_label(Label::new(span.clone()).with_message("consider checking this function for errors (and file an issue on GitLab, this is likely a compiler bug)"))
                                .finish(),
                        );
                        return Err(err);
                    }
                };

                let callable_reg = RegionLocation {
                    node: calldef.node,
                    region_index: 0,
                };
                let call_sig = self.get_lambda_arg_signature(callable_reg.node);
                let mut expected_call_sig: SmallVec<[Ty; 3]> = SmallVec::default();
                for (argidx, maybe_ty) in call_sig.into_iter().enumerate() {
                    if let Some(t) = maybe_ty {
                        expected_call_sig.push(t);
                    } else {
                        let err = OptError::Any {
                            text: format!(
                                "Argument-Type {argidx} was not set. This indicates a bug!"
                            ),
                        };

                        report(
                            error_reporter(err.clone(), span.clone())
                                .with_label(Label::new(span.clone()).with_message("Of this λ-Node"))
                                .finish(),
                        );
                        return Err(err);
                    }
                }

                //Now match the call sig to the apply_node sig
                let apply_node_sig = {
                    let mut apply_sig: SmallVec<[Ty; 3]> = SmallVec::default();
                    let argcount = apply.get_call_arg_count();
                    for i in 0..argcount {
                        if let Some(edg) = apply.argument_input(i).unwrap().edge {
                            let ty = if let Some(ty) = self.graph.edge(edg).ty.get_type() {
                                ty
                            } else {
                                //type of argument not yet derived
                                return Ok(None);
                            };
                            apply_sig.push(ty.clone());
                        } else {
                            let err = OptError::Any {
                                text: format!("apply node's {i}-th input has no connected edge."),
                            };
                            report(
                                error_reporter(err.clone(), region_src_span.clone())
                                    .with_label(
                                        Label::new(region_src_span.clone())
                                            .with_message("In this region"),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    }

                    apply_sig
                };

                assert!(apply_node_sig.len() == expected_call_sig.len());
                for i in 0..apply_node_sig.len() {
                    if apply_node_sig[i] != expected_call_sig[i] {
                        let err = OptError::Any {
                            text: format!(
                                "call's {i}-th input: expected type {:?} but got {:?}",
                                expected_call_sig[i], apply_node_sig[i]
                            ),
                        };
                        let call_span = self.find_span(node.into()).unwrap_or(Span::empty());
                        report(
                            error_reporter(err.clone(), span.clone())
                                .with_label(Label::new(call_span).with_message("For this call"))
                                .with_label(
                                    Label::new(span.clone()).with_message("of this function"),
                                )
                                .finish(),
                        );
                        return Err(err);
                    }
                }
                //If we finished till here, then its all-right
                Ok(Some(result_type))
            } else {
                let err = OptError::Any {
                    text: format!("apply node's call-edge hat no callable producer."),
                };
                report(
                    error_reporter(err.clone(), region_src_span.clone())
                        .with_label(
                            Label::new(region_src_span.clone()).with_message("In this region"),
                        )
                        .finish(),
                );
                Err(err)
            }
        } else {
            let err = OptError::Any {
                text: format!("apply node's calledge was not connected"),
            };
            report(
                error_reporter(err.clone(), region_src_span.clone())
                    .with_label(Label::new(region_src_span.clone()).with_message("In this region"))
                    .finish(),
            );
            Err(err)
        }
    }

    fn try_gamma_derive(&mut self, node: NodeRef) -> Result<Option<Ty>, OptError> {
        //We just need to check two things:
        // 1. that the conditional is a Boolean output,
        // 2. That all branches emit the same type

        let conditional_type = {
            if let Some(condty) = self.get_type_for_inport(InportLocation {
                node,
                input: InputType::GammaPredicate,
            }) {
                condty
            } else {
                //Was not yet resolved, return.
                return Ok(None);
            }
        };

        if !conditional_type.is_bool() {
            let span = {
                let condition_src = self.graph.node(node).input_src(&self.graph, 0).unwrap();
                self.span_tags
                    .get(&condition_src.into())
                    .cloned()
                    .unwrap_or(Span::empty())
            };

            let err = OptError::Any {
                text: format!(
                    "Condition must be an expression of type Bool, was {conditional_type}"
                ),
            };
            report(
                error_reporter(err.clone(), span.clone())
                    .with_label(
                        Label::new(span.clone()).with_message("Consider changing this expression!"),
                    )
                    .finish(),
            );
            return Err(err);
        }

        //find all input types. If we have them, propagate them _into_
        //the regions.
        //Then recurse the region derive.
        //Note that we only need the type on any
        //EV that is acutally used by any of the branches.

        let subregion_count = self.graph.node(node).regions().len();
        assert!(subregion_count > 0);
        for input_type in self.graph.node(node).inport_types() {
            if let Some(ty) = self
                .find_type(
                    &InportLocation {
                        node,
                        input: input_type,
                    }
                    .into(),
                )
                .clone()
            {
                for ridx in 0..subregion_count {
                    if let Some(mapped_internally) = input_type.map_to_in_region(ridx) {
                        self.typemap.set(
                            OutportLocation {
                                node,
                                output: mapped_internally,
                            }
                            .into(),
                            ty.clone(),
                        );
                    }
                }
            } else {
                //NOTE: We check if the port is in use. If not, its okey if no type information was
                //      found
                let is_in_use = {
                    let mut is_in_use = false;
                    for subreg in 0..subregion_count {
                        if let Some(p) = self
                            .graph
                            .node(node)
                            .outport(&input_type.map_to_in_region(subreg).unwrap())
                        {
                            if p.edges.len() > 0 {
                                is_in_use = true;
                                break;
                            }
                        }
                    }
                    is_in_use
                };

                if is_in_use {
                    //Port is in use, but input is untype, therfore try again later.
                    return Ok(None);
                }
            }
        }

        //if we reached this, recurse!
        for ridx in 0..subregion_count {
            let regloc = RegionLocation {
                node,
                region_index: ridx,
            };
            let span = self
                .span_tags
                .get(&regloc.into())
                .cloned()
                .unwrap_or(Span::empty());
            self.derive_region(regloc, span)?;
        }

        //make sure that both regions return the same type.
        //now find the output type of both regions, and make sure they are the same. If so, we are good to go
        let reg_if_ty = if let Some(t) = self.find_type(
            &InportLocation {
                node,
                input: InputType::ExitVariableResult {
                    branch: 0,
                    exit_variable: 0,
                },
            }
            .into(),
        ) {
            t
        } else {
            panic!("Gamma-internal if-branch region should have been resolved!");
        };
        let reg_else_ty = if let Some(t) = self.find_type(
            &InportLocation {
                node,
                input: InputType::ExitVariableResult {
                    branch: 1,
                    exit_variable: 0,
                },
            }
            .into(),
        ) {
            t
        } else {
            panic!("Gamma-internal else-branch should have been resolved!");
        };

        if reg_if_ty != reg_else_ty {
            let if_branch_span = self
                .span_tags
                .get(
                    &RegionLocation {
                        node,
                        region_index: 0,
                    }
                    .into(),
                )
                .cloned()
                .unwrap_or(Span::empty());
            let else_branch_span = self
                .span_tags
                .get(
                    &RegionLocation {
                        node,
                        region_index: 1,
                    }
                    .into(),
                )
                .cloned()
                .unwrap_or(Span::empty());

            let err = OptError::Any {
                text: format!(
                    "Return type conflict. If branch returns {}, but else branch returns {}",
                    reg_if_ty, reg_else_ty
                ),
            };
            report(
                error_reporter(err.clone(), if_branch_span.clone())
                    .with_label(
                        Label::new(if_branch_span.clone())
                            .with_message(format!("This returns {reg_if_ty}")),
                    )
                    .with_label(
                        Label::new(else_branch_span.clone())
                            .with_message(format!("This returns {reg_else_ty}")),
                    )
                    .finish(),
            );

            Err(err)
        } else {
            Ok(Some(reg_if_ty))
        }
    }

    ///Does theta type derive, assuming that all used inputs are type set.
    fn theta_type_derive(&mut self, theta: NodeRef) -> Result<(), OptError> {
        let theta_span = self.find_span(theta.into()).unwrap_or(Span::empty());
        //at the start, check input types.
        //we must be sure that the loop bounds (input 0 & 1) are natural numbers.
        //
        //for all other inports we map the input type to the argument type in the loop-region.
        //Inputs that have no edge connected to the input and the equivalent argument are ignored
        for inport in self.graph[theta].inport_types() {
            let inportloc = theta.as_inport_location(inport);
            let in_region_arg = theta.as_outport_location(inport.map_to_in_region(0).unwrap());
            let arg_span = if let Some(span) = self.find_span(inportloc.into()) {
                span
            } else {
                theta_span.clone()
            };
            let input_ty = match inport {
                //Check for natural
                InputType::Input(0) | InputType::Input(1) => {
                    if let Some(ty) = self.find_type(&inportloc.into()) {
                        if !ty.is_nat() {
                            let err = OptError::TypeDeriveError {
                                text: format!("Loop bound should be {}, was {}", Ty::Nat, ty),
                            };
                            report(
                                error_reporter(err.clone(), arg_span.clone())
                                    .with_label(Label::new(arg_span).with_message("Here"))
                                    .finish(),
                            );
                            return Err(err);
                        } else {
                            ty
                        }
                    } else {
                        //Did not find a type for the loop-bound, which is also undefined
                        let err = OptError::TypeDeriveError {
                            text: format!("Loop bound should be {}, but had no type!", Ty::Nat),
                        };
                        report(
                            error_reporter(err.clone(), arg_span.clone())
                                .with_label(Label::new(arg_span).with_message("Here"))
                                .finish(),
                        );
                        return Err(err);
                    }
                }
                InputType::Input(other) => {
                    //if both, the arg, and the in-loop arg are unconnected, ignore, otherwise,
                    //expect a type, and map that into the region
                    //Yay can ignore
                    if self.graph[inportloc].edge.is_none()
                        && self.graph[in_region_arg].edges.len() == 0
                    {
                        continue;
                    }
                    if let Some(ty) = self.find_type(&inportloc.into()) {
                        ty
                    } else {
                        //unexpected
                        let err = OptError::TypeDeriveError {
                            text: format!("Loop argument[{other}] should be typed but wasn't!"),
                        };
                        report(
                            error_reporter(err.clone(), arg_span.clone())
                                .with_label(Label::new(arg_span).with_message("Here"))
                                .finish(),
                        );
                        return Err(err);
                    }
                }
                other => {
                    //unexpected input type. However panic, since the loop would be malformed anywasy in that case
                    panic!("Malformed Theta node, can only have Inputs, had {other:?}");
                }
            };

            //map the type into the region
            self.typemap.set(in_region_arg.into(), input_ty);
        }

        //aight, derive the region
        self.derive_region(
            RegionLocation {
                node: theta,
                region_index: 0,
            },
            theta_span.clone(),
        )?;
        //make sure the theta-predicate is a bool now
        if let Some(ty) =
            self.find_type(&theta.as_inport_location(InputType::ThetaPredicate).into())
        {
            if !ty.is_bool() {
                let err = OptError::TypeDeriveError {
                    text: format!("Loop predicate should be {:?}, but is {:?}!", Ty::Bool, ty),
                };
                report(
                    error_reporter(err.clone(), theta_span.clone())
                        .with_label(Label::new(theta_span).with_message("Here"))
                        .finish(),
                );
                return Err(err);
            }
        } else {
            let err = OptError::TypeDeriveError {
                text: format!("Loop predicate should be {:?}, but had no type!", Ty::Bool,),
            };
            report(
                error_reporter(err.clone(), theta_span.clone())
                    .with_label(Label::new(theta_span.clone()).with_message("Here"))
                    .finish(),
            );
            return Err(err);
        }

        //now map all results out of the theta
        for output in self.graph[theta].outport_types() {
            let result_port = theta.as_outport_location(output);
            let in_region_port = theta.as_inport_location(output.map_to_in_region(0).unwrap());
            let result_ty = if let Some(ty) = self.find_type(&in_region_port.into()) {
                ty
            } else {
                //has no type. Make sure there is no producer, and user
                if self.graph[in_region_port].edge.is_none()
                    && self.graph[result_port].edges.len() == 0
                {
                    continue;
                } else {
                    let err = OptError::TypeDeriveError {
                        text: format!("Loop output should be typed, but had no type!"),
                    };
                    report(
                        error_reporter(err.clone(), theta_span.clone())
                            .with_label(Label::new(theta_span).with_message("Here"))
                            .finish(),
                    );
                    return Err(err);
                }
            };
            //result has a type. Find the equivalent argument port, and also make sure that they match.
            let argument_port = if let OutputType::Output(t) = output {
                OutputType::Argument(t)
            } else {
                panic!("Theta node outupt-argument missmatch");
            };
            let argloc = theta.as_outport_location(argument_port);
            let argument_ty = self.find_type(&argloc.into());
            let set_ty = match (result_ty, argument_ty) {
                (ty, None) => {
                    //result is set, argument is not, thats all right
                    ty
                }
                (ty, Some(argty)) => {
                    if ty != argty {
                        //type missmatch
                        let err = OptError::TypeDeriveError {
                            text: format!("Loop-Variable[{argument_port:?}] type missmatch. Argument was {argty}, but Result was {ty}!"),
                        };
                        report(
                            error_reporter(err.clone(), theta_span.clone())
                                .with_label(Label::new(theta_span).with_message("Here"))
                                .finish(),
                        );
                        return Err(err);
                    } else {
                        ty
                    }
                }
            };
            //overwrite both, just to be sure
            self.typemap.set(argloc.into(), set_ty.clone());
            self.typemap.set(result_port.into(), set_ty);
        }

        Ok(())
    }

    pub fn try_node_type_derive(
        &mut self,
        node: NodeRef,
    ) -> Result<(Option<Ty>, OutportLocation), OptError> {
        //gather all inputs and let the node try to resolve itself
        let payload = match &self.graph.node(node).node_type {
            NodeType::Simple(s) => {
                let ty = s.node.try_derive_type(
                    &self.typemap,
                    &self.graph,
                    &self.concepts,
                    &self.csg_node_defs,
                );

                (ty?, node.output(0))
            }
            NodeType::Apply(a) => {
                let region_span = self
                    .find_span(self.graph[node].parent.unwrap().into())
                    .unwrap_or(Span::empty());
                let ty = self.try_apply_derive(node, a, &region_span);
                (ty?, node.output(0))
            }
            NodeType::Gamma(_g) => {
                let ty = self.try_gamma_derive(node);
                (ty?, node.output(0))
            }
            //NOTE: by convention the θ-Node resolves to output 2
            NodeType::Theta(_t) => {
                self.theta_type_derive(node)?;
                let output_port = self.value_producer_port(node).unwrap();
                let ty = self.find_type(&output_port.into());
                (ty, output_port)
            }
            NodeType::Lambda(_l) => {
                self.derive_lambda(node)?;
                //lambdas alwas return a single callable
                (
                    Some(Ty::Callable),
                    node.as_outport_location(OutputType::LambdaDeclaration),
                )
            }
            t => {
                let err = OptError::Any {
                    text: format!("Unexpected node type {t} in graph while doing type resolution."),
                };
                return Err(err);
            }
        };

        Ok(payload)
    }

    fn derive_lambda(&mut self, lambda: NodeRef) -> Result<(), OptError> {
        //The only thing we need to do is check that all CVs are set (and map them into the region),
        //and that all arguments have a type.
        assert_eq!(self.graph[lambda].into_abstract(), AbstractNodeType::Lambda);
        let span = self.find_span(lambda.into()).unwrap_or(Span::empty());

        for argty in self.graph[lambda].argument_types(0) {
            match argty {
                OutputType::Argument(a) => {
                    let loc = lambda.as_outport_location(argty);
                    //NOTE: if the argument is anused, just ignore it
                    if self.graph[loc].edges.len() == 0 {
                        continue;
                    }
                    //for arguments, just check that the type is already set
                    if self.find_type(&loc.into()).is_none() {
                        let argspan = if let Some(argspan) = self.find_span(loc.into()) {
                            argspan
                        } else {
                            span.clone()
                        };

                        let err = OptError::TypeDeriveError {
                            text: format!(
                                "Expected function's argument[{a}] to be type set, but wasn't"
                            ),
                        };
                        report(
                            error_reporter(err.clone(), argspan.clone())
                                .with_label(Label::new(argspan.clone()).with_message("Here"))
                                .finish(),
                        );
                        return Err(err);
                    }
                }
                OutputType::ContextVariableArgument(idx) => {
                    //for cv inputs, try to find a type.
                    //we ignore that tho, if the cv is not connected
                    let loc = lambda.as_outport_location(argty);
                    if self.graph[loc].edges.len() == 0 {
                        continue;
                    }

                    //is connected, try to copy over type
                    let input_port = lambda.as_inport_location(argty.map_out_of_region().unwrap());
                    if let Some(ty) = self.find_type(&input_port.into()) {
                        self.typemap.set(loc.into(), ty);
                    } else {
                        //failed to find inputs's type

                        let argspan = if let Some(argspan) = self.find_span(loc.into()) {
                            argspan
                        } else {
                            span.clone()
                        };

                        let err = OptError::TypeDeriveError {
                            text: format!(
                                "Function's context-variable[{idx}] was not yet type set!"
                            ),
                        };
                        report(
                            error_reporter(err.clone(), argspan.clone())
                                .with_label(Label::new(argspan.clone()).with_message("Here"))
                                .finish(),
                        );
                        return Err(err);
                    }
                }
                _ => {
                    let err = OptError::TypeDeriveError {
                        text: format!("Function had unexpected argument type {argty:?}"),
                    };
                    report(
                        error_reporter(err.clone(), span.clone())
                            .with_label(Label::new(span.clone()).with_message("Here"))
                            .finish(),
                    );
                    return Err(err);
                }
            }
        }

        //now do internal type derivative
        self.derive_region(
            RegionLocation {
                node: lambda,
                region_index: 0,
            },
            span,
        )?;

        Ok(())
    }

    ///Derives the type for all live nodes in the region.
    pub(crate) fn derive_region(
        &mut self,
        reg: RegionLocation,
        region_src_span: Span,
    ) -> Result<(), OptError> {
        //The idea is pretty simple: we build the topological order of all nodes, and
        //derive the type for each.
        //
        //The assumption is, that the type of any node can be calculated from the types of all inputs.
        //
        //A special case are nodes with sub regions. In that case we copy over all input-types to the
        //arguments of each region, and recurse into the region.
        //
        //In practice this makes the algorithm have the recursion depth of the graph's maximum region-nesting count... which is okay.

        //Preset all edges where we know the type already. For instance if the type map
        //contains type info for any of the ports of an edge
        for edg in &self.graph[reg].edges.clone() {
            let src = self.graph[*edg].src().clone();
            if let Some(ty) = self.find_type(&src.into()) {
                if let Some(preset) = self.graph.edge(*edg).ty.get_type() {
                    if preset != &ty {
                        let err = OptError::Any {
                            text: format!("Edge was already set to {:?}, but was about to be overwritten with an incompatible type {:?}", preset, ty),
                        };
                        report(
                            error_reporter(err.clone(), region_src_span.clone())
                                .with_label(
                                    Label::new(region_src_span.clone())
                                        .with_message("In this region"),
                                )
                                .finish(),
                        );
                        return Err(err);
                    }
                } else {
                    self.graph.edge_mut(*edg).ty = OptEdge::Value {
                        ty: TypeState::Set(ty.clone()),
                    };
                }
            }
        }

        let topoord = self.graph.topological_order_region(reg);

        for node in topoord {
            let (type_resolve_try, resolved_port) = match self.try_node_type_derive(node) {
                Ok(t) => t,
                Err(e) => {
                    let span = self.find_span(node.into()).unwrap_or(Span::empty());
                    report(
                        error_reporter(e.clone(), span.clone())
                            .with_label(Label::new(span.clone()).with_message("on this operation"))
                            .finish(),
                    );
                    //Immediately abort, since we have no way of finishing.
                    // TODO: We could also collect all error at this point...
                    return Err(e);
                }
            };

            match type_resolve_try {
                Some(ty) => {
                    //now assign that type to the nodes's output.
                    self.typemap.set(resolved_port.into(), ty.clone());

                    //And propagate to all edges
                    for edg in self.graph[resolved_port].edges.clone().iter() {
                        if let Err(e) = self.graph[*edg].ty.set_derived_state(ty.clone()) {
                            if let Some(span) = self.find_span(resolved_port.node.into()) {
                                report(
                                    error_reporter(e.clone(), span.clone())
                                        .with_label(
                                            Label::new(span.clone()).with_message("On this node"),
                                        )
                                        .finish(),
                                );
                            } else {
                                report(error_reporter(e.clone(), Span::empty()).finish());
                            }

                            return Err(e);
                        }
                    }
                }
                None => {
                    //This was a soft error before, but now we assume that it is a hard erro
                    return Err(OptError::TypeDeriveError {
                        text: format!(
                            "Could not derive type for node: {} ({})",
                            self.graph[node].name(),
                            node
                        ),
                    });
                }
            }
        }

        Ok(())
    }
}
