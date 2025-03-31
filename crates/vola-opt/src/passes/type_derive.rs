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

use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::{ApplyNode, NodeType},
    region::RegionLocation,
    smallvec::{smallvec, SmallVec},
    util::abstract_node_type::AbstractNodeType,
    NodeRef, SmallColl,
};
use vola_common::{ariadne::Label, error_reporter, report, Span};

use crate::{
    common::{DataType, Shape, Ty},
    error::OptError,
    graph::auxiliary::{Function, Impl},
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
        #[cfg(feature = "log")]
        log::info!("type derive");

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_PRE_TYPE_DERIVE").is_ok() {
            self.push_debug_state("pre type derive");
        }

        let mut errors = SmallColl::new();

        //we first build the topological order of all nodes in Omega, then
        //we enter eac
        let toplevel = self.graph.toplevel_region();
        let topoord = self.graph.topological_order_region(toplevel);

        for node in topoord {
            if let Err(e) = self.try_node_type_derive(node) {
                errors.push(e)
            }
        }

        for implblock in self.concept_impl.values() {
            if let Err(e) = self.verify_imblblock(implblock) {
                errors.push(e)
            }
        }

        for f in self.functions.values() {
            if let Err(e) = self.verify_fn(f) {
                errors.push(e)
            }
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_POST_TYPE_DERIVE").is_ok()
        {
            self.push_debug_state("post type derive");
        }

        if errors.len() > 0 {
            return Err(OptError::ErrorsOccurred(errors.len()));
        } else {
            Ok(())
        }
    }

    //Verifies that the implblock has the correct input and output signature set on all edges
    fn verify_imblblock(&self, block: &Impl) -> Result<(), OptError> {
        //NOTE: we currently only check that the return type matches, since the input types are
        // always pre-set by the impblock builder, based on the concept's deceleration. So there would be some
        // kind of type conflict within the region.

        let expected_result_type: Ty = self
            .concepts
            .get(&block.concept)
            .unwrap()
            .dst_ty
            .clone()
            .try_into()
            .expect("Could not convert concept type into opttype");

        //Try to find the src node's span. For better error reporting
        let result_node_span = {
            if let Some(srcnode) = self
                .graph
                .region(&block.region())
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
        if let Some(result_edg) = self.graph.region(&block.region()).unwrap().results[0].edge {
            match self.graph.edge(result_edg).ty.get_type() {
                None => {
                    let err = OptError::Any {
                        text: format!("impl-block's output was untyped"),
                    };
                    report(
                        error_reporter(err.clone(), block.def_span.clone())
                            .with_label(
                                Label::new(block.region_span.clone())
                                    .with_message("in this region"),
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
                                error_reporter(err.clone(), block.def_span.clone())
                                    .with_label(
                                        Label::new(block.region_span.clone())
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
                error_reporter(err.clone(), block.def_span.clone())
                    .with_label(
                        Label::new(block.region_span.clone()).with_message("In this region"),
                    )
                    .finish(),
            );
            Err(err)
        }
    }

    fn verify_fn(&self, algefn: &Function) -> Result<(), OptError> {
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
                            error_reporter(err.clone(), algefn.def_span.clone())
                                .with_label(Label::new(algefn.region_span.clone()).with_message(
                                    &format!("user of argument {argidx} in this region"),
                                ))
                                .finish(),
                        );
                        return Err(err);
                    }
                } else {
                    let err = OptError::Any {
                        text: format!("Argument {argidx} is of type \"{argty:?}\", but connected edge was untyped. This is a compiler bug!"),
                    };
                    report(
                        error_reporter(err.clone(), algefn.def_span.clone())
                            .with_label(
                                Label::new(algefn.region_span.clone()).with_message(&format!(
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
                if ty != &algefn.return_type {
                    let err = OptError::TypeDeriveError {
                        text: format!(
                            "Result type was \"{ty}\", but expected {}!",
                            algefn.return_type
                        ),
                    };
                    report(
                        error_reporter(err.clone(), algefn.def_span.clone())
                            .with_label(Label::new(algefn.region_span.clone()).with_message(
                                &format!(
                                    "result producer in this region should be {}",
                                    algefn.return_type
                                ),
                            ))
                            .finish(),
                    );
                    return Err(err);
                }
            } else {
                let err = OptError::TypeDeriveError {
                    text: format!(
                        "Result is of type \"{}\", but result was untyped. This is a compiler bug!",
                        algefn.return_type
                    ),
                };
                report(
                    error_reporter(err.clone(), algefn.def_span.clone())
                        .with_label(
                            Label::new(algefn.region_span.clone())
                                .with_message("result producer in this region"),
                        )
                        .finish(),
                );
                return Err(err);
            }
        } else {
            let err = OptError::Any {
                text: format!("Result is of type \"{:?}\", but there was no return value set. This is a compiler bug!", algefn.return_type),
            };
            report(
                error_reporter(err.clone(), algefn.def_span.clone())
                    .with_label(
                        Label::new(algefn.region_span.clone())
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
    ) -> Result<(Ty, OutportLocation), OptError> {
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
                                let err = OptError::TypeDeriveError {
                                    text: format!("argument {i} of this call has no type"),
                                };

                                if let Some(span) = self.find_span(node.into()) {
                                    report(
                                        error_reporter(err.clone(), span.clone())
                                            .with_label(
                                                Label::new(span).with_message("for this call"),
                                            )
                                            .finish(),
                                    );
                                } else {
                                    report(error_reporter(err.clone(), Span::empty()).finish());
                                }

                                return Err(err);
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

                Ok((result_type, node.output(0)))
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

    fn gamma_derive(
        &mut self,
        gamma: NodeRef,
    ) -> Result<SmallColl<(Ty, OutportLocation)>, OptError> {
        //Check that the condional port is a boolean, then
        //map all connected (and used) ports into both regions
        //
        //After being done, map the result types out of the regions, and unify them.
        //this should catch type missmatches in gamma branches.
        let gamma_span = self.find_span(gamma.into()).unwrap_or(Span::empty());

        for input in self.graph[gamma].inport_types() {
            match input {
                InputType::GammaPredicate => {
                    let predicate_port = gamma.as_inport_location(InputType::GammaPredicate);
                    let predicate_span = self
                        .find_span(self.graph.inport_src(predicate_port).unwrap().into())
                        .unwrap_or(Span::empty());
                    if let Some(ty) = self.find_type(&predicate_port.into()) {
                        if !ty.is_bool() {
                            let err = OptError::TypeDeriveError {
                                text: format!(
                                    "Condition must be an expression of type Bool, was {ty}"
                                ),
                            };
                            report(
                                error_reporter(err.clone(), predicate_span.clone())
                                    .with_label(
                                        Label::new(predicate_span.clone())
                                            .with_message("Consider changing this expression!"),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    } else {
                        let err = OptError::TypeDeriveError {
                            text: format!(
                                "Condition must be an expression of type Bool, but had no type!"
                            ),
                        };
                        report(
                            error_reporter(err.clone(), predicate_span.clone())
                                .with_label(
                                    Label::new(predicate_span.clone())
                                        .with_message("Consider changing this expression!"),
                                )
                                .finish(),
                        );
                        return Err(err);
                    }
                }
                InputType::EntryVariableInput(ev) => {
                    //for each ev, try to get the type, if there is none, make sure that there are also no users
                    let evport = gamma.as_inport_location(input);
                    if let Some(ty) = self.find_type(&evport.into()) {
                        for region_idx in 0..self.graph[gamma].regions().len() {
                            let in_region_port = gamma
                                .as_outport_location(input.map_to_in_region(region_idx).unwrap());
                            self.typemap.set(in_region_port.into(), ty.clone());
                        }
                    } else {
                        for region_idx in 0..self.graph[gamma].regions().len() {
                            let in_region_port = gamma
                                .as_outport_location(input.map_to_in_region(region_idx).unwrap());
                            if self.graph[in_region_port].edges.len() > 0 {
                                let err = OptError::TypeDeriveError {
                                    text: format!(
                                        "Gamma input[{ev}] has no type set, but is in use in branch {region_idx}!"
                                    ),
                                };
                                report(
                                    error_reporter(err.clone(), gamma_span.clone())
                                        .with_label(
                                            Label::new(gamma_span.clone()).with_message("Here!"),
                                        )
                                        .finish(),
                                );
                                return Err(err);
                            }
                        }
                    }
                }
                _ => panic!("Malformed gamma node!"),
            }
        }

        //now do type_derivative for each branch
        for region_index in 0..self.graph[gamma].regions().len() {
            let reg = RegionLocation {
                node: gamma,
                region_index,
            };
            if let Err(e) = self.derive_region(reg, gamma_span.clone()) {
                let branch_span = self.find_span(reg.into()).unwrap_or(Span::empty());
                let intermediat_error = OptError::TypeDeriveError {
                    text: format!("Could not derive type for branch {region_index}"),
                };
                report(
                    error_reporter(intermediat_error, branch_span.clone())
                        .with_label(Label::new(branch_span.clone()).with_message("Here!"))
                        .finish(),
                );
                return Err(e);
            }
        }

        //finally, map all set outport out of the region, and error if either they are not of unified type,
        //or unset, but in use
        let mut result_sig = SmallColl::default();
        for output in self.graph[gamma].outport_types() {
            let outport = gamma.as_outport_location(output);
            match output {
                OutputType::ExitVariableOutput(idx) => {
                    if let Some(ty) = self.gamma_unified_type(gamma, idx) {
                        //set the output
                        self.typemap.set(outport.into(), ty.clone());
                        result_sig.push((ty, outport));
                    } else {
                        //ignore for unused values
                        if self.graph[outport].edges.len() == 0 {
                            continue;
                        }
                        let predicate_src = self
                            .graph
                            .inport_src(gamma.as_inport_location(InputType::GammaPredicate))
                            .unwrap();
                        let head_span = self
                            .find_span(predicate_src.into())
                            .unwrap_or(gamma_span.clone());
                        //could not unify types
                        let if_branch_ty = self.find_type(
                            &gamma
                                .as_inport_location(InputType::ExitVariableResult {
                                    branch: 0,
                                    exit_variable: idx,
                                })
                                .into(),
                        );
                        let else_branch_ty = self.find_type(
                            &gamma
                                .as_inport_location(InputType::ExitVariableResult {
                                    branch: 1,
                                    exit_variable: idx,
                                })
                                .into(),
                        );

                        let if_branch_span = self
                            .span_tags
                            .get(
                                &RegionLocation {
                                    node: gamma,
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
                                    node: gamma,
                                    region_index: 1,
                                }
                                .into(),
                            )
                            .cloned()
                            .unwrap_or(Span::empty());

                        let if_branch_ty = if_branch_ty.clone().unwrap_or(Ty::Shaped {
                            ty: DataType::Void,
                            shape: Shape::Scalar,
                        });
                        let else_branch_ty = else_branch_ty.clone().unwrap_or(Ty::Shaped {
                            ty: DataType::Void,
                            shape: Shape::Scalar,
                        });

                        let err = OptError::Any {
                            text: format!(
                                "Return type conflict. If branch returns {}, but else branch returns {}",
                                if_branch_ty, else_branch_ty
                            ),
                        };
                        report(
                            error_reporter(err.clone(), head_span)
                                .with_label(
                                    Label::new(if_branch_span.clone()).with_message(format!(
                                        "\"If\"-branch returns {if_branch_ty}"
                                    )),
                                )
                                .with_label(Label::new(else_branch_span.clone()).with_message(
                                    format!("\"Else\"-branch returns {else_branch_ty}"),
                                ))
                                .finish(),
                        );

                        return Err(err);
                    }
                }
                _ => panic!("Malformed Gamma output: {output:?}"),
            }
        }
        Ok(result_sig)
    }

    ///Does theta type derive, assuming that all used inputs are type set.
    fn theta_type_derive(
        &mut self,
        theta: NodeRef,
    ) -> Result<SmallColl<(Ty, OutportLocation)>, OptError> {
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
                        if !ty.is_integer() {
                            let err = OptError::TypeDeriveError {
                                text: format!(
                                    "Loop bound should be {}, was {}",
                                    Ty::SCALAR_INT,
                                    ty
                                ),
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
                            text: format!(
                                "Loop bound should be {}, but had no type!",
                                Ty::SCALAR_INT
                            ),
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
                    text: format!(
                        "Loop predicate should be {:?}, but is {:?}!",
                        Ty::SCALAR_BOOL,
                        ty
                    ),
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
                text: format!(
                    "Loop predicate should be {:?}, but had no type!",
                    Ty::SCALAR_BOOL,
                ),
            };
            report(
                error_reporter(err.clone(), theta_span.clone())
                    .with_label(Label::new(theta_span.clone()).with_message("Here"))
                    .finish(),
            );
            return Err(err);
        }

        //now map all results out of the theta node and build the signature
        let mut output_sig = SmallColl::default();
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

            output_sig.push((result_ty.clone(), result_port));

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
                            text: format!("Loop-Variable type missmatch: Argument was imported as {argty}, but resulted in {ty}!"),
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

        Ok(output_sig)
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

        //declare all λ-decl edges as "callable"
        self.typemap.set(
            lambda
                .as_outport_location(OutputType::LambdaDeclaration)
                .into(),
            Ty::Callable,
        );

        let consumers = self
            .graph
            .find_consumer_out(lambda.as_outport_location(OutputType::LambdaDeclaration));
        for consumer in consumers {
            let path = self.graph.trace_path(consumer);
            let t = self.type_path(&path)?;
            assert_eq!(t, Ty::Callable);
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

    pub fn try_node_type_derive(
        &mut self,
        node: NodeRef,
    ) -> Result<SmallColl<(Ty, OutportLocation)>, OptError> {
        //gather input types, those must be present, by definition
        let mut input_config = SmallColl::default();
        for input in self.graph.inports(node) {
            let ty = if let Some(near_type) = self.find_type(&input.into()) {
                near_type
            } else {
                //NOTE: there is this special case, if we _use_ a λ that was not yet type derived.
                if self.graph.find_producer_inp(input).map(|port| port.output)
                    == Some(OutputType::LambdaDeclaration)
                {
                    Ty::Callable
                } else {
                    let err = OptError::TypeDeriveError {
                        text: format!(
                        "Argument {} ({node:?}) is not connected, but also not type-set, which is an error",
                        input.input
                    ),
                    };
                    return Err(err);
                }
            };
            input_config.push(ty);
        }

        //gather all inputs and let the node try to resolve itself
        let payload: SmallColl<(Ty, OutportLocation)> = match &self.graph.node(node).node_type {
            NodeType::Simple(s) => {
                let ty =
                    s.node
                        .try_derive_type(&input_config, &self.concepts, &self.csg_node_defs)?;
                smallvec![(ty, node.output(0))]
            }
            NodeType::Apply(a) => {
                let region_span = self
                    .find_span(self.graph[node].parent.unwrap().into())
                    .unwrap_or(Span::empty());
                let res = self.try_apply_derive(node, a, &region_span)?;
                smallvec![res]
            }
            NodeType::Gamma(_g) => {
                let result = self.gamma_derive(node)?;
                result
            }
            //NOTE: by convention the θ-Node resolves to output 2
            NodeType::Theta(_t) => {
                let result = self.theta_type_derive(node)?;
                result
            }
            NodeType::Lambda(_l) => {
                self.derive_lambda(node)?;
                //lambdas alwas return a single callable
                smallvec![(
                    Ty::Callable,
                    node.as_outport_location(OutputType::LambdaDeclaration),
                )]
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
            let resolved_values = match self.try_node_type_derive(node) {
                Ok(values) => values,
                Err(e) => {
                    if let Some(span) = self.find_span(node.into()) {
                        report(
                            error_reporter(e.clone(), span.clone())
                                .with_label(
                                    Label::new(span.clone()).with_message("on this operation"),
                                )
                                .finish(),
                        )
                    } else {
                        report(error_reporter(e.clone(), Span::empty()).finish());
                    }
                    //Immediately abort, since we have no way of finishing.
                    // TODO: We could also collect all error at this point...
                    return Err(e);
                }
            };

            for (ty, port) in resolved_values {
                //typset all edges and ports that are resolved
                if let Some(old) = self.typemap.set(port.into(), ty.clone()) {
                    if old != ty {
                        let err = OptError::TypeDeriveError {
                            text: format!(
                                "Type collision, declared as {old:?}, but derived to {ty:?}"
                            ),
                        };
                        if let Some(span) = self.find_span(port.node.into()) {
                            report(
                                error_reporter(err.clone(), span.clone())
                                    .with_label(Label::new(span))
                                    .with_message(format!("Sould be {old:?}"))
                                    .finish(),
                            );
                        } else {
                            report(error_reporter(err.clone(), Span::empty()).finish());
                        }

                        return Err(err);
                    }
                }
                //And propagate to all edges
                for edg in self.graph[port].edges.clone().iter() {
                    if let Err(e) = self.graph[*edg].ty.set_derived_state(ty.clone()) {
                        if let Some(span) = self.find_span(port.node.into()) {
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
        }

        Ok(())
    }
}
