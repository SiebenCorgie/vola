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

use std::collections::VecDeque;

use ahash::{AHashMap, AHashSet};
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation},
    nodes::{ApplyNode, NodeType},
    region::RegionLocation,
    smallvec::SmallVec,
    NodeRef,
};
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
                            .with_label(Label::new(block.span.clone()).with_message("in this region"))
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
                                    .with_label(Label::new(s.clone()).with_message("Tried to return this"))
                                    .finish(),
                            );
                            Err(err)
                        } else {
                            let err = OptError::Any{
                                text: format!(
                                    "ImplBlock's output was of type {:?} but expected {:?}",
                                    t, expected_result_type
                                ),
                            };
                            report(
                                error_reporter(err.clone(), block.span.clone())
                                    .with_label(Label::new(block.span.clone()).with_message("In this region"))
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
                            .with_label(Label::new(fdef.span.clone()).with_message("In this region"))
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
                                .with_label(Label::new(export.span.clone()).with_message("In this region"))
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
                            .with_label(Label::new(export.span.clone()).with_message("In this region"))
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
                                .with_label(Label::new(algefn.span.clone()).with_message(&format!("user of argument {argidx} in this region")))
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
                            .with_label(Label::new(algefn.span.clone()).with_message(&format!("user of argument {argidx} in this region")))
                            .finish(),
                    );
                    return Err(err);
                }
            }
        }

        //Check that the result is set correctly
        if let Some(edg) = &self.graph.node(algefn.lambda).node_type.unwrap_lambda_ref().result(0).unwrap().edge{
            if let Some(ty) = self.graph.edge(*edg).ty.get_type(){
                if ty != &algefn.retty{
                    let err = OptError::Any { 
                        text: format!("Result type was \"{ty:?}\", but expected {:?}!", algefn.retty), 
                    };
                    report(
                        error_reporter(err.clone(), algefn.span.clone())
                            .with_label(Label::new(algefn.span.clone()).with_message(&format!("result producer in this region should be {:?}", algefn.retty)))
                            .finish(),
                    );
                    return Err(err);
                }
            }else{
                let err = OptError::Any { 
                    text: format!("Result is of type \"{:?}\", but connected edge was untyped. This is a compiler bug!", algefn.retty), 
                };
                report(
                    error_reporter(err.clone(), algefn.span.clone())
                        .with_label(Label::new(algefn.span.clone()).with_message("result producer in this region"))
                        .finish(),
                );
                return Err(err);
            }
        }else{
            let err = OptError::Any { 
                text: format!("Result is of type \"{:?}\", but there was no return value set. This is a compiler bug!", algefn.retty), 
            };
            report(
                error_reporter(err.clone(), algefn.span.clone())
                    .with_label(Label::new(algefn.span.clone()).with_message("no result producer in this region"))
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
                            let ty = if let Some(ty) = self.graph.edge(edg).ty.get_type(){
                                ty
                            }else{
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
                                    .with_label(Label::new(region_src_span.clone()).with_message("In this region"))
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
                                .with_label(Label::new(span.clone()).with_message("of this function"))
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
                        .with_label(Label::new(region_src_span.clone()).with_message("In this region"))
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

    fn try_gamma_derive(&mut self, node: NodeRef) -> Result<Option<Ty>, OptError>{
        //We just need to check two things:
        // 1. that the conditional is a Boolean output, 
        // 2. That all branches emit the same type

        let conditional_type = {
            if let Some(condty) = self.get_type_for_inport(InportLocation { node, input: InputType::GammaPredicate }){
                condty
            }else{
                //Was not yet resolved, return.
                return Ok(None);
            }
        };

        if !conditional_type.is_bool(){
            let span = {
                let condition_src = self.graph.node(node).input_src(&self.graph, 0).unwrap();
                self.span_tags.get(&condition_src.into()).cloned().unwrap_or(Span::empty())
            };
            
            let err = OptError::Any {
                text: format!("Condition must be an expression of type Bool, was {conditional_type}"),
            };
            report(
                error_reporter(err.clone(), span.clone())
                    .with_label(Label::new(span.clone()).with_message("Consider changing this expression!"))
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
        for input_type in self.graph.node(node).inport_types(){

            
            if let Some(ty) = self.find_type(&InportLocation{node, input: input_type}.into()).clone(){
                for ridx in 0..subregion_count{
                    if let Some(mapped_internally) = input_type.map_to_in_region(ridx){
                        self.typemap.set(OutportLocation{node, output: mapped_internally}.into(), ty.clone());
                    }
                }
            }else{
                //NOTE: We check if the port is in use. If not, its okey if no type information was 
                //      found
                let is_in_use = {
                    let mut is_in_use = false;
                    for subreg in 0..subregion_count{
                        if let Some(p) = self.graph.node(node).outport(&input_type.map_to_in_region(subreg).unwrap()){
                             if p.edges.len() > 0 {
                                 is_in_use = true;
                                 break;
                             }
                        }
                    }
                    is_in_use
                };

                if is_in_use{
                    //Port is in use, but input is untype, therfore try again later.
                    return Ok(None);
                }
            }
        }

        //if we reached this, recurse!
        for ridx in 0..subregion_count{
            let regloc = RegionLocation{node, region_index: ridx};
            let span = self.span_tags.get(&regloc.into()).cloned().unwrap_or(Span::empty());
            self.derive_region(regloc, span)?;
        }

        //make sure that both regions return the same type.
        //now find the output type of both regions, and make sure they are the same. If so, we are good to go
        let reg_if_ty = if let Some(t) = self.find_type(&InportLocation{node, input: InputType::ExitVariableResult { branch: 0, exit_variable: 0 }}.into()){
            t
        }else{
            panic!("Gamma-internal if-branch region should have been resolved!");
        };
        let reg_else_ty = if let Some(t) = self.find_type(&InportLocation{node, input: InputType::ExitVariableResult { branch: 1, exit_variable: 0 }}.into()){
            t
        }else{
            panic!("Gamma-internal else-branch should have been resolved!");
        };

        if reg_if_ty != reg_else_ty{
            let if_branch_span = self.span_tags.get(&RegionLocation{node, region_index: 0}.into()).cloned().unwrap_or(Span::empty());     
            let else_branch_span = self.span_tags.get(&RegionLocation{node, region_index: 1}.into()).cloned().unwrap_or(Span::empty());

            let err = OptError::Any { text: format!("Return type conflict. If branch returns {}, but else branch returns {}", reg_if_ty, reg_else_ty) };
            report(
                error_reporter(err.clone(), if_branch_span.clone())
                    .with_label(Label::new(if_branch_span.clone()).with_message(format!("This returns {reg_if_ty}")))
                    .with_label(Label::new(else_branch_span.clone()).with_message(format!("This returns {reg_else_ty}")))
                    .finish(),
            );

            Err(err)
        }else{
            Ok(Some(reg_if_ty))
        }
    } 

    fn try_theta_derive(&mut self, node: NodeRef) -> Result<Option<Ty>, OptError>{

        let loop_node_span = self.span_tags.get(&node.into()).cloned().unwrap_or(Span::empty());
        //check lower bound value
        if let Some(ty) = self.find_type(&InportLocation{node, input: InputType::Input(0)}.into()){
            if ty != Ty::Nat{
                let err = OptError::Any { text: format!("Loop bound conflict: Lower loop bound needs to be a natural number, but is {ty}") };
                report(
                    error_reporter(err.clone(), loop_node_span.clone())
                        .with_label(Label::new(loop_node_span.clone()).with_message(format!("in this loop")))
                        .finish()
                );
                return Err(err);
            }
        }else{
            //not yet resolved
            return Ok(None);
        }
        //check higher bound value
        if let Some(ty) = self.find_type(&InportLocation{node, input: InputType::Input(1)}.into()){
            if ty != Ty::Nat{
                let err = OptError::Any { text: format!("Loop bound conflict: Upper loop bound needs to be a natural number, but is {ty}") };
                report(
                    error_reporter(err.clone(), loop_node_span.clone())
                        .with_label(Label::new(loop_node_span.clone()).with_message(format!("in this loop")))
                        .finish()
                );
                return Err(err);
            }
        }else{
            //not yet resolved
            return Ok(None);
        }

        //now checkout the type of the assigned result. This basically makes sure, that we can later on 
        //identify type missmatches between the first assigned value, and the later-on assigned 
        //value in the loop.
        let initial_result_type = if let Some(ty) = self.find_type(&InportLocation{node, input: InputType::Input(2)}.into()){
            ty
        }else{
            //not yet set
            return Ok(None);
        };
        
        //the idea here is similar, we only start deriving the theta node, once we
        //could tag all lv-variables with a type.
        //however, in here, we can do all testing only _after_ finishing
        for input_type in self.graph.node(node).inport_types(){
            if let Some(ty) = self.find_type(&InportLocation{node, input: input_type}.into()).clone(){
                if let Some(mapped_internally) = input_type.map_to_in_region(0){
                    self.typemap.set(OutportLocation{node, output: mapped_internally}.into(), ty.clone());
                }else{
                    if input_type != InputType::ThetaPredicate{
                        panic!("any non-theta-predicate type should be mapable, but {input_type:?} wasnt!");
                    }
                }
            }else{
                //this one wasn't set, so return
                return Ok(None);
            }
        }

        //now dispatch the loop-region
        let regloc = RegionLocation{node, region_index: 0};
        let span = self.span_tags.get(&regloc.into()).cloned().unwrap_or(Span::empty());
        self.derive_region(regloc, span)?;

        //now do our semantic tests. which is:
        // - theta_predicate needs to be bool,
        // - there needs to be only one return value ..
        // - ...and that needs to be typed the same way as initial_result_type
        if let Some(ty) = self.find_type(&InportLocation{node, input: InputType::ThetaPredicate}.into()){
            if ty != Ty::Bool{
                let err = OptError::Any { text: format!("Loop condition must be Bool, but was {ty}. This is a bug!") };
                report(error_reporter(err.clone(), loop_node_span.clone()).with_label(Label::new(loop_node_span).with_message("here")).finish());
                return Err(err);
            }
        }else{
            //this is an error at this point, since the region derive has already returned 
            let err = OptError::Any { text: format!("Could not derive a type for the loop's break condition. This is a bug!") };
            report(error_reporter(err.clone(), loop_node_span.clone()).with_label(Label::new(loop_node_span).with_message("here")).finish());
            return Err(err);
        }

        //now do the lv of the loop-value
        if let Some(ty) = self.find_type(&InportLocation{node, input: InputType::Result(2)}.into()){
            if ty != initial_result_type{
                let err = OptError::Any { text: format!("Loop value must be {initial_result_type}, but was {ty}.") };
                //TODO: find the right span?
                let value_result_span = Span::empty();
                report(error_reporter(err.clone(), loop_node_span.clone()).with_label(Label::new(loop_node_span.clone()).with_message("first defined here as {initial_result_type}")).with_label(Label::new(value_result_span).with_message("But than defined here as {ty}")).finish());
                return Err(err);
            }
        }else{
            //this is an error at this point, since the region derive has already returned 
            let err = OptError::Any { text: format!("Could not derive a type for the loop-value. This is a bug!") };
            report(error_reporter(err.clone(), loop_node_span.clone()).with_label(Label::new(loop_node_span).with_message("here")).finish());
            return Err(err);
        }

        Ok(Some(initial_result_type))
    }
    
    fn derive_region(
        &mut self,
        reg: RegionLocation,
        region_src_span: Span,
    ) -> Result<(), OptError> {
        //First gather all nodes in the region
        let (mut build_stack, edges) = self
            .graph
            .on_region(&reg, |reg| {
                //The resolution stack
                let build_stack: VecDeque<NodeRef> =
                    reg.region().nodes.iter().map(|n| *n).collect();
                let edges = reg.region().edges.iter().map(|e| *e).collect::<Vec<_>>();
                (build_stack, edges)
            })
            .expect("Failed to gather nodes in λ-Region");

        //Preset all edges where we know the type already. For instance if the type map 
        //contains type info for any of the ports of an edge
        for edg in &edges {
            let src = self.graph.edge(*edg).src().clone();
            if let Some(ty) = self.find_type(&src.into()) {
                if let Some(preset) = self.graph.edge(*edg).ty.get_type() {
                    if preset != &ty {
                        let err = OptError::Any { 
                            text: format!("Edge was already set to {:?}, but was about to be overwritten with an incompatible type {:?}", preset, ty), 
                        };                        
                        report(
                            error_reporter(err.clone(), region_src_span.clone())
                                .with_label(Label::new(region_src_span.clone()).with_message("In this region"))
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

        'resolution_loop: loop {
            //Flag that tells us _after_ touching all nodes,
            // if we made any advances. If not we are stuck and return with an error.
            let mut resolved_any_node = false;

            let mut local_stack = VecDeque::new();
            std::mem::swap(&mut local_stack, &mut build_stack);
            for node in local_stack {
                //gather all inputs and let the node try to resolve itself
                let (type_resolve_try, resolved_port) = match &self.graph.node(node).node_type {
                    NodeType::Simple(s) => {let ty = s.node.try_derive_type(
                        &self.typemap,
                        &self.graph,
                        &self.concepts,
                        &self.csg_node_defs,
                    );
                    
                    (ty, node.output(0))},
                    NodeType::Apply(a) => {let ty = self.try_apply_derive(node, a, &region_src_span); (ty, node.output(0))},
                    NodeType::Gamma(_g) => {
                        let ty = self.try_gamma_derive(node);
                        (ty, node.output(0))
                    },
                    //NOTE: by convention the θ-Node resolves to output 2
                    NodeType::Theta(_t) => {let ty = self.try_theta_derive(node); (ty, node.output(2))},
                    t => {
                        let err = OptError::Any {
                            text: format!(
                                "Unexpected node type {t} in graph while doing type resolution."
                            ),
                        };
                        report(
                            error_reporter(err.clone(), region_src_span.clone())
                                .with_label(Label::new(region_src_span.clone()).with_message("In this region"))
                                .finish(),
                        );
                        return Err(err);
                    }
                };

                match type_resolve_try {
                    Ok(Some(ty)) => {
                        //flag change.
                        resolved_any_node = true;

                        //now assign that type to the nodes's output.
                        self.typemap.set(
                            resolved_port.into(),
                            ty.clone(),
                        );

                        //And propagate to all edges
                        for edg in self
                            .graph
                            .node(node)
                            .outport(&resolved_port.output)
                            .unwrap()
                            .edges
                            .clone()
                            .iter()
                        {
                            
                            if let Err(e) = self.graph.edge_mut(*edg).ty.set_derived_state(ty.clone()){
                                
                                if let Some(span) = self.find_span(resolved_port.node.into()){
                                    report(error_reporter(e.clone(), span.clone()).with_label(Label::new(span.clone()).with_message("On this node")).finish());
                                    
                                }else{
                                    report(error_reporter(e.clone(), Span::empty()).finish());
                                }
                                
                                return Err(e);

                            }
                        }
                    }
                    Ok(None) => {
                        //push back into build_stack
                        build_stack.push_front(node);
                    }
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
                }
            }

            //If we didn't change anything, or if the build_stack is empty, end the loop
            if !resolved_any_node || build_stack.is_empty() {
                break 'resolution_loop;
            }
        }

        //If the build stack is not empty at this point, type derivation has failed. Report all failed nodes
        if !build_stack.is_empty() {
            for failed_node in &build_stack {
                let node = self.graph.node(*failed_node);
                let span = match &node.node_type {
                    NodeType::Simple(s) => Some(s.span.clone()),
                    NodeType::Apply(_) => {
                        if let Some(span) = self.span_tags.get(&failed_node.clone().into()) {
                            Some(span.clone())
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                
                let err = OptError::Any {
                    text: format!("Failed to derive a type"),
                };

                if let Some(span) = span{
                    report(
                        error_reporter(err.clone(), span.clone())
                            .with_label(Label::new(span.clone()).with_message("for this"))
                            .finish(),
                    );
                }else{
                    report(
                        error_reporter(err.clone(), Span::empty())
                            .with_message(&format!("On note of type {} without span", &node.node_type)).finish()
                    );
                }
                
            }

            
            if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_TYPE_DERIVE_FAILED").is_ok() {
                self.push_debug_state("type derive failed");
            }
            
            return Err(OptError::TypeDeriveFailed {
                errorcount: build_stack.len(),
            });
        }

        Ok(())
    }
}
