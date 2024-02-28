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

use rvsdg::{
    edge::{OutportLocation, OutputType},
    nodes::NodeType,
    region::RegionLocation,
    NodeRef,
};
use vola_common::{report, Span};

use crate::{
    alge::implblock::ConceptImpl,
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

        let mut error_count = 0;
        let implblocks = self
            .concept_impl
            .values()
            .map(|v| (v.lambda_region, v.span.clone()))
            .collect::<Vec<_>>();
        for (implblock, span) in implblocks {
            if let Err(_err) = self.derive_region(implblock, span.clone()) {
                error_count += 1;
            }
        }

        for implblock in self.concept_impl.values() {
            self.verify_imblblock(implblock)?;
        }

        let fregs = self
            .field_def
            .values()
            .map(|fdef| (fdef.lambda_region.clone(), fdef.span.clone()))
            .collect::<Vec<_>>();
        //Now do the field def
        for (def, srcspan) in fregs {
            if let Err(_err) = self.derive_region(def, srcspan.clone()) {
                error_count += 1;
            }
        }

        for fdef in self.field_def.values() {
            self.verify_field_def(fdef)?;
        }

        let exports = self
            .export_fn
            .values()
            .map(|exp| (exp.lambda_region.clone(), exp.span.clone()))
            .collect::<Vec<_>>();
        for (exp, span) in exports {
            if let Err(_err) = self.derive_region(exp, span) {
                error_count += 1;
            }
        }

        for exp in self.export_fn.values() {
            self.verify_export_fn(exp)?;
        }

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
        // always pre-set by the impblock builder, based on the concept's decleration. So there would be some
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
                    let err = OptError::AnySpanned {
                        span: block.span.clone().into(),
                        text: format!("impl-block's output was untyped"),
                        span_text: "in this region".to_owned(),
                    };
                    report(err.clone(), block.span.get_file());
                    Err(err)
                }
                Some(t) => {
                    if *t != expected_result_type {
                        if let Some(s) = result_node_span {
                            let err = OptError::AnySpanned {
                                span: s.clone().into(),
                                text: format!(
                                    "ImplBlock's output was of type {:?} but expected {:?}",
                                    t, expected_result_type
                                ),
                                span_text: "tried to return this".to_owned(),
                            };
                            report(err.clone(), s.get_file());
                            Err(err)
                        } else {
                            let err = OptError::AnySpanned {
                                span: block.span.clone().into(),
                                text: format!(
                                    "ImplBlock's output was of type {:?} but expected {:?}",
                                    t, expected_result_type
                                ),
                                span_text: "in this region".to_owned(),
                            };
                            report(err.clone(), block.span.get_file());
                            Err(err)
                        }
                    } else {
                        Ok(())
                    }
                }
            }
        } else {
            let err = OptError::AnySpanned {
                span: block.span.clone().into(),
                text: format!("ImplBlocks's output was not connected"),
                span_text: "in this region".to_owned(),
            };
            report(err.clone(), block.span.get_file());
            Err(err)
        }
    }

    fn verify_field_def(&self, fdef: &FieldDef) -> Result<(), OptError> {
        //For the field def its pretty easy. We just have to check that it actually returns a CSGTree

        if let Some(result_edge) = self.graph.region(&fdef.lambda_region).unwrap().results[0].edge {
            if let Some(ty) = self.graph.edge(result_edge).ty.get_type() {
                if ty != &Ty::CSGTree {
                    let err = OptError::AnySpanned {
                        span: fdef.span.clone().into(),
                        text: format!(
                            "field definition's output is expected to be a CSGTree, not {:?}",
                            ty
                        ),
                        span_text: "in this region".to_owned(),
                    };
                    report(err.clone(), fdef.span.get_file());
                    Err(err)
                } else {
                    Ok(())
                }
            } else {
                let err = OptError::AnySpanned {
                    span: fdef.span.clone().into(),
                    text: format!("field definition's output was not typed"),
                    span_text: "in this region".to_owned(),
                };
                report(err.clone(), fdef.span.get_file());
                Err(err)
            }
        } else {
            let err = OptError::AnySpanned {
                span: fdef.span.clone().into(),
                text: format!("field definition's output was not connected"),
                span_text: "in this region".to_owned(),
            };
            report(err.clone(), fdef.span.get_file());
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
                        let err = OptError::AnySpanned {
                            span: export.span.clone().into(),
                            text: format!(
                                "field definition's output {} is expected to be a {:?}, not {:?}",
                                i, expected_ty, ty
                            ),
                            span_text: "in this region".to_owned(),
                        };
                        report(err.clone(), export.span.get_file());
                        return Err(err);
                    }
                } else {
                    let err = OptError::AnySpanned {
                        span: export.span.clone().into(),
                        text: format!("field-export's output[{}] was not typed", i),
                        span_text: "in this region".to_owned(),
                    };
                    report(err.clone(), export.span.get_file());
                    return Err(err);
                }
            } else {
                let err = OptError::AnySpanned {
                    span: export.span.clone().into(),
                    text: format!("field-export's output[{}] was not connected", i),
                    span_text: "in this region".to_owned(),
                };
                report(err.clone(), export.span.get_file());
                return Err(err);
            }
        }

        Ok(())
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

        //Preset all edges where we know the type already
        for edg in &edges {
            let src = self.graph.edge(*edg).src().clone();
            if let Some([ty]) = self.typemap.attrib(&src.into()) {
                self.graph.edge_mut(*edg).ty = OptEdge::Value {
                    ty: TypeState::Set(ty.clone()),
                };
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

                //if the node's output edge is already set, skip node.
                match self.typemap.attrib(
                    &OutportLocation {
                        node,
                        output: OutputType::Output(0),
                    }
                    .into(),
                ) {
                    Some([ty]) => {
                        //has type set already, skip node but propagate type if needed
                        for edg in self
                            .graph
                            .node(node)
                            .outport(&OutputType::Output(0))
                            .unwrap()
                            .edges
                            .clone()
                            .iter()
                        {
                            self.graph.edge_mut(*edg).ty.set_derived_state(ty.clone());
                        }
                        continue;
                    }
                    Some(_multiple_types) => {
                        let span = self
                            .graph
                            .node(node)
                            .node_type
                            .unwrap_simple_ref()
                            .span
                            .clone();
                        return Err(OptError::AnySpanned {
                            span: span.into(),
                            text: "Node hat multiple types set on output-port".to_owned(),
                            span_text: "here".to_owned(),
                        });
                    }
                    _ => {}
                }

                let type_resolve_try = self
                    .graph
                    .node(node)
                    .node_type
                    .unwrap_simple_ref()
                    .node
                    .try_derive_type(
                        &self.typemap,
                        &self.graph,
                        &self.concepts,
                        &self.csg_node_defs,
                    );
                match type_resolve_try {
                    Ok(Some(ty)) => {
                        //flag change.
                        resolved_any_node = true;

                        //now assign that type to the nodes's output. For sanity reasons, make sure there
                        // is actually just one output.
                        assert!(
                            self.graph.node_mut(node).outputs().len() == 1,
                            "encounterd node with != 1 outport"
                        );
                        self.typemap.push_attrib(
                            &OutportLocation {
                                node,
                                output: OutputType::Output(0),
                            }
                            .into(),
                            ty.clone(),
                        );

                        //And propagate to all edges
                        for edg in self
                            .graph
                            .node(node)
                            .outport(&OutputType::Output(0))
                            .unwrap()
                            .edges
                            .clone()
                            .iter()
                        {
                            self.graph.edge_mut(*edg).ty = OptEdge::Value {
                                ty: TypeState::Derived(ty.clone()),
                            };
                        }
                    }
                    Ok(None) => {
                        //push back into build_stack
                        build_stack.push_front(node);
                    }
                    Err(e) => {
                        let e = e.into_spanned(
                            &self.graph.node(node).node_type.unwrap_simple_ref().span,
                        );
                        report(
                            e.clone(),
                            self.graph
                                .node(node)
                                .node_type
                                .unwrap_simple_ref()
                                .span
                                .get_file(),
                        );
                        //Immediatly abort, since we have no way of finishing.
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
                let optnode = self.graph.node(*failed_node);
                let err = OptError::AnySpanned {
                    span: optnode.node_type.unwrap_simple_ref().span.clone().into(),
                    text: format!("Failed to derive a type"),
                    span_text: "for this".to_owned(),
                };
                report(err, optnode.node_type.unwrap_simple_ref().span.get_file());
            }

            return Err(OptError::TypeDeriveFailed {
                errorcount: build_stack.len(),
                span: region_src_span.into(),
            });
        }

        Ok(())
    }
}
