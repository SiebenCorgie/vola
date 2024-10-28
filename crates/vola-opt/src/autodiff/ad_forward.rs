/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # Forward-Mode auto differentiation
//!
//! Implements the forward autodiff pass based on a diff-entry point node.
//!
//! ## Basic idea
//!
//! The idea of the forward pass is relatively straight forward. Similar to the specialization pass we walk
//! _through_ our expression graph. For any _dynamic_ node, so any node that uses _the_ wrt-argument, we split off
//! the walk through as f'(g(x)) = f'(g(x)) * g'(x), where f'(g(x)) is one split, and g'(x) is the other split.
//!
//! In order to be able to do that we assume that the expression is already in _ad-canonicalized-form_. So is only made from
//! essential operations.
//!
//! In forward mode, control flow stays _in the same order_ so branches/gamma nodes work the same way (chose
//! one of the derivatives in a branch), and loops also work the same way, just for the derivative.
//!
//! In practice we copy over control-flow nodes into the derivative calculation part and replace a result r with the
//! derivative of r.
//!
//! ## Implementation details
//!
//! ### Highlevel
//!
//! We start from just a single `AutoDiff` node.
//! The first part is canonicalizing the subgraph referenced by the first argument into ad-able form.
//! That means mostly transforming/unfolding complex expressions into _elementary_ operations, as well as checking
//! That no _none-ad-able_ nodes are in the graph.
//!
//! NOTE: Right now we also inline all function calls, because that makes life easier atm. However, it would be nicer
//! to specialize a _derivative-of-that-λ_, that could be reused if needed.
//!
//! After that we create the derivative of all wrt arguments (see below _derivative creation_).
//!
//! The last part is replacing the `AutoDiff` node with either the single derivative (in case of scalar output),
//! or a vector-constructor (in case of vector-output).
//!
//! ## Derivative creation
//!
//! Derivative creation is a two step process.
//! The first pass explores the computational tree, and tags nodes that are depending on the wrt-argument, _dynamically_.
//! That lets the actual forward pass distinguish the case f'(g(x)) = f'(g(x)) * g'(x) from the case f'(c) = 0
//!
//!
//! Finally we are ready to do the forward accumulation. The nice
//! part being, that for any constant we can reuse parts of the original
//! graph.

use super::{activity::Activity, AdResponse, AutoDiffError};
use crate::{autodiff::AutoDiff, OptEdge, OptError, Optimizer};
use ahash::AHashMap;
use rvsdg::{
    edge::{InportLocation, OutportLocation, OutputType},
    region::RegionLocation,
    util::abstract_node_type::AbstractNodeType,
    NodeRef,
};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

type DiffExprCache = AHashMap<OutportLocation, OutportLocation>;

struct ForwardADCtx {
    ///Tracks already created derivatives for reuse
    expr_cache: DiffExprCache,
    ///Handles activity exploration and initialization of active values
    activity: Activity,
}

impl Optimizer {
    ///Executes forward-ad pass on `entrypoint`. Assumes that it is a `AutoDiff` node. If that is the case, the node will be replaced
    /// with the differentiated value(s) after this pass (successfuly) ends.
    pub fn forward_ad(&mut self, entrypoint: NodeRef) -> Result<(), OptError> {
        if !self.is_node_type::<AutoDiff>(entrypoint) {
            return Err(OptError::Internal(format!(
                "AD Entrypoint was not of type AutoDiff"
            )));
        }

        //If the wrt-arg is a constructor, linearize the ad-entrypoint into
        // multiple AD-Nodes with a single (scalar) WRT-Arg.
        let entrypoints = self.linearize_ad(entrypoint)?;

        let region = self.graph[entrypoint].parent.unwrap();
        //TODO: Can speed up thing, or make them slower, do heuristically
        //self.graph.dne_region(region)?;

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_FWAD_LINEARIZED").is_ok() {
            self.push_debug_state(&format!("fw-autodiff-{entrypoint}-linearized"));
        }

        for entrypoint in &entrypoints {
            self.canonicalize_for_ad(*entrypoint)?;
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("DUMP_FWAD_CANONICALIZED").is_ok()
        {
            self.push_debug_state(&format!("fw-autodiff-{entrypoint}-canonicalized"));
        }

        //do type derive and dead-node elemination in order to have
        // a) clean DAG (faster / easier transformation)
        // b) type information to emit correct zero-nodes
        self.graph.dne_region(region)?;
        let span = self.find_span(region.into()).unwrap_or(Span::empty());
        self.derive_region(region, span)?;

        //All entrypoints are with respect to a single scalar at this point,
        //and hooked up to the vector-value_creator already (if-needed).
        //
        // canonicalization is also taken care of, so we can just dispatch the forward
        // diff for all of them.
        for entrypoint in entrypoints {
            self.forward_diff(entrypoint)?;
        }

        Ok(())
    }

    ///Builds the topological order of `value`'s producer node and all its dependencies in that region
    fn collect_with_dependencies_in_region(
        &self,
        region: RegionLocation,
        value: OutportLocation,
    ) -> Vec<NodeRef> {
        //build dependency node list for diff node's expression
        let mut dependencies: Vec<_> = self
            .graph
            .walk_predecessor_nodes_region(value.node, region)
            .collect();
        //push back expr_seed node
        dependencies.push(value.node);

        //order the dependencies
        let ordered_dependencies = self
            .graph
            .topological_order_nodes(dependencies.iter().cloned());
        ordered_dependencies
    }

    ///The actual forward-autodiff implementation for a single, scalar
    /// wrt-arg.
    ///
    /// Replaces the AutoDiff node with the differential value.
    fn forward_diff(&mut self, diffnode: NodeRef) -> Result<(), OptError> {
        let region = self.graph.node(diffnode).parent.unwrap();

        let expr_src = self
            .graph
            .inport_src(InportLocation {
                node: diffnode,
                input: AutoDiff::expr_input(),
            })
            .ok_or(OptError::from(AutoDiffError::EmptyExprArg))?;

        //find all active nodes of this expression, then start doing the forward iteration,
        //guided by the activity analysis.
        let activity = self.activity_explorer(diffnode)?;

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_AD_ACTIVITY").is_ok() {
            self.push_debug_state_with(&format!("fw-autodiff-{diffnode}-activity"), |builder| {
                builder.with_flags("activity", &activity.active)
            });
        }

        let mut ctx = ForwardADCtx {
            expr_cache: DiffExprCache::new(),
            activity,
        };

        let derivative_src = self.forward_diff_in_region(region, &mut ctx, expr_src)?;

        //replace the differential_value and the autodiff node
        self.graph
            .replace_outport_uses(diffnode.output(0), derivative_src)?;

        Ok(())
    }

    ///Builds the derivative of `expr_src` based on `ctx`'s activity state.
    fn forward_diff_in_region(
        &mut self,
        region: RegionLocation,
        ctx: &mut ForwardADCtx,
        expr_src: OutportLocation,
    ) -> Result<OutportLocation, OptError> {
        let ordered_dependencies = self.collect_with_dependencies_in_region(region, expr_src);

        //Now iterate the topo-ordered dependecy list. This is effectively the
        // _forward_ part of forward AD.
        //
        // In practice we can be _safe_ that for each considered node, the predecessor's derivative was already calculated, if needed.
        //
        // Therfore we only need to build this node's derivative, and possibly hook-up chain rule sub expressions
        for node in ordered_dependencies {
            self.build_derivative(region, node, ctx)?;
        }

        //There is an edge case, where the initial expression is not active. In that case _nothing_
        //will have happened, what effectively won't generate the zero-node for the expression. In that case
        //we post-derivative generate the expression.
        //
        //NOTE: we don't do that in build_derivative, since we'd otherwise generate a zero for each inactive node in the dependency
        //      graph, which is a big waste.
        //for inactive nodes, try to get the value producer output, and let our port handler generate an inactive
        //seed (usually zero or something)
        if !ctx.activity.is_active_port(self, expr_src) {
            let inactive_port = self.fwad_handle_port(region, expr_src, ctx)?;
            ctx.expr_cache.insert(expr_src, inactive_port);
        }

        //retrieve the derivative of the diff expression port from the expr-cache
        let derivative_src = *ctx
            .expr_cache
            .get(&expr_src)
            .expect("Expected derivative for output!");

        //Before ending, always do a final type derive though
        let span = self.find_span(region.into()).unwrap_or(Span::empty());
        self.derive_region(region, span)?;

        Ok(derivative_src)
    }

    ///Builds the derivative of `node`, based on the given activties.
    fn build_derivative(
        &mut self,
        parent_region: RegionLocation,
        node: NodeRef,
        ctx: &mut ForwardADCtx,
    ) -> Result<(), OptError> {
        //Bail none active nodes
        if !ctx.activity.is_node_active(&self, node) {
            return Ok(());
        }

        //handle node derivative
        let response = self.fwad_handle_node(parent_region, node, ctx)?;

        assert_eq!(response.src_output, self.value_producer_port(node).unwrap());
        //push active derivative into cache
        ctx.expr_cache
            .insert(response.src_output, response.diff_output);

        //now hookup all post-connection ports that are signaled by the node handler.
        for (post_diff_src, targets) in response.chained_derivatives {
            //Try to reuse expr-cache, otherwise build new subexpression
            let diff_expr_src = self
                .fwad_handle_port(parent_region, post_diff_src, ctx)
                .unwrap();
            for t in targets {
                self.graph
                    .connect(diff_expr_src, t, OptEdge::value_edge_unset())?;
            }
        }

        Ok(())
    }

    /// Handles the creation of the derivative of a node, as well as generating additional connection data.
    fn fwad_handle_node(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
        ctx: &mut ForwardADCtx,
    ) -> Result<AdResponse, OptError> {
        //we should end up here only for active nodes, otherwise the value would be zero already
        // (by the port handler)
        assert!(ctx.activity.is_node_active(&self, node));

        match self.graph[node].into_abstract() {
            AbstractNodeType::Simple => {
                //active simple nodes are our _main_ working point.
                // We can skip all non-alge-dialect, nodes by simply recursing.
                // The alge dialect nodes are then handled in a seperate dispatch
                // handler.

                match self.graph[node]
                    .node_type
                    .unwrap_simple_ref()
                    .node
                    .dialect()
                {
                    "alge" => self
                        .fwd_handle_alge_node(region, node, ctx)
                        .map_err(|e| e.into()),
                    "autodiff" => {
                        report(
                            error_reporter(
                                OptError::from(AutoDiffError::UnexpectedAutoDiffNode),
                                Span::empty(),
                            )
                            .finish(),
                        );

                        return Err(AutoDiffError::UnexpectedAutoDiffNode.into());
                    }
                    "imm" => {
                        //an active immediate value will be splat to zero, since this will always
                        //be equivalent to a constant.
                        Ok(AdResponse::new(
                            node.output(0),
                            self.emit_zero_for_port(region, node.output(0)),
                        ))
                    }
                    other => {
                        //Right now we panic for all unknown nodes.
                        //
                        // The _right_ thing to do would be to find out if this node
                        // is relevant to AD, if not, overstep, otherwise let it AD
                        // itself.
                        panic!("Unexpected dialect {other}");
                    }
                }
            }
            AbstractNodeType::Apply => {
                //Similar approach to γ/τ nodes. We deep copy the λ that is being called,
                //copy over the activity state, then recurse into its region.
                //this'll effectively create λ-derivative implementation for us. Finally we replace the call site
                //with that expression.

                let original_value_port = node.output(0);
                let callsrc = self
                    .graph
                    .find_callabel_def(self.graph.inport_src(node.input(0)).unwrap())
                    .unwrap();
                let call_region = self.graph[callsrc.node].parent.unwrap();
                //note we copy the λ into its parent region
                let lmd_cpy = self.graph.deep_copy_node(callsrc.node, call_region);
                self.copy_input_connections(callsrc.node, lmd_cpy);
                self.copy_node_attributes(callsrc.node, lmd_cpy);
                for attrib in self.graph.iter_node_attribs(callsrc.node) {
                    let dst_attrib_loc = attrib.clone().change_node(lmd_cpy);
                    let _ = ctx.activity.active.copy(&attrib, dst_attrib_loc);
                }

                //build a new call node that is connected to the lmd copy
                let (in_region_cpy_src, _) = self.graph.import_context(
                    lmd_cpy.as_outport_location(OutputType::LambdaDeclaration),
                    region,
                )?;

                let apply_copy = self
                    .graph
                    .on_region(&region, |b| {
                        let (node, _) = b.call(in_region_cpy_src, &[]).unwrap();
                        node
                    })
                    .unwrap();

                for inty in self.graph[node].inport_types().into_iter().skip(1) {
                    if let Some(src_edg) = self.graph[node.as_inport_location(inty)].edge {
                        let src = self.graph[src_edg].src().clone();
                        let ty = self.graph[src_edg].ty.clone();
                        self.graph
                            .connect(src, apply_copy.as_inport_location(inty), ty)
                            .unwrap();
                    }
                }

                self.copy_node_attributes(node, apply_copy);
                //Pre trace new λ node, by tracing the apply node
                ctx.activity.trace_node_activity(self, apply_copy);
                let lmd_region = RegionLocation {
                    node: lmd_cpy,
                    region_index: 0,
                };
                for resultty in self.graph[lmd_cpy].result_types(0) {
                    let result_port = InportLocation {
                        node: lmd_cpy,
                        input: resultty,
                    };
                    if let Some(value_edge) = self.graph[result_port].edge {
                        let src = *self.graph[value_edge].src();
                        let output = self.forward_diff_in_region(lmd_region, ctx, src)?;
                        //disconnect old edge, connect new edge
                        let _old_value = self.graph.disconnect(value_edge).unwrap();
                        //Don't know the type yet though
                        self.graph
                            .connect(output, result_port, OptEdge::value_edge_unset())?;
                    }
                }
                //Post derive region after AD
                self.derive_region(
                    RegionLocation {
                        node: lmd_cpy,
                        region_index: 0,
                    },
                    Span::empty(),
                )
                .unwrap();

                self.names
                    .set(lmd_cpy.into(), format!("λ derivative of {}", callsrc.node));

                let result_port = apply_copy.output(0);
                //register the diffed output in the cache for the gamma-nodes output.
                ctx.expr_cache.insert(original_value_port, result_port);

                //finaly return the gamma's output as the diff value
                Ok(AdResponse::new(original_value_port, result_port))
            }
            AbstractNodeType::Gamma => {
                //We handle active gamma nodes by deep-copying them, and then recursing the forward-algorithm
                //within that region. Once that region returns,
                //we return the gamma-node's result
                let original_value_output =
                    node.as_outport_location(OutputType::ExitVariableOutput(0));
                let gamma_cpy = self.graph.deep_copy_node(node, region);
                //Copy over connections
                self.copy_input_connections(node, gamma_cpy);
                //copy over activity attributes
                for attrib in self.graph.iter_node_attribs(node) {
                    let dst_attrib_loc = attrib.clone().change_node(gamma_cpy);
                    let _ = ctx.activity.active.copy(&attrib, dst_attrib_loc);
                }
                //let it trace the activity.
                //NOTE: This also pushes forward the producer states into the newly created
                //      Region. Note that otherwise, when calling the activity-state first time from _within_
                //      the node, this would get resolved _the wrong way_.
                ctx.activity.trace_node_activity(self, gamma_cpy);
                //Now with the activity and producer-state set,
                //let the recursion handle all region's outputs.
                for reg in 0..self.graph[gamma_cpy].regions().len() {
                    let subregion = RegionLocation {
                        node: gamma_cpy,
                        region_index: reg,
                    };

                    for resultty in self.graph[gamma_cpy].result_types(reg) {
                        let result_port = InportLocation {
                            node: gamma_cpy,
                            input: resultty,
                        };
                        if let Some(value_edge) = self.graph[result_port].edge {
                            let src = *self.graph[value_edge].src();
                            let output = self.forward_diff_in_region(subregion, ctx, src)?;
                            //disconnect old edge, connect new edge
                            let _old_value = self.graph.disconnect(value_edge).unwrap();
                            //Don't know the type yet though
                            self.graph
                                .connect(output, result_port, OptEdge::value_edge_unset())?;
                        }
                    }
                }

                self.names
                    .set(gamma_cpy.into(), format!("Derivative of {node}"));

                let result_port = gamma_cpy.as_outport_location(OutputType::ExitVariableOutput(0));
                //register the diffed output in the cache for the gamma-nodes output.
                ctx.expr_cache.insert(original_value_output, result_port);
                //finaly return the gamma's output as the diff value
                Ok(AdResponse::new(original_value_output, result_port))
            }
            other => return Err(AutoDiffError::FwadUnexpectedNodeType(other).into()),
        }
    }

    ///Handles any _potentually_ differentiatable node
    fn fwd_handle_alge_node(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
        ctx: &mut ForwardADCtx,
    ) -> Result<AdResponse, AutoDiffError> {
        //This is the point, where we handle the dispatch of the chain rule.
        //
        //Note that things like cross-product-rule, quotient-rule or dot-product-rule are
        //handeled by the respective node.
        //
        //however, dispatching the chain-rule can / must be done
        let response = match self.build_diff_value(region, node, &mut ctx.activity) {
            Ok(t) => t,
            Err(e) => {
                if let Some(span) = self.find_span(node.into()) {
                    report(
                        error_reporter(e.clone(), span.clone())
                            .with_label(Label::new(span).with_message("On this operation"))
                            .finish(),
                    );
                    return Err(e.into());
                } else {
                    return Err(e.into());
                }
            }
        };

        Ok(response)
    }

    //Small indirection that lets us catch active & WRT-Ports. This is mostly used if
    //a region-argument is also a wrt-argument, to end the recursion, or emit zero if a region's non active
    //argument is used.
    fn fwad_handle_port(
        &mut self,
        region: RegionLocation,
        port: OutportLocation,
        ctx: &mut ForwardADCtx,
    ) -> Result<OutportLocation, AutoDiffError> {
        //A port that is part of the respect_to chain always derives to 1.0
        if ctx.activity.is_wrt_producer(&port) {
            //NOTE: _handling_ a wrt port means splatting it with 1.0
            //      Math wise this is handling a expression:
            //      f(x) = x;
            //      where f'(x) = f'(x) = 1*x^0 = 1
            //
            //      For vectors this means initing _the-right_ index with one, same for matrix.
            //      We have that information from the activity trace, which if why we'll use that.
            //println!("Getting {port:?} : {:?}", activity.wrt_producer.get(&port));
            Ok(ctx
                .activity
                .build_diff_init_value_for_wrt(self, region, port))
        } else {
            if port.output.is_argument() {
                //If the outport is active and an argument, but not a wrt-producer, this means that
                //we get _supplied_ the diffed_value here, so just use that
                //
                //if however the port is not active, replace it with an apropriate zero
                if ctx.activity.is_active_port(self, port) {
                    Ok(port)
                } else {
                    Ok(self.emit_zero_for_port(region, port))
                }
            } else {
                //if is no argument, and no wrt producer, check if the port is active, if not
                //it can be considered a _constant_
                //therfore the derivative is always zero.
                if !ctx.activity.is_active_port(&self, port) {
                    let zero_node = self.emit_zero_for_port(region, port);
                    Ok(zero_node)
                } else {
                    //The last case is active, non-wrt port that is no argument. In that case there should already be the value in our cache
                    if let Some(diffed_port) = ctx.expr_cache.get(&port) {
                        Ok(*diffed_port)
                    } else {
                        Err(AutoDiffError::FwPortUnhandeled(port))
                    }
                }
            }
        }
    }
}
