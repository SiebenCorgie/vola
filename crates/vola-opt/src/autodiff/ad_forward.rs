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

use ahash::AHashMap;
use rvsdg::{
    edge::{InportLocation, OutportLocation},
    region::RegionLocation,
    util::abstract_node_type::AbstractNodeType,
    NodeRef,
};
use vola_common::{error::error_reporter, report, Span};

use crate::{autodiff::AutoDiff, OptEdge, OptError, Optimizer};

use super::{activity::Activity, AutoDiffError};

type DiffExprCache = AHashMap<OutportLocation, OutportLocation>;

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

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_FWAD_LINEARIZED").is_ok() {
            self.push_debug_state(&format!("fw-autodiff-{entrypoint}-linearized"));
        }

        for entrypoint in &entrypoints {
            self.canonicalize_for_ad(*entrypoint)?;
        }

        //do type derive and dead-node elemination in order to have
        // a) clean DAG (faster / easier transformation)
        // b) type information to emit correct zero-nodes

        let region = self.graph[entrypoint].parent.unwrap();
        self.graph.dne_region(region)?;
        let span = self.find_span(region.into()).unwrap_or(Span::empty());
        self.derive_region(region, span)?;

        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("DUMP_FWAD_CANONICALIZED").is_ok()
        {
            self.push_debug_state(&format!("fw-autodiff-{entrypoint}-canonicalized"));
        }

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
        /*
        println!("Active Nodes: ");
        for active in activity.active.flags.keys() {
            if let AttribLocation::Node(n) = active {
                println!("    {n}");
            }
        }

        println!("WRT-Outputs");
        for o in &activity.wrt_producer {
            println!("    {o:?}");
        }
        */

        let mut expr_cache = DiffExprCache::new();
        let diffed_src =
            self.fwad_handle_node(region, expr_src.node, &activity, &mut expr_cache)?;

        //replace the differential_value and the autodiff node
        self.graph.replace_node_uses(diffnode, diffed_src.node)?;

        Ok(())
    }

    ///The push-forward recursive differentiation handler.
    ///The idea is to apply all auto-diff rules _simply_ recursively.
    ///
    /// On paper this might generate a lot of redundant code/nodes. In practice
    /// a common-node-elemination pass can take care of that.
    ///
    ///
    /// The _nice_ part about the recursive pattern is, that we can apply all
    /// rules simply by traversing the dependecy DAG of a node an rewriting said node
    /// according to the rules.
    ///
    /// So given a node N = f(g(x)), that needs to apply the chain rule, we can simply
    /// split the tree into f'(g(x)) (where g(x) is the original _rest_ of the DAG), and
    /// g'(x), where we just call fwad_handle_node(g) to recursively process _the rest_.
    fn fwad_handle_node(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
        activity: &Activity,
        expr_cache: &mut DiffExprCache,
    ) -> Result<OutportLocation, OptError> {
        //Any node that is not active can be considered a _constant_
        //therfore the derivative is always zero.
        if !activity.is_active(node).unwrap_or(false) {
            let zero_node = self.emit_zero_for_node(region, node);
            return Ok(zero_node);
        }

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
                    "alge" => self.fwd_handle_alge_node(region, node, activity, expr_cache),
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
                        Ok(self.emit_zero_for_node(region, node))
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
                todo!("Function calls not yet implemented!")
            }
            other => return Err(AutoDiffError::FwadUnexpectedNodeType(other).into()),
        }
    }

    ///Handles any _potentually_ differentiatable node
    fn fwd_handle_alge_node(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
        activity: &Activity,
        expr_cache: &mut DiffExprCache,
    ) -> Result<OutportLocation, OptError> {
        //This is the point, where we handle the dispatch of the chain rule.
        //
        //Note that things like cross-product-rule, quotient-rule or dot-product-rule are
        //handeled by the respective node.
        //
        //however, dispatching the chain-rule can / must be done

        let (result, postdiffs) = self.build_diff_value(region, node, activity)?;
        for (post_diff_src, targets) in postdiffs {
            //Try to reuse expr-cache, otherwise build new subexpression
            let diff_expr_src = if let Some(cached) = expr_cache.get(&post_diff_src) {
                *cached
            } else {
                let diffvalue =
                    self.fwad_handle_port(region, post_diff_src, activity, expr_cache)?;
                expr_cache.insert(post_diff_src, diffvalue);
                diffvalue
            };
            for t in targets {
                self.graph
                    .connect(diff_expr_src, t, OptEdge::value_edge_unset())?;
            }
        }

        Ok(result)
    }

    //Small indirection that lets us catch active & WRT-Ports. This is mostly used if
    //a region-argument is also a wrt-argument, to end the recursion, or emit zero if a region's non active
    //argument is used.
    fn fwad_handle_port(
        &mut self,
        region: RegionLocation,
        port: OutportLocation,
        activity: &Activity,
        expr_cache: &mut DiffExprCache,
    ) -> Result<OutportLocation, OptError> {
        //A port that is part of the respect_to chain always derives to 1.0
        if activity.is_wrt_producer(&port) {
            //NOTE: _handling_ a wrt port means splatting it with 1.0
            //      Math wise this is handling a expression:
            //      f(x) = x;
            //      where f'(x) = f'(x) = 1*x^0 = 1
            //
            //      For vectors this means initing _the-right_ index with one, same for matrix.
            //      We have that information from the activity trace, which if why we'll use that.
            //println!("Getting {port:?} : {:?}", activity.wrt_producer.get(&port));
            Ok(activity.build_diff_init_value_for_wrt(self, region, port))
        } else {
            if port.output.is_argument() && !activity.get_outport_active(port) {
                Ok(self.emit_zero_for_port(region, port))
            } else {
                //Otherwise we need to recurse
                self.fwad_handle_node(region, port.node, activity, expr_cache)
            }
        }
    }
}
