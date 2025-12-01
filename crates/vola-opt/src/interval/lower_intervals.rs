/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use std::collections::VecDeque;

use ahash::AHashMap;
use rvsdg::{
    edge::OutportLocation, region::RegionLocation, util::abstract_node_type::AbstractNodeType,
    NodeRef,
};
use rvsdg_viewer::View;
use vola_common::{Span, VolaError};

mod arith;
mod buildin;
mod calling;
mod cf;
mod trigonometric;
mod typelevel;

use crate::{
    alge::{
        arithmetic::{BinaryArith, UnaryArith},
        buildin::Buildin,
        trigonometric::Trig,
    },
    interval::IntervalError,
    typelevel::{ConstantIndex, IntervalConstruct, UniformConstruct},
    OptError, Optimizer,
};

/// Pass that replace any interval-operation with _normal_ operations over tuple elements.
///
/// For instance it'll replace an operation `a + b` with `a`/`b` being intervals with `(a.start + b.start, a.end + b.end)`.
///
/// Those rules can become more involved for specific rules here. The main problem is the monotonic property of an operation. If we know it
/// we can, for monoton increasing or decreasing function, trivially apply the transformation (see the `a+b` example).
/// For non monotonic functions however, we need to _know_ (or analyse) extrem points. We do that whenever possible, or fall back
/// to conservative bounds. For instance if the input interval `x` in `sin(x)` is constant, we can produce an exact interval,
/// otherwise we fall-back to `[-1..1]`.
///
/// The pass traverses interval-calculations and substitudes any simple-node output that is an interval (read of type `interval<something>`) with a
/// mapping to its _lowered_ `start` and `end` calculation. For control-flow and function-calls we combine those into the former mentioned tuples (i.e `interval<t>` -> `(t, t)`).
///
/// This translates well to any indexing operation, since they stay essentially the same.
pub struct LowerIntervals<'opt> {
    pub(crate) optimizer: &'opt mut Optimizer,

    ///Maps a interval port to the already resolved lower and upper bound of some _primitive_ type
    pub(crate) mapping: AHashMap<OutportLocation, (OutportLocation, OutportLocation)>,
    pub(crate) region_queue: VecDeque<RegionLocation>,
}

impl<'opt> LowerIntervals<'opt> {
    pub fn setup(optimizer: &'opt mut Optimizer) -> Self {
        let mut region_queue = VecDeque::new();
        region_queue.push_front(optimizer.graph.toplevel_region());
        LowerIntervals {
            optimizer,
            mapping: AHashMap::default(),
            region_queue,
        }
    }

    pub fn execute(mut self) -> Result<(), VolaError<OptError>> {
        #[cfg(feature = "log")]
        log::info!("Lower Intervals");

        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("DUMP_PRE_INTERVAL_LOWER").is_ok()
        {
            self.optimizer
                .push_debug_state(&format!("pre-interval-lower"));
        }

        //Execution of this lowering simply iterates the live-code
        // in topological order, for any node that produces an interval, we call
        // the _handle_ routine.
        //
        // Two observations: Since we iterate the _before transformation_ state, we won't have
        // to consider _created_ nodes. Since we iterate it in topological order, we can assume that, for any node
        // that consumes some interval, an equivalent lowered representation already exists in `mapping`.
        //
        // Since we are building the live-node/topo-ord for each region only once they are touched, we
        // can modify interfaces of λ-nodes _before_ their region is actually lowered.

        while let Some(next_region) = self.region_queue.pop_back() {
            //discover live nodes and order them
            let live_in_region = self.optimizer.graph.live_nodes_in_region(next_region);
            let mut topoord = self
                .optimizer
                .graph
                .topological_order_nodes(live_in_region.into_iter());

            //Now, pre-handle any none_simple node, i.e. Control-flow and inter-procedural stuff
            // We must do that, because this changes the live-nodes in this region.
            let mut changed_any = false;
            for node in &topoord {
                let pre_transform_changed_any = match self.optimizer.graph[*node].into_abstract() {
                    AbstractNodeType::Lambda => self.lower_lambda(*node)?,
                    AbstractNodeType::Theta => self.lower_theta(*node)?,
                    AbstractNodeType::Gamma => self.lower_gamma(*node)?,
                    _other => false,
                };
                if pre_transform_changed_any {
                    changed_any = true;
                }
            }

            //now, if any of the live-nodes changed, re-do the topo-ord since some of the old indexing operations
            // might have become invalid
            if changed_any {
                let live_in_region = self.optimizer.graph.live_nodes_in_region(next_region);
                topoord = self
                    .optimizer
                    .graph
                    .topological_order_nodes(live_in_region.into_iter());
            }

            //TODO: Maybe we want to use the per-port liveness analysis here? There we could _not process_ dead
            //      outputs. But not sure if the additional granularity is really needed.
            // NOTE: This _might_ enqueue additional regions, if any of the non-simple nodes decide so while
            //       being lowered.
            for node in topoord {
                match self.optimizer.graph[node].into_abstract() {
                    AbstractNodeType::Simple => self.handle_simple_node(node)?,
                    AbstractNodeType::Lambda
                    | AbstractNodeType::Gamma
                    | AbstractNodeType::Theta => {
                        //Those should be pre-transformed, but make sure for sanity reasons.
                        assert!(!self.has_interval_in_or_out(node));
                        for region_index in 0..self.optimizer.graph[node].regions().len() {
                            assert!(
                                !self.has_interval_in_region_interface(RegionLocation {
                                    node,
                                    region_index
                                }),
                                "{} / {} had interval in interface!",
                                node,
                                self.optimizer.graph[node].name()
                            );
                        }
                    }
                    AbstractNodeType::Apply => self.lower_apply(node)?,
                    AbstractNodeType::Omega => {
                        //This we can safely ignore :)
                    }
                    other => {
                        return Err(VolaError::new(
                            IntervalError::UnsupportedOp(format!("Unexpected node type: {other}"))
                                .into(),
                        ))
                    }
                }
            }
        }

        //TODO: don't do that once a fix for #41 lands.
        self.optimizer.inline_all().map_err(|e| VolaError::new(e))?;

        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("DUMP_POST_INTERVAL_LOWER").is_ok()
        {
            self.optimizer
                .push_debug_state(&format!("post-interval-lower"));
        }

        Ok(())
    }

    ///Handles any simple node
    fn handle_simple_node(&mut self, node: NodeRef) -> Result<(), VolaError<OptError>> {
        //if the node produces any interval edges, handle it
        for port in self.optimizer.graph.outports(node) {
            if self
                .optimizer
                .get_out_type_mut(port)
                .map_err(|e| e.to_error())?
                .is_interval()
            {
                self.handle_value(port)?
            }
        }

        //If this node is a interval-indexing operation, we have the special case, where
        // we can now flatten that index into a specific value.
        if let Some(iindex) = self.optimizer.try_unwrap_node::<ConstantIndex>(node) {
            let index = iindex.access;
            let is_indexing_interval = self
                .optimizer
                .get_in_type_mut(node.input(0))
                .unwrap()
                .is_interval();
            //If this is actually indexing an interval, call the unwraping handler
            if is_indexing_interval {
                self.lower_constant_index(node, index)?;
            }
        }

        Ok(())
    }

    ///Replaces the interval of `value` with the tuple of `(start, end).
    ///
    /// Assumes that, iff any input to this value's node is a interval, that it has been associated with a lowered interval representation
    /// already.
    fn handle_value(&mut self, value: OutportLocation) -> Result<(), VolaError<OptError>> {
        //If this port is already in the mapping, bail.
        if self.mapping.contains_key(&value) {
            return Ok(());
        }

        if !self.optimizer.graph[value.node].node_type.is_simple() {
            //TODO: Implement pulling out such values from a region
            // The idea is, for control-flow to _just create_ new ports, that represent
            // the flattened values, put them into the map, and the recursively pre-fetch the interval-connected
            // port. Once its done, special-handle the connection _out_ of the CF node.
            //
            // For lambda/apply nodes we can probably just copy the lambda, specialize it for the interval (use
            // the mapping to reuse/signal that for the lambda-definition port), and replace the apply node,
            // which should now produce two values.
            panic!("non-simple node");
        }

        let span = self
            .optimizer
            .find_span(value.node)
            .unwrap_or(Span::empty());
        let region = self.optimizer.graph[value.node].parent.unwrap();

        //Similar to the AutoDiff dispatch we can statically dispatch the type-tag of the dyn-dispatched
        // node, so we have to traverse like this atm.
        if let Some(binarith) = self.optimizer.try_unwrap_node::<BinaryArith>(value.node) {
            let op = binarith.op;
            return self.lower_binary_arith(region, span.clone(), value.node, op);
        }
        if let Some(unaryarith) = self.optimizer.try_unwrap_node::<UnaryArith>(value.node) {
            let op = unaryarith.op;
            return self.lower_unary_arith(region, span.clone(), value.node, op);
        }
        if let Some(trig) = self.optimizer.try_unwrap_node::<Trig>(value.node) {
            let op = trig.op;
            return self.lower_trig(region, span.clone(), value.node, op);
        }

        if let Some(buildin) = self.optimizer.try_unwrap_node::<Buildin>(value.node) {
            let op = buildin.op.clone();
            return self.lower_buildin(region, span.clone(), value.node, op);
        }

        //BreakOut to typelevel specific code. Those are a little different to the _actual_ operations
        if let Some(_ic) = self
            .optimizer
            .try_unwrap_node::<IntervalConstruct>(value.node)
        {
            return self.lower_interval_contruct(region, span.clone(), value.node);
        }
        if let Some(_ic) = self
            .optimizer
            .try_unwrap_node::<IntervalConstruct>(value.node)
        {
            return self.lower_interval_contruct(region, span.clone(), value.node);
        }

        if let Some(_uc) = self
            .optimizer
            .try_unwrap_node::<UniformConstruct>(value.node)
        {
            return self.lower_uniform_construct(region, span.clone(), value.node);
        }

        Err(
            VolaError::new(OptError::Interval(IntervalError::UnsupportedOp(
                self.optimizer.graph[value.node].name(),
            )))
            .with_error(span, "here"),
        )
    }
}
