/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::collections::VecDeque;

use ahash::AHashSet;
use rvsdg::{
    attrib::FlagStore,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    util::abstract_node_type::AbstractNodeType,
    NodeRef, SmallColl,
};

use crate::{
    alge::{ConstantIndex, Construct},
    autodiff::AutoDiffError,
    DialectNode, OptError, Optimizer,
};

use super::AutoDiff;

impl Optimizer {
    ///Explores the `entrypoint`'s expression in relation to its wrt-argument.
    /// Taggs all nodes in the expression that are influenced by the wrt-argument.
    ///
    /// Also enters sub regions and called functions.
    pub fn activity_explorer(&self, entrypoint: NodeRef) -> Result<FlagStore<bool>, OptError> {
        // This is a three-stage process.
        // 1. Find the actual value producer that is _active_. The dialects permit none-transforming nodes like _index_

        let wrt_src = self
            .graph
            .inport_src(InportLocation {
                node: entrypoint,
                input: AutoDiff::wrt_input(),
            })
            .unwrap();

        let expr_src = self
            .graph
            .inport_src(InportLocation {
                node: entrypoint,
                input: AutoDiff::expr_input(),
            })
            .unwrap();

        let activity_seed_nodes = self.trace_activity_seed(wrt_src);
        //Find all nodes that are somehow related to the expression.
        let mut preds = self
            .graph
            .walk_predecessors_in_region(expr_src.node)
            .map(|port| port.node)
            .collect::<AHashSet<_>>();
        //NOTE: need to add the expr_src node, since
        //      the iterator only walks the predecessors,
        //      not the node itself.
        preds.insert(expr_src.node);

        //now the idea is to flag all nodes in `preds` that are reachable from any of the activity nodes.
        //the simplest way is to trace all followers of `activity`, and only continue tracing followers, if a node
        //is in `preds`.

        let mut active = FlagStore::new();
        let mut stack: VecDeque<_> = activity_seed_nodes.into_iter().collect();

        while let Some(next) = stack.pop_back() {
            //push port regardless,
            //then trace the port's target nodes. For
            //each, if the node is
            // 1. in `preds`
            // 2. not in `active`
            // -> enque all output ports of the node in stack & push node
            //    into active.

            active.set(next.clone().into(), true);

            for dst in self.graph.outport_dsts(next) {
                if preds.contains(&dst.node) && !active.flags.contains_key(&dst.node.into()) {
                    //node is active, and not yet seen, mark as active, and equeue all
                    //output ports
                    self.handel_active_node(dst.node, &mut active, &mut stack);
                }
            }
        }

        Ok(active)
    }

    fn handel_active_node(
        &self,
        node: NodeRef,
        active: &mut FlagStore<bool>,
        stack: &mut VecDeque<OutportLocation>,
    ) {
        //mark node as active
        active.set(node.into(), true);

        //now handle the activity by node type.
        //
        //For simple nodes, we can just mark all outputs
        //for apply nodes, we can tag all active argument ports of the apply node's active inputs
        //in the λ-region.
        //
        //for Gamma/Theta nodes, we do this: https://arxiv.org/abs/1810.07951
        match self.graph[node].into_abstract() {
            AbstractNodeType::Simple => {
                for outport in self.graph[node].outport_types() {
                    stack.push_front(OutportLocation {
                        node,
                        output: outport,
                    });
                }
            }
            _ => panic!("unhandled activity"),
        }
    }

    ///Traces all nodes that are connected to `wrt` _backwards_, till it ends at either a
    /// Value producing node, or a region-argument
    fn trace_activity_seed(&self, wrt: OutportLocation) -> SmallColl<OutportLocation> {
        let mut seeds = SmallColl::new();
        let mut seen_nodes = AHashSet::new();
        let mut stack = VecDeque::new();
        stack.push_back(wrt);

        while let Some(next) = stack.pop_front() {
            //add the port to the seed ports regardless
            seeds.push(next);

            //If this node was not yet inspected, check if
            //its a producer, or if it needs to terminate this trace.
            if seen_nodes.contains(&next.node) {
                continue;
            } else {
                //add as seen
                seen_nodes.insert(next.node);
            }

            if !self.is_value_producer(next.node) {
                //is not a value producer, so enqueue all predecessors
                for inp in self.graph[next.node].input_srcs(&self.graph) {
                    if let Some(src) = inp {
                        stack.push_back(src);
                    }
                }
            }
        }

        seeds
    }

    fn is_value_producer(&self, node: NodeRef) -> bool {
        //FIXME: While currently correct, this is a _really_ bad way of finding that out :((
        if self.is_node_type::<ConstantIndex>(node) {
            false
        } else {
            true
        }
    }
}
