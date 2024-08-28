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
    attrib::{AttribLocation, FlagStore},
    edge::{InportLocation, OutportLocation},
    util::abstract_node_type::AbstractNodeType,
    NodeRef, SmallColl,
};

use crate::{alge::ConstantIndex, OptError, Optimizer};

use super::AutoDiff;

pub struct Activity {
    ///All nodes that are _active_ for the given expression.
    ///This is: All nodes that are reachable by any node in
    /// `wrt_trace.
    pub active: FlagStore<bool>,
    ///Of the initial trace from the AutoDiff connected wrt-argument to the actual producer.
    pub wrt_trace: FlagStore<bool>,
    ///All ports that are equivalent to the wrt-input of the
    ///original AutoDiff node.
    ///
    /// Example: If the wrt argument is `a.x.y`, then this are all
    /// nodes that produce `a.x.y` in this region.
    pub wrt_producer: SmallColl<OutportLocation>,
}

impl Activity {
    pub fn is_active(&self, attrib: impl Into<AttribLocation>) -> bool {
        if let Some(state) = self.active.get(&attrib.into()) {
            *state
        } else {
            //TODO: do we need to retrace? In practice this should only be called on the _orginal_ expression
            //      (which is fully explored). But maybe that'll change?
            false
        }
    }

    //True if this port is a wrt producer
    pub fn is_part_of_wrt(&self, port: OutportLocation) -> bool {
        if self.wrt_producer.contains(&port) {
            true
        } else {
            //TODO: do we need to retrace? In practice this should only be called on the _orginal_ expression
            //      (which is fully explored). But maybe that'll change?
            false
        }
    }
}

impl Optimizer {
    ///Explores the `entrypoint`'s expression in relation to its wrt-argument.
    /// Taggs all nodes in the expression that are influenced by the wrt-argument.
    ///
    /// Also enters sub regions and called functions.
    pub fn activity_explorer(&self, entrypoint: NodeRef) -> Result<Activity, OptError> {
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

        let (activity_seed_nodes, wrt_producer) = self.trace_activity_seed(wrt_src);

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
        let mut stack: VecDeque<_> = wrt_producer.iter().cloned().collect();

        while let Some(next) = stack.pop_back() {
            //push port regardless
            //
            //then trace the port's target nodes. For
            //each, if the node is
            // 1. in `preds`
            // 2. not in `active`
            // 3. not a index node that is not in `seeds` (do not add index nodes that are not indexing the right value)
            // -> enque all output ports of the node in stack & push node
            //    into active.

            active.set(next.clone().into(), true);

            for dst in self.graph.outport_dsts(next) {
                if self.is_node_type::<ConstantIndex>(dst.node) {
                    //if this constant index is not an active seed node, ignore it and the predecessors
                    if !activity_seed_nodes.is_set(&dst.node.into()) {
                        continue;
                    }
                }

                //if the node is part of the expression, and, not yet _touched_ by the explorer, handle it
                if preds.contains(&dst.node) && !active.flags.contains_key(&dst.node.into()) {
                    //node is active, and not yet seen, mark as active, and equeue all
                    //output ports
                    self.handel_active_node(dst.node, &mut active, &mut stack);
                }
            }
        }

        Ok(Activity {
            active,
            wrt_trace: activity_seed_nodes,
            wrt_producer,
        })
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
    fn trace_activity_seed(
        &self,
        wrt: OutportLocation,
    ) -> (FlagStore<bool>, SmallColl<OutportLocation>) {
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
        //turns the seed trace to _start_ at the origin, and end at the wrt-arg src.
        seeds.reverse();
        self.propagate_activity_seed(seeds)
    }

    ///Propagates the acivity trace and taggs all nodes that are
    ///equal on a level. Returns flagstore that flags all nodes
    /// and ports on that trace, as well as a list of all wrt-argument producing
    /// nodes, ie. nodes that are equal to the wrt-argument-connected node.
    ///
    /// Example: using a wrt argument `a.x.y`, which indexes a matrix
    /// `a`, for the first column, and that column's second value:
    /// The `trace_activity_seed` will find the origin of `a`, as well as the `.y` and `.x` indexing operations.
    ///
    /// This propagation will tag all `.x` operations that are
    /// directly connected to the `a` value, and then
    /// tag all `.y` operations that are connected to a `a.y`
    /// operation.
    fn propagate_activity_seed(
        &self,
        seed_trace: SmallColl<OutportLocation>,
    ) -> (FlagStore<bool>, SmallColl<OutportLocation>) {
        let mut wrt_related = FlagStore::new();
        let mut end_ports = SmallColl::new();

        //trace from the start
        let start = seed_trace[0];
        //build the index list we have to follow for valid producer-index-ops.
        let index_path = seed_trace[1..]
            .iter()
            .map(|port| {
                if let Some(node) = self.try_unwrap_node::<ConstantIndex>(port.node) {
                    node.access
                } else {
                    panic!("None indexing node in index path!");
                }
            })
            .collect::<SmallColl<_>>();
        //shortcut that needs no tracing, if there is no indexing at all.
        if seed_trace.len() == 1 {
            end_ports.push(start);
            wrt_related.set(start.into(), true);
            return (wrt_related, end_ports);
        } else {
            //seed the trace with all value-connected indexing operations, and add the
            //value itself to wrt-related regardless, and add it, if there is any non-indexing
            //node connected
            wrt_related.set(start.into(), true);
            end_ports.push(start);

            for connected in self.graph.outport_dsts(start) {
                if self.is_node_type::<ConstantIndex>(connected.node) {
                    //trace _the rest_ from the origin part
                    self.trace_for_index_path(
                        connected.node,
                        &index_path,
                        &mut wrt_related,
                        &mut end_ports,
                    );
                }
            }
        }

        (wrt_related, end_ports)
    }

    fn trace_for_index_path(
        &self,
        node: NodeRef,
        index_list: &[usize],
        wrt_related: &mut FlagStore<bool>,
        end_ports: &mut SmallColl<OutportLocation>,
    ) {
        assert!(self.graph[node].outputs().len() == 1);
        let this_index = if let Some(n) = self.try_unwrap_node::<ConstantIndex>(node) {
            n.access
        } else {
            panic!("non indexing in index trace")
        };

        //check if this is a valid index, if not, return without doing anything
        if this_index != index_list[0] {
            return;
        }
        //otherwise, recurse for all connected index nodes, and add this value to end-ports
        //if any non-indexing nodes are connected.
        //add to wrt-related regardless

        wrt_related.set(node.into(), true);

        let mut is_end_port = false;
        for connected in self.graph.outport_dsts(node.output(0)) {
            if self.is_node_type::<ConstantIndex>(connected.node) {
                //recurse, but only if indices are left
                if index_list.len() > 1 {
                    self.trace_for_index_path(
                        connected.node,
                        &index_list[1..],
                        wrt_related,
                        end_ports,
                    );
                }
            } else {
                //tag as endport
                is_end_port = true;
            }
        }

        if is_end_port {
            end_ports.push(node.output(0));
        }
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
