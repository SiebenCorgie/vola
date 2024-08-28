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
    edge::{InportLocation, OutportLocation},
    util::abstract_node_type::AbstractNodeType,
    NodeRef, SmallColl,
};

use crate::{alge::ConstantIndex, OptError, Optimizer};

use super::AutoDiff;

pub struct Activity {
    ///All nodes or ports from which we _know_ the activity state.
    pub active: FlagStore<bool>,
    ///All ports that are equivalent to the wrt-input of the
    ///original AutoDiff node.
    ///
    /// Example: If the wrt argument is `a.x.y`, then this are all
    /// nodes that produce `a.x.y` in this region.
    pub wrt_producer: SmallColl<OutportLocation>,
}

impl Activity {
    ///Tries to read the activity state of `node`. Returns None if the node does not
    /// appear in the activity list.
    pub fn is_active(&self, node: NodeRef) -> Option<bool> {
        if let Some(state) = self.active.get(&node.into()) {
            Some(*state)
        } else {
            None
        }
    }

    ///Returns true, if this `port` produces the specific wrt-value, or an aggregate type that contains
    /// the wrt value
    pub fn is_wrt_producer(&self, port: &OutportLocation) -> bool {
        self.wrt_producer.contains(port)
    }

    //Tries to recover the activity state of this port, and if it can't, traces it
    pub fn is_active_port(&mut self, opt: &Optimizer, port: OutportLocation) -> bool {
        if let Some(state) = self.active.get(&port.into()) {
            *state
        } else {
            //NOTE: Trace the port's node, and overwrite the querried node afterwards
            let port_active = self.is_node_active(opt, port.node);
            self.active.set(port.into(), port_active);
            port_active
        }
    }

    pub fn is_node_active(&mut self, opt: &Optimizer, node: NodeRef) -> bool {
        if let Some(state) = self.active.get(&node.into()) {
            *state
        } else {
            self.trace_node_activity(opt, node)
        }
    }

    //traces all predecessors of this node. Each trace ends at
    // 1. Finding cached value for node.
    // 2. at wrt-producer (and recursively taggs predecessors all as _active_)
    // 3. at region argument
    fn trace_node_activity(&mut self, opt: &Optimizer, node: NodeRef) -> bool {
        //TODO: Handle nodes with sub regions, Gamma&Theta nodes mostly...

        //by definition, this node is active, if any of its predecessors is active
        let mut any_active = false;
        for pred in opt.graph[node].pred(&opt.graph) {
            if self.is_active_port(opt, pred) {
                any_active = true;
            }
        }

        //now handle the activity by node type.
        //
        //For simple nodes, we can just mark all outputs
        //for apply nodes, we can tag all active argument ports of the apply node's active inputs
        //in the λ-region.
        //
        //for Gamma/Theta nodes, we do this: https://arxiv.org/abs/1810.07951

        //Overwrite this node, and all ports
        self.active.set(node.into(), any_active);
        match opt.graph[node].into_abstract() {
            AbstractNodeType::Simple => {
                for output in opt.graph[node].outport_types() {
                    self.active
                        .set(OutportLocation { node, output }.into(), any_active);
                }
            }
            _ => panic!("unhandled activity"),
        }

        any_active
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

        //Preset the activity state with all active seed nodes
        /*
                println!("Seeding with: ");
                for (k, s) in &activity_seed_nodes.flags {
                    println!("    {k:?} : {s}");
                }
        */
        //NOTE: right now we just build the activity for all nodes that are predecessors to thea expression
        //
        //TODO: We _could_ also build that thing lazyly. At least the Activity _thingy_ would permit that. But
        //      In that case we'd have to update all passes that _assume_ all active nodes set to querry instead.

        let mut predecerssors = self
            .graph
            .walk_predecessors_in_region(expr_src.node)
            .collect::<Vec<_>>();
        //Push ourselfs into that node as well
        predecerssors.push(expr_src);

        let mut activity = Activity {
            active: activity_seed_nodes,
            wrt_producer,
        };

        //Now just querry the _activity_ state once for each output, which lets the helper
        //build that state.
        for pred in self
            .graph
            .walk_predecessors_in_region(expr_src.node)
            .chain([expr_src].into_iter())
        {
            let _val = activity.is_active_port(self, pred);
        }

        Ok(activity)
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
            //blacklist this index
            wrt_related.set(node.into(), false);
            //and all its outputs
            for output in self.graph[node].outport_types() {
                wrt_related.set(OutportLocation { node, output }.into(), false);
            }
            return;
        }
        //otherwise, recurse for all connected index nodes, and add this value to end-ports
        //if any non-indexing nodes are connected.
        //add to wrt-related regardless
        wrt_related.set(node.into(), true);
        wrt_related.set(node.output(0).into(), true);

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
