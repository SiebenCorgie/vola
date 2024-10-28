/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::collections::VecDeque;

use ahash::{AHashMap, AHashSet};

use crate::{
    edge::{InportLocation, LangEdge, OutportLocation},
    nodes::LangNode,
    region::RegionLocation,
    NodeRef, Rvsdg, SmallColl,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Returns the order of all (live) nodes in the region in revese topological order. This means for any noden
    /// `n` which produces a value for node `m`, `m` will occure before `n`.
    pub fn reverse_topological_order_region(&self, regionloc: RegionLocation) -> Vec<NodeRef> {
        //TODO: Do some nice iterator intead. But I'm not sure
        //      if we only want to iterate live nodes, or all.

        let region = self.region(&regionloc).unwrap();
        let mut seen_nodes = AHashSet::with_capacity(region.nodes.len());
        let mut forward_order = Vec::with_capacity(region.nodes.len());

        for residx in 0..region.results.len() {
            if let Some(src) = region.result_src(self, residx) {
                if src.node == regionloc.node {
                    continue;
                }

                if !seen_nodes.contains(&src.node) {
                    forward_order.push(src.node);
                    seen_nodes.insert(src.node);
                    for n in self.walk_predecessors_in_region(src.node) {
                        if n.node == regionloc.node {
                            continue;
                        }

                        assert!(self.node(n.node).parent == Some(regionloc));
                        if !seen_nodes.contains(&n.node) {
                            forward_order.push(n.node);
                            seen_nodes.insert(n.node);
                        }
                    }
                }
            }
        }

        forward_order
    }

    ///Returns a unique set of nodes that are connected to the inputs of this node.
    pub fn unique_src_nodes(&self, node: NodeRef) -> SmallColl<NodeRef> {
        let mut known = SmallColl::new();
        for edge in self
            .node(node)
            .inputs()
            .iter()
            .filter_map(|port| port.edge.clone())
        {
            let src = self.edge(edge).src();
            if !known.contains(&src.node) {
                known.push(src.node)
            }
        }

        known
    }

    ///Returns a unique set of source ports that are connected to the inputs of this node.
    pub fn unique_src_ports(&self, node: NodeRef) -> SmallColl<OutportLocation> {
        let mut known = SmallColl::new();
        for edge in self
            .node(node)
            .inputs()
            .iter()
            .filter_map(|port| port.edge.clone())
        {
            let src = self.edge(edge).src();
            if !known.contains(src) {
                known.push(*src)
            }
        }

        known
    }

    ///Returns a unique set of nodes that are connected to the outputs of this node.
    pub fn unique_dst_nodes(&self, node: NodeRef) -> SmallColl<NodeRef> {
        let mut known = SmallColl::new();
        for edge in self
            .node(node)
            .outputs()
            .iter()
            .map(|port| port.edges.iter())
            .flatten()
        {
            let dst = self.edge(*edge).dst();
            if !known.contains(&dst.node) {
                known.push(dst.node)
            }
        }

        known
    }

    ///Returns a unique set of destination ports that are connected to the outputs of this node.
    pub fn unique_dst_ports(&self, node: NodeRef) -> SmallColl<InportLocation> {
        let mut known = SmallColl::new();
        for edge in self
            .node(node)
            .outputs()
            .iter()
            .map(|port| port.edges.iter())
            .flatten()
        {
            let dst = self.edge(*edge).dst();
            if !known.contains(dst) {
                known.push(*dst)
            }
        }

        known
    }
    ///Returns `nodes` ordered topological. This means for any node
    /// `n` which produces a value for node `m`, `n` will occure before `m`.
    /// If `n` and `m` are in disjunct graphs, no guarantee over which comes first is given.
    pub fn topological_order_nodes(&self, nodes: impl Iterator<Item = NodeRef>) -> Vec<NodeRef> {
        //NOTE: we use Kahn's algorithm at the moment. If this ever bottlenecks, there are
        //      parallel algorithms.
        //NOTE: We don't work on the Nodes n, but the ports p. So we seed the
        //      search with all argument ports and with all output ports of nodes, that don't have
        //      an input.

        let mut stack_follower = VecDeque::new();
        let mut node_edges = AHashMap::default();

        let mut node_count = 0;
        for node in nodes {
            node_count += 1;
            //sort all nodes with no input, or where all inputs are the region-node (a argument)
            //into the stack as initializing nodes.
            //For the rest, build the edge list and sort them into the _m_ list.
            let parent_region = self[node].parent.unwrap_or(self.toplevel_region());
            let filtered_srcs = self
                .unique_src_nodes(node)
                .into_iter()
                .filter(|n| *n != parent_region.node)
                .collect::<SmallColl<_>>();

            if filtered_srcs.len() == 0 {
                let follower = self
                    .unique_dst_nodes(node)
                    .into_iter()
                    .filter(|n| *n != parent_region.node)
                    .collect();
                stack_follower.push_back((node, follower));
                //also add to node_edges, so we can latter check if a node is in the `nodes`
                //list
                node_edges.insert(node, SmallColl::new());
            } else {
                node_edges.insert(node, filtered_srcs);
            }
        }

        let mut l = Vec::with_capacity(node_count);

        while let Some((popped, follower)) = stack_follower.pop_front() {
            //ignore the region node
            let parent_region = self[popped].parent.unwrap_or(self.toplevel_region());
            //if popped == parent_region.node {
            //    continue;
            //}

            l.push(popped);

            //remove popped from all followers
            for f in follower {
                //Ignore followers that are not in the initial list
                if !node_edges.contains_key(&f) {
                    continue;
                }

                //Remove the edge to the popped element
                let must_be_enqued = if let Some(ne) = node_edges.get_mut(&f) {
                    ne.retain(|n| *n != popped);

                    ne.len() == 0
                } else {
                    panic!("Should not happen on {} in region {parent_region:?}!", f);
                };
                //if f has no incoming edges left, add to stack
                if must_be_enqued {
                    let preds = node_edges.remove(&f).unwrap();
                    assert!(preds.len() == 0);
                    let follower: SmallColl<_> = self
                        .unique_dst_nodes(f)
                        .into_iter()
                        .filter(|n| *n != parent_region.node)
                        .collect();
                    stack_follower.push_back((f, follower));
                }
            }
        }

        l
    }
    ///Returns the order of all nodes in the region in topological order. This means for any node
    /// `n` which produces a value for node `m`, `n` will occure before `m`.
    pub fn topological_order_region(&self, region: RegionLocation) -> Vec<NodeRef> {
        self.topological_order_nodes(self[region].nodes.iter().cloned())
    }
}
