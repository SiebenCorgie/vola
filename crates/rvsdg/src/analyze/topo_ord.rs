/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use ahash::AHashSet;

use crate::{edge::LangEdge, nodes::LangNode, region::RegionLocation, NodeRef, Rvsdg};

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

    ///Returns the order of all (live) nodes in the region in topological order. This means for any noden
    /// `n` which produces a value for node `m`, `n` will occure before `m`.
    pub fn topological_order_region(&self, region: RegionLocation) -> Vec<NodeRef> {
        let mut forward_order = self.reverse_topological_order_region(region);
        forward_order.reverse();
        forward_order
    }
}
