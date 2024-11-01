/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::collections::VecDeque;

use ahash::AHashSet;

use crate::{edge::LangEdge, nodes::LangNode, NodeRef, Rvsdg};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Finds all [SimpleNodes](crate::nodes::NodeType::Simple) that are directly or indirectly connected to `node` in its parent region.
    ///Returns an empty vector if `node` itself is is not a simple node. Otherwise the vector is guaranteed to only contain simple nodes that are connected to another.
    ///
    /// The resulting vector contains `node`, all predecessors, and all successors that are [SimpleNodes](crate::nodes::NodeType::Simple) in the `node`'s parent region.
    pub fn connected_simple_nodes(&self, node: NodeRef) -> Vec<NodeRef> {
        if !self[node].node_type.is_simple() {
            return Vec::with_capacity(0);
        }
        let parent_region = self[node].parent.unwrap();
        //walk forward all connected nodes, but stop at none simple nodes
        let mut node_set = AHashSet::new();

        let unique_simple_succ = |node| {
            self.unique_dst_nodes(node)
                .into_iter()
                .filter(|n| self[*n].node_type.is_simple())
        };

        let mut stack = unique_simple_succ(node).collect::<VecDeque<_>>();

        while let Some(next) = stack.pop_front() {
            //Ignore seen node. That also prevents us from re-enquing those.
            if node_set.contains(&next) || next == parent_region.node {
                continue;
            }
            node_set.insert(next);
            //extend the forward stack
            stack.extend(unique_simple_succ(next));
        }

        assert!(stack.is_empty());
        //Now do the same for all predecessors
        let unique_simple_src_nodes = |node| {
            self.unique_src_nodes(node)
                .into_iter()
                .filter(|n| self[*n].node_type.is_simple())
        };

        stack.extend(unique_simple_src_nodes(node));
        while let Some(next) = stack.pop_front() {
            if node_set.contains(&next) || next == parent_region.node {
                continue;
            }
            node_set.insert(next);
            stack.extend(unique_simple_src_nodes(next));
        }

        node_set.into_iter().collect()
    }
}
