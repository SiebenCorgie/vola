/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::collections::VecDeque;

use ahash::AHashSet;

use crate::{
    edge::{LangEdge, OutportLocation},
    nodes::LangNode,
    region::RegionLocation,
    NodeRef, Rvsdg,
};

///Utility that walks the predecessors of a node in breadth-first style.
///
/// All node ports are traversed only once. So a node can be touched multiple times by the Iterator, but once all
/// output ports are touched, the node won't occur anymore.
///
/// Note that this iterator traverses region boundaries. So for instance if a node is indirectly connected to a context variable
/// of some λ-node, the iterator will break out of thea λ-node's body, and traverse the outside region.
pub struct PredWalker<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    walked: AHashSet<OutportLocation>,
    walker_stack: VecDeque<OutportLocation>,

    ctx: &'a Rvsdg<N, E>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> PredWalker<'a, N, E> {
    pub fn new(ctx: &'a Rvsdg<N, E>, node: NodeRef) -> Self {
        //Init stack
        let stack = ctx.node(node).pred(ctx).collect();
        PredWalker {
            walked: AHashSet::default(),
            walker_stack: stack,
            ctx,
        }
    }
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for PredWalker<'a, N, E> {
    type Item = OutportLocation;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(n) = self.walker_stack.pop_back() {
            //collect this nodes predecessors
            for pred in self.ctx.node(n.node).pred(self.ctx) {
                if !self.walked.contains(&pred) {
                    self.walker_stack.push_front(pred.clone());
                    self.walked.insert(pred.clone());
                }
            }

            Some(n)
        } else {
            None
        }
    }
}

///Utility that walks the predecessors of a node in breadth-first style. Stops at the region's boundaries of the seeding node.
///
/// If you don't want that, have a look at [PredWalker].
///
/// All node ports are traversed only once. So a node can be touched multiple times by the Iterator, but once all
/// output ports are touched, the node won't occur anymore.
pub struct PredWalkerRegion<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    walked: AHashSet<OutportLocation>,
    walker_stack: VecDeque<OutportLocation>,
    region_boundary_node: NodeRef,
    ctx: &'a Rvsdg<N, E>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> PredWalkerRegion<'a, N, E> {
    //NOTE: assumes that `node` has a parent, so is not the OmegaNode.
    pub fn new(ctx: &'a Rvsdg<N, E>, node: NodeRef) -> Self {
        let region_boundary_node = ctx.node(node).parent.unwrap().node;
        //Init stack
        let stack = ctx.node(node).pred(ctx).collect();
        PredWalkerRegion {
            walked: AHashSet::default(),
            walker_stack: stack,
            ctx,
            region_boundary_node,
        }
    }
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for PredWalkerRegion<'a, N, E> {
    type Item = OutportLocation;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(n) = self.walker_stack.pop_back() {
            //collect this nodes predecessors, but only if the node is _not_ the region_boundary_node.
            if n.node != self.region_boundary_node {
                for pred in self.ctx.node(n.node).pred(self.ctx) {
                    if !self.walked.contains(&pred) {
                        self.walker_stack.push_front(pred.clone());
                        self.walked.insert(pred.clone());
                    }
                }
            }
            Some(n)
        } else {
            None
        }
    }
}

///Walks all predecessor nodes once. Compared to [PredWalker] and [PredWalkerRegion] this is a little bit faster, since
///each node is only visited once, compared to once-per-output.
///
///
/// Does **not** walk the seeding node.
pub struct PredWalkerNodes<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    walked: AHashSet<NodeRef>,
    walker_stack: VecDeque<NodeRef>,
    ctx: &'a Rvsdg<N, E>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> PredWalkerNodes<'a, N, E> {
    pub fn new(ctx: &'a Rvsdg<N, E>, node: NodeRef) -> Self {
        let mut stack = VecDeque::new();
        //Seed walker with all predecessors
        for pred in ctx.unique_src_nodes(node) {
            stack.push_back(pred);
        }
        PredWalkerNodes {
            ctx,
            walked: AHashSet::new(),
            walker_stack: stack,
        }
    }
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for PredWalkerNodes<'a, N, E> {
    type Item = NodeRef;
    fn next(&mut self) -> Option<Self::Item> {
        //shortcut for empty stack
        if self.walker_stack.len() > 0 {
            //now pop stack values till we find an unseen node
            while let Some(next) = self.walker_stack.pop_front() {
                if self.walked.contains(&next) {
                    continue;
                }

                //found unseen, push all unique predecessors and return this node
                self.walked.insert(next);
                for pred in self.ctx.unique_src_nodes(next) {
                    //Only push unseen, otherwise we'd needlessly grow the stack
                    if !self.walked.contains(&pred) {
                        self.walker_stack.push_back(pred)
                    }
                }
                return Some(next);
            }
            //Consumed the whole stack, but didn't find an unseen node
            None
        } else {
            None
        }
    }
}

///Walks all predecessor nodes once. Compared to [PredWalker] and [PredWalkerRegion] this is a little bit faster, since
///each node is only visited once, compared to once-per-output.
///
/// Stops at the given `region` boundary.
///
/// Does **not** walk the seeding node.
pub struct PredWalkerNodesRegion<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    walked: AHashSet<NodeRef>,
    region_boundary: RegionLocation,
    walker_stack: VecDeque<NodeRef>,
    ctx: &'a Rvsdg<N, E>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> PredWalkerNodesRegion<'a, N, E> {
    pub fn new(ctx: &'a Rvsdg<N, E>, node: NodeRef, boundary: RegionLocation) -> Self {
        let mut stack = VecDeque::new();
        //Seed walker with all predecessors
        for pred in ctx.unique_src_nodes(node) {
            //do not push boundary node
            if pred == boundary.node {
                continue;
            }
            stack.push_back(pred);
        }
        PredWalkerNodesRegion {
            ctx,
            region_boundary: boundary,
            walked: AHashSet::new(),
            walker_stack: stack,
        }
    }
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator
    for PredWalkerNodesRegion<'a, N, E>
{
    type Item = NodeRef;
    fn next(&mut self) -> Option<Self::Item> {
        //shortcut for empty stack
        if self.walker_stack.len() > 0 {
            //now pop stack values till we find an unseen node
            while let Some(next) = self.walker_stack.pop_front() {
                if self.walked.contains(&next) {
                    continue;
                }

                //found unseen, push all unique predecessors and return this node
                self.walked.insert(next);
                for pred in self.ctx.unique_src_nodes(next) {
                    //Avoid pushing region boundary node
                    if pred == self.region_boundary.node {
                        continue;
                    }
                    //Only push unseen, otherwise we'd needlessly grow the stack
                    if !self.walked.contains(&pred) {
                        self.walker_stack.push_back(pred)
                    }
                }
                return Some(next);
            }
            //Consumed the whole stack, but didn't find an unseen node
            None
        } else {
            None
        }
    }
}
