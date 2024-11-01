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
    edge::{InportLocation, LangEdge},
    nodes::LangNode,
    region::RegionLocation,
    NodeRef, Rvsdg,
};

///Utility that walks the successors of a node in breadth-first style.
///
/// All node ports are traversed only once. So a node can be touched multiple times by the Iterator, but once all
/// input ports are touched, the node won't occur anymore.
///
/// Crosses region boundaries. If the iterator should not leave the region, have a look at [SuccWalkerRegion].
pub struct SuccWalker<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    walked: AHashSet<InportLocation>,
    walker_stack: VecDeque<InportLocation>,

    ctx: &'a Rvsdg<N, E>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> SuccWalker<'a, N, E> {
    pub fn new(ctx: &'a Rvsdg<N, E>, node: NodeRef) -> Self {
        //Init stack
        let stack = ctx.node(node).succ(ctx).collect();
        SuccWalker {
            walked: AHashSet::default(),
            walker_stack: stack,
            ctx,
        }
    }
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for SuccWalker<'a, N, E> {
    type Item = InportLocation;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(n) = self.walker_stack.pop_back() {
            //collect this nodes predecessors
            for succ in self.ctx.node(n.node).succ(self.ctx) {
                if !self.walked.contains(&succ) {
                    self.walker_stack.push_front(succ.clone());
                    self.walked.insert(succ);
                }
            }

            Some(n)
        } else {
            None
        }
    }
}

///Utility that walks the successors of a node in breadth-first style.
///
/// All node ports are traversed only once. So a node can be touched multiple times by the Iterator, but once all
/// input ports are touched, the node won't occur anymore.
///
/// Does not break out of its `bound` region.
pub struct SuccWalkerRegion<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    walked: AHashSet<InportLocation>,
    walker_stack: VecDeque<InportLocation>,
    bound: RegionLocation,

    ctx: &'a Rvsdg<N, E>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> SuccWalkerRegion<'a, N, E> {
    pub fn new(ctx: &'a Rvsdg<N, E>, node: NodeRef, bound: RegionLocation) -> Self {
        //Init stack
        let stack = ctx.node(node).succ(ctx).collect();
        SuccWalkerRegion {
            walked: AHashSet::default(),
            walker_stack: stack,
            ctx,
            bound,
        }
    }
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for SuccWalkerRegion<'a, N, E> {
    type Item = InportLocation;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(n) = self.walker_stack.pop_back() {
            //collect this nodes predecessors
            for succ in self.ctx.node(n.node).succ(self.ctx) {
                //Do not enque bound node, which will effectively
                //also not enque anything _outside_ the bound region.
                if succ.node == self.bound.node {
                    continue;
                }
                if !self.walked.contains(&succ) {
                    self.walker_stack.push_front(succ.clone());
                    self.walked.insert(succ);
                }
            }

            Some(n)
        } else {
            None
        }
    }
}
