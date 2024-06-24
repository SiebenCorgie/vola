/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! RVSDG analyzis tools
//!
//! Provides several tools to analyse an RVSDG. Including walker utilities
//! that make gathering information easy, and finding dependencies or definitions of nodes.

use std::collections::VecDeque;

use ahash::{AHashMap, AHashSet};

mod region_walker;
pub use region_walker::RegionWalker;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::LangNode,
    region::RegionLocation,
    util::Path,
    NodeRef, Rvsdg, SmallColl,
};

use self::region_walker::RegionLocationWalker;

///Utility that walks the predecessors of a node in breadth-first style.
///
/// All node ports are traversed only once. So a node can be touched multiple times by the Iterator, but once all
/// output ports are touched, the node won't occur anymore.
pub struct PredWalker<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    walked: AHashSet<OutportLocation>,
    walker_stack: VecDeque<OutportLocation>,

    ctx: &'a Rvsdg<N, E>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> PredWalker<'a, N, E> {
    fn new(ctx: &'a Rvsdg<N, E>, node: NodeRef) -> Self {
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

///Utility that walks the successors of a node in breadth-first style.
///
/// All node ports are traversed only once. So a node can be touched multiple times by the Iterator, but once all
/// input ports are touched, the node won't occur anymore.
pub struct SuccWalker<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    walked: AHashSet<InportLocation>,
    walker_stack: VecDeque<InportLocation>,

    ctx: &'a Rvsdg<N, E>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> SuccWalker<'a, N, E> {
    fn new(ctx: &'a Rvsdg<N, E>, node: NodeRef) -> Self {
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

///An iterator that emits all reachable nodes for the entrypoint of a graph. The iterator is breadth-first and top down style.
///
/// _Reachable_ is defined as any node, that can be reached from any of the exported nodes _upwards_ the graph (consumer -> producer direction).
///
/// _Breadth-First_ means, that, for any node n first all directly connected nodes are traversed, before any 1st-degree indirectly connected nodes are traversed, then the 2nd-degree etc..
///
/// _Top-Down_ means, that first all nodes of a region are traversed breadth first, before any discovered sub-regions (of structural nodes) are traversed.
pub struct ReachableWalker<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a Rvsdg<N, E>,
    ///All nodes we have already seen (for the first time)
    walked: AHashSet<NodeRef>,
    ///Waiting sub regions (models the top-down way)
    waiting_regions: VecDeque<(NodeRef, usize)>,
    ///Waiting nodes, modeling the breadth-first traversal
    waiting_nodes: VecDeque<NodeRef>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for ReachableWalker<'a, N, E> {
    type Item = NodeRef;
    fn next(&mut self) -> Option<Self::Item> {
        //shortcut for empty iterators
        if self.waiting_nodes.is_empty() && self.waiting_regions.is_empty() {
            return None;
        }

        //Check if we have to pop a region to get the next node
        if self.waiting_nodes.is_empty() {
            //no nodes left, try to pop a new region, and init the waiting_nodes with that regions first-degree reachable
            // nodes. We do that in a loops, since a region could have no connected nodes
            while self.waiting_nodes.is_empty() && !self.waiting_regions.is_empty() {
                if let Some((region_owner, regidx)) = self.waiting_regions.pop_back() {
                    let reg = &self.ctx.node(region_owner).regions()[regidx];
                    for regres in reg.results.iter() {
                        if let Some(edg) = regres.edge {
                            //if an edge is connected to result, push that on the formerly empty waiting_nodes stack
                            let edge = self.ctx.edge(edg);
                            if !self.walked.contains(&edge.src.node) {
                                self.walked.insert(edge.src.node);
                                self.waiting_nodes.push_front(edge.src.node);
                            }
                        }
                    }
                }
            }

            //This is the case that we poped all regions above, but still didn't find anything
            if self.waiting_nodes.is_empty() && self.waiting_regions.is_empty() {
                return None;
            }
        }

        debug_assert!(
            !self.waiting_nodes.is_empty(),
            "node stack shouldn't be empty at this point"
        );

        //Try to pop the next node from the stack
        if let Some(next_node) = self.waiting_nodes.pop_back() {
            //if we've found a new node, check if it has any regions. If so push them on the regions stack. Then collect all not-yet-visited connected nodes.
            // then return
            for (reg_idx, _) in self.ctx.node(next_node).regions().iter().enumerate() {
                //for each region, push our self as the region owner, as well as the region index.
                self.waiting_regions.push_front((next_node, reg_idx));
            }

            //push all inputs we haven't seen yet.
            for inp in self.ctx.node(next_node).inputs() {
                if let Some(edg) = inp.edge {
                    let edge = self.ctx.edge(edg);
                    if !self.walked.contains(&edge.src.node) {
                        self.walked.insert(edge.src.node);
                        self.waiting_nodes.push_front(edge.src.node);
                    }
                }
            }

            Some(next_node)
        } else {
            panic!("Invalid ReachableIterator state!");
        }
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Traverses context-variable boundaries of inter-procedural nodes as well as entry-variable boundaries
    /// of γ-Nodes and input/argument boundaries of theta-nodes, until it finds the producing port of `input`.
    ///
    /// That might be a lambda_definition, entry_variable, or anything else, that is not a context variable.
    /// Note that the producer is possibly outside the region of the original `inport`. So it is not safe to use that node for inter
    /// connection. The helper however is perfect for gathering information about a producer. For instance type information, a lambda's
    /// signature etc.
    ///
    /// Returns None if the `input` is not connected to any producing node. For instance if a context_variable is undefined, or any other connection is
    /// not set.
    pub fn find_producer_out(&self, output: OutportLocation) -> Option<OutportLocation> {
        //We trace until we find a non-cv / non-entry variable. We consider all others `producers`.
        //
        // start out by finding the first src port. Per iteration we then either _hop_ over the node boundary (defined by cv / lv),
        // or return.

        let mut next_port = output;

        //For loop safety, track all visited nodes. In theory this is not needed, since we are operating on a DAG,
        // but in practise it is possible to wire loops.
        let mut visited = AHashSet::default();
        visited.insert(next_port.node);

        loop {
            match next_port.output {
                OutputType::ContextVariableArgument(cv) => {
                    //try to _hop_ and find a edge that is connected to the ContextVariableInput(cv)
                    if let Some(cv_inp) = self
                        .node(next_port.node)
                        .inport(&InputType::ContextVariableInput(cv))
                    {
                        if let Some(edg) = cv_inp.edge {
                            next_port = self.edge(edg).src.clone();
                            if visited.contains(&next_port.node) {
                                panic!("detected graph loop in producer chain, which is invalid");
                            }
                            visited.insert(next_port.node);
                        } else {
                            //This is the case that we have a CVInput(cv), but it is not connected.
                            return None;
                        }
                    } else {
                        panic!(
                            "Invalid hop: node {:?} has CVArgument({cv}), but not a CVInput({cv})",
                            next_port.node
                        );
                    }
                }
                OutputType::EntryVariableArgument {
                    branch: _,
                    entry_variable,
                } => {
                    //This happens if we hop over a gamma boundary, which we can do in this direction, since all entry_variable[x] of any branch b are
                    // mapped to the same entry_variable_input.
                    if let Some(ev_inp) = self
                        .node(next_port.node)
                        .inport(&InputType::EntryVariableInput(entry_variable))
                    {
                        if let Some(edg) = ev_inp.edge {
                            next_port = self.edge(edg).src.clone();
                            if visited.contains(&next_port.node) {
                                panic!("detected graph loop in producer chain, which is invalid");
                            }
                            visited.insert(next_port.node);
                        } else {
                            //This is the case that we have a EVInput(cv), but it is not connected.
                            return None;
                        }
                    } else {
                        panic!("Invalid hop: node {:?} has EVArgument({entry_variable}), but not a EVInput({entry_variable})", next_port.node);
                    }
                }
                OutputType::Argument(i) => {
                    //Traverse _outside_ the theta node, if this is a theta-node argument
                    if self.node(next_port.node).node_type.is_theta() {
                        //check that the related input is used as well.
                        if let Some(src) = self.node(next_port.node).input_src(&self, i) {
                            //have a valid src, set this up as the next port, and push our selfs
                            //into the visited list
                            next_port = src;
                            if visited.contains(&src.node) {
                                panic!("detected loop in producer chain, which is invalid!");
                            }
                            visited.insert(next_port.node);
                        } else {
                            //Happens if the theta-input is not connected
                            return None;
                        }
                    } else {
                        // Is not a theta-node argument, so this is the origin of _something_
                        return Some(next_port);
                    }
                }
                //Any other case, return the the node
                _ => return Some(next_port),
            }
        }
    }
    pub fn find_producer_inp(&self, input: InportLocation) -> Option<OutportLocation> {
        let start_out = if let Some(start_edge) = self.node(input.node).inport(&input.input)?.edge {
            self.edge(start_edge).src.clone()
        } else {
            return None;
        };

        self.find_producer_out(start_out)
    }

    //Finds all consumers of this `src` port. The finder traverses all _traversable_
    // node boundaries. So _into_ gamma/theta regions as well as into lambda/phi regions if connected
    // to a context variable.
    pub fn find_consumer_out(&self, src: OutportLocation) -> SmallColl<InportLocation> {
        //NOTE find the first set of connected inport, and union all consumers
        //FIXME: The union_set is kinda dirty atm.
        let mut union_set = AHashSet::new();
        for connected in &self.node(src.node).outport(&src.output).unwrap().edges {
            let dst = self.edge(*connected).dst().clone();
            let consumers = self.find_consumer_in(dst);
            for found in consumers {
                union_set.insert(found);
            }
        }

        union_set.into_iter().collect()
    }

    //Finds all consumers of this `src` port. The finder traverses all _traversable_
    // node boundaries. So _into_ gamma/theta regions as well as into lambda/phi regions if connected
    // to a context variable.
    pub fn find_consumer_in(&self, src: InportLocation) -> SmallColl<InportLocation> {
        let mut seen = AHashSet::new();
        let mut collected = SmallColl::new();

        let mut waiting = vec![src];

        while let Some(next) = waiting.pop() {
            if seen.contains(&next) {
                continue;
            }

            //try to map the port _into_ all regions. If thats possible,
            //push back the next candidate. If not this means the node is a consumer
            let subregcount = self.node(next.node).regions().len();
            if subregcount > 0 {
                let mut pushed_already = false;
                for regixd in 0..subregcount {
                    if let Some(mapped_into) = next.input.map_to_in_region(regixd) {
                        //found new candidate outport, push all connected inports
                        let outport = OutportLocation {
                            node: next.node,
                            output: mapped_into,
                        };
                        for connected in &self
                            .node(outport.node)
                            .outport(&outport.output)
                            .unwrap()
                            .edges
                        {
                            let dst = self.edge(*connected).dst().clone();
                            waiting.push(dst);
                        }
                    } else {
                        //could not map into region, so is a consumer as well, for instance
                        // a GammaPredicate
                        if !pushed_already {
                            pushed_already = true;
                            collected.push(next);
                        }
                    }
                }
            } else {
                //in case of 0 subregions this must be a consumer
                // since its either a apply or a simple node
                collected.push(next);
            }

            seen.insert(next);
        }

        collected
    }

    ///Tries to find a callable definition at the end of this producer-chain. The helper lets you trace over
    /// inter-procedural node boundaries easily. It stops whenever it reaches a node-output that can be called by an apply node,
    /// returning the reference to the node, if that node is an lambda or phi node.
    ///
    /// If you get `Some(node)`, you'd be safe to unwrap into a lambda or phi node.
    pub fn find_callabel_def(&self, src: OutportLocation) -> Option<OutportLocation> {
        //We trace by matching context variables to cross inter/intra-procedural node bounds, until we reach a node-output
        //If the found node is λ/ϕ, we return.

        let producer = self.find_producer_out(src)?;
        match producer.output {
            //TODO: I think the phi nodes might be broken with this
            OutputType::LambdaDeclaration => Some(producer),
            _ => None,
        }
    }

    ///Traverses all connections of the `node` definition port and collects all `apply` nods that are connected
    /// to it. This includes connections through context variables and recursion variables.
    ///
    /// Returns None if `lmd` is not a λ node.
    ///
    /// All returned node references are guaranteed to be Apply nodes.
    pub fn find_caller(&self, node: NodeRef) -> Option<SmallColl<NodeRef>> {
        if !self.node(node).node_type.is_lambda() {
            return None;
        }

        let mut results = SmallColl::new();
        //init stack with the definition_port
        let mut stack = VecDeque::new();
        stack.push_back(OutportLocation {
            node,
            output: OutputType::LambdaDeclaration,
        });

        while !stack.is_empty() {
            let next = stack.pop_front().unwrap();
            //checkout all connections. For each connection, go from the source (next) to the
            // dst, and check if we found an apply node.
            // If so, add the apply node to the result list
            // If not, check if we can map _out_ or _into_ a node. If so, add that port to the stack.
            // otherwise terminate.

            //Do not try to break out of the omega node.
            //NOTE: we could make the for-loop-header catch a None port, but this
            //      case _should_ really be the only way how the unwrap fails. Otherwise
            //      there is some kind of bug in the graph.
            if self.node(next.node).node_type.is_omega() {
                continue;
            }

            //NOTE: checkout above if this panics
            for edg in &self.node(next.node).outport(&next.output).unwrap().edges {
                let dst = self.edge(*edg).dst();
                if self.node(dst.node).node_type.is_apply() {
                    assert!(dst.input == InputType::Input(0), "λ-Decleration was connected to apply node, but not on input 0, which is invalid!");
                    results.push(dst.node);
                } else {
                    //try to map into (all) regions that are connected. Mostly used for
                    //context variables, but this would also map a λ into a loop through the loop-variables etc.
                    for regidx in 0..self.node(dst.node).regions().len() {
                        if let Some(mapped_into) = dst.input.map_to_in_region(regidx) {
                            stack.push_back(OutportLocation {
                                node: dst.node,
                                output: mapped_into,
                            });
                        }
                    }
                    //try to map _out_ of a region. This is mostly done if a λ thats defined in a recursive context
                    // is exported through a recursion variable.
                    if let Some(mapped_out) = dst.input.map_out_of_region() {
                        stack.push_back(OutportLocation {
                            node: dst.node,
                            output: mapped_out,
                        });
                    }
                }
            }
        }

        Some(results)
    }

    ///Iterates over all predecessors of this node, see [PredWalker] for more info.
    pub fn walk_predecessors<'a>(&'a self, node: NodeRef) -> PredWalker<'a, N, E> {
        PredWalker::new(self, node)
    }

    ///Iterates over all successors of this node, see [SuccWalker] for more info.
    pub fn walk_successors<'a>(&'a self, node: NodeRef) -> SuccWalker<'a, N, E> {
        SuccWalker::new(self, node)
    }

    ///Builds an iterator that emits all reachable nodes for the entrypoint of this graph. The iterator is breadth-first and top down style.
    ///
    /// _Reachable_ is defined as any node, that can be reached from any of the exported nodes.
    ///
    /// _Breadth-First_ means, that, for any node n first all directly connected nodes are traversed, before any n-th-degree indirectly connected nodes are traversed.
    ///
    /// _Top-Down_ means, that first all nodes of a region are traversed breadth first, before any discovered sub-regions (of structural nodes) are traversed.
    pub fn walk_reachable<'a>(&'a self) -> ReachableWalker<'a, N, E> {
        //init waiting nodes with all exported nodes
        let ep = self.entry_node();

        let mut waiting_nodes = VecDeque::new();
        let mut walked = AHashSet::new();
        for res in self.node(ep).regions()[0].results.iter() {
            if let Some(edg) = res.edge {
                let node_ref = self.edge(edg).src.node;
                if !walked.contains(&node_ref) {
                    waiting_nodes.push_front(node_ref);
                    walked.insert(node_ref);
                }
            }
        }

        ReachableWalker {
            ctx: self,
            waiting_nodes,
            walked,
            waiting_regions: VecDeque::new(),
        }
    }

    pub fn walk_region_locations<'a>(&'a self) -> RegionLocationWalker<'a, N, E> {
        let mut region_queue = VecDeque::new();
        region_queue.push_back(self.toplevel_region());
        RegionLocationWalker {
            graph: self,
            region_queue,
        }
    }

    ///Iterates all regions of the graph in breadth-first / top-down fashion.
    pub fn walk_regions<'a>(&'a self) -> RegionWalker<'a, N, E> {
        RegionWalker {
            location_walker: self.walk_region_locations(),
        }
    }

    ///Checks if any successor of `node` or `node` itself is part of a loop.
    pub fn node_has_cycle(&self, node: NodeRef) -> bool {
        //Init the unchecked list with all predecessors.
        let mut unchecked = (0..self.node(node).outputs().len())
            .filter_map(|idx| self.node(node).output_dsts(self, idx))
            .flatten()
            .map(|inport| inport.node)
            .collect::<AHashSet<NodeRef>>();
        let mut checked = AHashSet::new();

        let take_next = |map: &mut AHashSet<NodeRef>| -> Option<NodeRef> {
            let next = if let Some(next) = map.iter().next().clone() {
                *next
            } else {
                return None;
            };

            let _ = map.remove(&next);
            Some(next)
        };

        while let Some(next) = take_next(&mut unchecked) {
            //if this is `node`, we found a loop
            if next == node {
                return true;
            }

            //not our node, so push `next` into known nodes
            if !checked.insert(next) {
                //this is somewhat strange, but means that we found a loop where `node` is not a part
                return true;
            }

            //now, build all successors, and push them into `unchecked`, if
            // we didn't check them yet
            for idx in 0..self.node(node).outputs().len() {
                if let Some(connected) = self.node(next).output_dsts(self, idx) {
                    for dst_node_inport in connected {
                        if !checked.contains(&dst_node_inport.node) {
                            let _ = unchecked.insert(dst_node_inport.node);
                        }
                    }
                }
            }
        }

        false
    }

    ///Checks if the region `r` contains cycles.
    pub fn region_has_cycles(&self, region: RegionLocation) -> bool {
        let region = self.region(&region).unwrap();

        //NOTE: this currently a pretty simple "test all nodes for cycle" test.
        //
        // We basically pick each node in the region, and iterate all successors. If we visit a node twice, we found a
        // cycle

        let mut known_nodes = AHashSet::with_capacity(16);
        for arg_idx in 0..region.arguments.len() {
            if let Some(connected) = region.argument_dst(self, arg_idx) {
                for node in connected {
                    if known_nodes.contains(&node) {
                        continue;
                    }
                    if self.node_has_cycle(node.node) {
                        return true;
                    }
                    known_nodes.insert(node);
                }
            }
        }

        false
    }

    ///Builds the dependecy graph for this region's nodes. Only includes region-local nodes.
    /// the `region.node` is a dependecy, if a nodes depends on any _outside-region_ value.
    pub fn region_node_dependecy_graph(&self, region: RegionLocation) -> DependencyGraph {
        let mut graph = AHashMap::default();

        for node in &self.region(&region).unwrap().nodes {
            let mut dependencies = AHashSet::new();
            let inputcount = self.node(*node).inputs().len();
            for input in 0..inputcount {
                if let Some(src) = self.node(*node).input_src(&self, input) {
                    assert!(dependencies.insert(src.node));
                }
            }

            let old = graph.insert(*node, dependencies);
            assert!(old.is_none());
        }

        DependencyGraph { graph }
    }

    ///Traces the path backwards from _end_ to its start.
    ///
    /// Traverses node boundaries like CVs or loop-variables.
    pub fn trace_path(&self, end: InportLocation) -> Path {
        let mut edges = SmallColl::new();
        let start_src = if let Some(edg) = self.node(end.node).inport(&end.input).unwrap().edge {
            edges.push(edg);
            self.edge(edg).src
        } else {
            panic!("Expected at least one edge on path");
        };
        let mut path = Path {
            start: start_src,
            end,
            edges,
        };

        //now append path-parts till we reached an end
        loop {
            if let Some(mapped_out) = path.start.output.map_out_of_region() {
                match self.node(path.start.node).inport(&mapped_out) {
                    Some(next_dst_port) => {
                        //was able to map last start _out of region_ so there could be _something_ connected
                        if let Some(edg) = next_dst_port.edge {
                            path.edges.push(edg);
                            path.start = self.edge(edg).src;
                        }
                    }
                    None => {
                        //The mapping works, but the port does in fact not exist, break as well
                        break;
                    }
                }
            } else {
                //cannot map out of region, so end
                break;
            }
        }

        //NOTE reverse the order of edges, since we start pushing from the end
        path.edges.reverse();
        path
    }
}

pub struct DependencyGraph {
    ///Maps a node to all nodes it depends on
    pub graph: AHashMap<NodeRef, AHashSet<NodeRef>>,
}
