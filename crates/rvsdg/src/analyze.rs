//! RVSDG analyzis tools
//!
//! Provides several tools to analyse an RVSDG. Including walker utilities
//! that make gathering information easy, and finding dependencies or definitions of nodes.

use std::collections::VecDeque;

use ahash::AHashSet;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::{LangNode, Node},
    NodeRef, Rvsdg,
};

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

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Traverses context-variable boundaries of inter-procedural nodes as well as entry-variable boundaries
    /// of γ-Nodes, until it finds the producing port of `input`.
    ///
    /// That might be a lambda_definition, entry_variable, or anything else, that is not a context variable.
    /// Note that the producer is possibly outside the region of the original `inport`. So it is not safe to use that node for inter
    /// connection. The helper however is perfect for gathering information about a producer. For instance type information, a lambda's
    /// signature etc.
    ///
    /// Returns None if the `input` is not connected to any producing node. For instance if a context_variable is undefined, or any other connection is
    /// not set.
    pub fn find_producer_out(&self, output: OutportLocation) -> Option<NodeRef> {
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
                        panic!("Invalid hop: node hat CVArgument({cv}), but not a CVInput({cv})");
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
                        panic!("Invalid hop: node has EVArgument({entry_variable}), but not a CVInput({entry_variable})");
                    }
                }
                //Any other case, return the the node
                _ => return Some(next_port.node),
            }
        }
    }
    pub fn find_producer_inp(&self, input: InportLocation) -> Option<NodeRef> {
        let start_out = if let Some(start_edge) = self.node(input.node).inport(&input.input)?.edge {
            self.edge(start_edge).src.clone()
        } else {
            return None;
        };

        self.find_producer_out(start_out)
    }

    ///Tries to find a callable definition at the end of this producer-chain. The helper lets you trace over
    /// inter-procedural node boundaries easily. It stops whenever it reaches a node-output that can be called by an apply node,
    /// returning the reference to the node, if that node is an lambda or phi node.
    ///
    /// If you get `Some(node)`, you'd be safe to unwrap into a lambda or phi node.
    pub fn find_callabel_def(&self, src: OutportLocation) -> Option<NodeRef> {
        //We trace by matching context variables to cross inter/intra-procedural node bounds, until we reach a node-output
        //If the found node is λ/ϕ, we return.

        let producer = self.find_producer_out(src)?;

        match self.node(producer) {
            Node::Lambda(_) | Node::Phi(_) => Some(producer),
            _ => None,
        }
    }

    ///Iterates over all predecessors of this node, see [PredWalker] for more info.
    pub fn walk_predecessors<'a>(&'a self, node: NodeRef) -> PredWalker<'a, N, E> {
        PredWalker::new(self, node)
    }

    ///Iterates over all successors of this node, see [SuccWalker] for more info.
    pub fn walk_successors<'a>(&'a self, node: NodeRef) -> SuccWalker<'a, N, E> {
        SuccWalker::new(self, node)
    }

    ///Explores the region from this node, trying to find its parent node. Returns none, if the node is not connected to any
    /// region's result, or argument via any reference.
    pub fn find_parent(&self, node: NodeRef) -> Option<NodeRef> {
        //First, check if we can find a source argument
        for pred in self.walk_predecessors(node) {
            if let OutputType::Argument(_) = pred.output {
                return Some(pred.node);
            }
        }

        //if that didn't work, try the same for an result
        for succ in self.walk_successors(node) {
            if let InputType::Result(_) = succ.input {
                return Some(succ.node);
            }
        }
        //Otherwise, this is not connected to a region
        None
    }
}
