//! private mod declaring intra procedural node builder.
//! Those are simple nodes, gamma nodes, and theta nodes.

use crate::{
    edge::LangEdge,
    nodes::{GammaNode, LangNode, Node, ThetaNode},
    region::Inport,
    NodeRef, Rvsdg,
};

use super::RegionBuilder;

///[γ-node](crate::nodes::GammaNode) (if-then-else) builder.
pub struct GammaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> GammaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(Node::Gamma(GammaNode::new()));
        GammaBuilder { ctx, node_ref }
    }

    pub fn node(&self) -> &GammaNode {
        if let Node::Gamma(g) = self.ctx.node(self.node_ref) {
            g
        } else {
            panic!("not gamma!")
        }
    }

    pub fn node_mut(&mut self) -> &mut GammaNode {
        if let Node::Gamma(g) = self.ctx.node_mut(self.node_ref) {
            g
        } else {
            panic!("not gamma!")
        }
    }

    ///Inserts this node into the graph, returning the reference it is safed at.
    pub fn build(self) -> NodeRef {
        let GammaBuilder { ctx: _, node_ref } = self;
        node_ref
    }

    ///Creates a new branch for this decision point.
    pub fn new_branch<R>(
        &mut self,
        branch_builder: impl FnOnce(&mut RegionBuilder<N, E, GammaNode>) -> R,
    ) -> (usize, R) {
        self.node_mut().add_region();
        let idx = self.node().regions.len() - 1;
        let mut builder = RegionBuilder::new(self.ctx, idx, self.node_ref);
        let res = branch_builder(&mut builder);

        (idx, res)
    }

    ///Adds a new variable that is used as an argument to all branches.
    pub fn add_entry_variable(&mut self) -> usize {
        self.node_mut().add_entry_var();
        let idx = self.node().entry_var_count - 1;
        idx
    }

    ///Adds a new variable that is used as a result of all branches.
    pub fn add_exit_variable(&mut self) -> usize {
        self.node_mut().add_exit_var();
        let idx = self.node().exit_var_count - 1;
        idx
    }
}

///[θ-node](crate::nodes::ThetaNode) (loop) builder.
pub struct ThetaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> ThetaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(Node::Theta(ThetaNode::new()));
        ThetaBuilder { ctx, node_ref }
    }

    pub fn node(&self) -> &ThetaNode {
        if let Node::Theta(t) = self.ctx.node(self.node_ref) {
            t
        } else {
            panic!("not theta!")
        }
    }

    pub fn node_mut(&mut self) -> &mut ThetaNode {
        if let Node::Theta(t) = self.ctx.node_mut(self.node_ref) {
            t
        } else {
            panic!("not theta!")
        }
    }

    ///Inserts this node into the graph, returning the reference it is safed at.
    pub fn build(self) -> NodeRef {
        let ThetaBuilder { ctx: _, node_ref } = self;
        node_ref
    }

    ///lets you change the behaviour of this node.
    pub fn on_loop<R>(&mut self, f: impl FnOnce(&mut RegionBuilder<N, E, ThetaNode>) -> R) -> R {
        //setup the builder for the region
        let mut builder = RegionBuilder::new(self.ctx, 0, self.node_ref);
        f(&mut builder)
    }

    ///Adds a new loop variable. This are variables that are an input, and/or an output to a single iteration of the loop.
    ///Returns the loop variable index it is created on.
    pub fn add_loop_variable(mut self) -> (Self, usize) {
        let created_at_idx = self.node_mut().add_loop_variable();
        (self, created_at_idx)
    }
}
