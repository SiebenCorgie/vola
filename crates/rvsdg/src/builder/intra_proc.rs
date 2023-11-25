//! private mod declaring intra procedural node builder.
//! Those are simple nodes, gamma nodes, and theta nodes.

use crate::{
    edge::LangEdge,
    nodes::{GammaNode, LangNode, Node, ThetaNode},
    NodeRef, Rvsdg,
};

use super::RegionBuilder;

///[γ-node](crate::nodes::GammaNode) (if-then-else) builder.
pub struct GammaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///The node that is being build
    node: GammaNode,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> GammaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(Node::Invalid);
        GammaBuilder {
            ctx,
            node: GammaNode::new(),
            node_ref,
        }
    }

    ///Inserts this node into the graph, returning the reference it is safed at.
    pub fn build(self) -> NodeRef {
        let GammaBuilder {
            ctx,
            node,
            node_ref,
        } = self;
        *ctx.node_mut(node_ref) = Node::Gamma(node);
        node_ref
    }

    ///Creates a new branch for this decision point.
    pub fn new_branch(&mut self, branch_builder: impl FnOnce(&mut RegionBuilder<N, E>)) -> usize {
        self.node.add_region();
        let idx = self.node.regions.len() - 1;
        let mut builder = RegionBuilder::new(self.ctx, &mut self.node.regions[idx], self.node_ref);
        branch_builder(&mut builder);

        idx
    }

    ///Adds a new variable that is used as an argument to all branches.
    pub fn add_entry_variable(&mut self) -> usize {
        self.node.add_entry_var();
        let idx = self.node.entry_var_count - 1;
        idx
    }

    ///Adds a new variable that is used as a result of all branches.
    pub fn add_exit_variable(&mut self) -> usize {
        self.node.add_exit_var();
        let idx = self.node.exit_var_count - 1;
        idx
    }
}

///[θ-node](crate::nodes::ThetaNode) (loop) builder.
pub struct ThetaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///The node that is being build
    node: ThetaNode,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> ThetaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(Node::Invalid);
        let node = ThetaNode::new();
        ThetaBuilder {
            ctx,
            node,
            node_ref,
        }
    }

    ///Inserts this node into the graph, returning the reference it is safed at.
    pub fn build(self) -> NodeRef {
        let ThetaBuilder {
            ctx,
            node,
            node_ref,
        } = self;
        *ctx.node_mut(node_ref) = Node::Theta(node);
        node_ref
    }

    ///Adds a new loop variable. This are variables that are an input, and/or an output to a single iteration of the loop.
    ///Returns the loop variable index it is created on.
    pub fn add_loop_variable(mut self) -> (Self, usize) {
        let created_at_idx = self.node.add_loop_variable();
        (self, created_at_idx)
    }
}
