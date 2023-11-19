//! privately declares all inter-procedural nodes, which are lambda, delta, phi and omega nodes.
use tinyvec::ArrayVec;

use crate::{
    edge::{Edge, LangEdge, PortIndex},
    nodes::{LambdaNode, LangNode, Node},
    region::Port,
    NodeRef, Rvsdg,
};

///[λ-region](crate::nodes::LambdaNode) builder.
pub struct LambdaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///The node that is being build
    node: LambdaNode,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> LambdaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(Node::Invalid);
        let node = LambdaNode::new(ctx);
        LambdaBuilder {
            ctx,
            node,
            node_ref,
        }
    }

    ///Builds the Lambda node for the borrowed context.
    pub fn build(self) -> NodeRef {
        //TODO: do some legalization already, or wait for a legalization pass?

        //Replace node_ref with actual *valid* lambda node
        *self.ctx.node_mut(self.node_ref) = Node::Lambda(self.node);
        self.node_ref
    }

    ///Imports the node `import` as a context variable. This means that `import` can be evaluated within this lambda.
    ///
    /// Returns not just the builder, but also the index of the context variable in this lambda's body.
    pub fn import_context(mut self, import: NodeRef, port_index: PortIndex) -> (Self, usize) {
        //Add new cv var
        let cv_idx = self.node.add_context_variable(self.ctx);
        // create edge to source
        let edge_ref = self.ctx.new_edge(Edge {
            src: import,
            src_index: port_index.clone(),
            dst: self.node_ref,
            dst_index: PortIndex::ContextVar {
                var_index: cv_idx,
                tuple_index: 0,
            },
            ty: E::value_edge(),
        });

        //setup edge on both ports
        self.ctx
            .port_mut(import, port_index)
            .unwrap()
            .edges
            .push(edge_ref);

        assert!(self.node.inputs[cv_idx].edges.is_empty());
        self.node.inputs[cv_idx].edges.push(edge_ref);

        (self, cv_idx)
    }

    ///Adds an argument to the lambda node, and its inner region. Returns the argument index
    pub fn add_argument(mut self) -> (Self, usize) {
        let idx = self.node.add_input(self.ctx);
        (self, idx)
    }

    ///Adds an result port the the function body. Returns the created output's index
    pub fn add_result(mut self) -> (Self, usize) {
        let idx = self.node.add_result(self.ctx);
        (self, idx)
    }
}
