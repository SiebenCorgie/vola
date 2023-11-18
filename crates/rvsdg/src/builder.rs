//! RVSDG building utilies. If your source is based on structured-control flow, the standart RVSDG builder should give
//! you everything needed to setup a a RVSDG representation.
//!
//! For non-SCF based source the paper outlines a construction strategy in 5.1, however this is not (yet) implemented.
//!
//!
//! In general the builder pattern usually represents some region that is currently worked on.
//! Those regions _usually_ mostly consist of [_Simple_ nodes](crate::nodes::Node). Whenever one of the more complex constructs is needed,
//! like a loop, if-then-else, function calls etc. the builder provides special methodes to build those.
//!
//! Nesting is represented by the closures used to build such special methodes.
//!

use tinyvec::ArrayVec;

use crate::{
    edge::{Edge, LangEdge, PortIndex},
    nodes::{LambdaNode, LangNode, Node},
    region::{Port, Region},
    NodeRef, RegionRef, Rvsdg,
};

///Probably the most used builder. Represents a simple [Region](crate::region::Region) within one of the higher level nodes.
pub struct RegionBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    region: Region,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> RegionBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        RegionBuilder {
            ctx,
            region: Region::new(),
        }
    }

    pub fn build(self) -> RegionRef {
        self.ctx.regions.insert(self.region)
    }
}

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
        let body = ctx.new_region();
        let node_ref = ctx.new_node(Node::Invalid);
        LambdaBuilder {
            ctx,
            node: LambdaNode {
                cv_count: 0,
                inputs: ArrayVec::default(),
                output: Port::default(),
                body,
            },
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
        //TODO could already legalize that the source node is a lambda node...

        //setup edge on both ports
        debug_assert!(self
            .ctx
            .port(import, port_index.clone())
            .unwrap()
            .edge
            .is_none());
        self.ctx.port_mut(import, port_index).unwrap().edge = Some(edge_ref);

        assert!(self.node.inputs[cv_idx].edge.is_none());
        self.node.inputs[cv_idx].edge = Some(edge_ref);

        (self, cv_idx)
    }

    pub fn add_argument(mut self) -> (Self, usize) {
        let idx = self.node.add_input(self.ctx);
        (self, idx)
    }
}
