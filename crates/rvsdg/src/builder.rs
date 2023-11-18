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

    /*
    ///Imports the node `import` as a context variable. This means that `import` can be evaluated within this lambda.
    ///
    /// Returns not just the builder, but also the index of the context variable.
    pub fn import_context(mut self, import: NodeRef, port_index: PortIndex) -> (Self, usize) {
        //Add a cv port configuration, and connect the lambda's port to the ctx
        let p_idx = self.node.context_variables.len();
        self.node.context_variables.push(Port::default());
        //insert a port at p_idx, aka, after all already registered cv-ports, but before any arguments.
        self.ctx.on_region(self.node.body, |region| {
            region.arguments.insert(p_idx, Port::default())
        });

        let edge = Edge {
            src: import,
            src_index: port_index,
            dst: self.node_ref,
            dst_index: PortIndex::ContextVar {
                var_index: p_idx,
                tuple_index: 2,
            },
            ty: E::value_edge(),
        };
        //now build a value-edge from `import` to the input cv
        let edge = self.ctx.new_edge(edge);
        //Add that edge to the port of the just generated port, as well as to the import node
        self.node.context_variables[p_idx].edge = Some(edge.clone());
        todo!("record in input node");
        //        self.ctx
        //            .on_node(import, |n| n.outputs_mut()[port_index].edge = Some(edge));

        //Return the cv variable. This is always also the index of the cv-argument of the `body`
        (self, p_idx)
    }
    */
}
