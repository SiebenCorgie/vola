//! privately declares all inter-procedural nodes, which are lambda, delta, phi and omega nodes.

use crate::{
    edge::{Edge, LangEdge, PortIndex},
    label::LabelLoc,
    nodes::{DeltaNode, LambdaNode, LangNode, Node, OmegaNode, PhiNode},
    NodeRef, Rvsdg,
};

use super::RegionBuilder;

///[λ-node](crate::nodes::LambdaNode) (function declaration) builder.
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

        let LambdaBuilder {
            ctx,
            node,
            node_ref,
        } = self;

        //Replace node_ref with actual *valid* lambda node
        *ctx.node_mut(node_ref) = Node::Lambda(node);
        node_ref
    }

    ///Imports the node `import` as a context variable. This means that `import` can be evaluated within this lambda.
    ///
    /// Returns not just the builder, but also the index of the context variable in this lambda's body.
    pub fn import_context(&mut self, import: NodeRef, port_index: PortIndex) -> usize {
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

        cv_idx
    }

    ///Adds an argument to the lambda node, and its inner region. Returns the argument index of the lambda's internal region.
    pub fn add_argument(&mut self) -> PortIndex {
        let idx = self.node.add_argument(self.ctx);
        PortIndex::Arg {
            subregion: 0,
            arg_idx: idx,
        }
    }

    ///Adds an result port the the function body. Returns the created result_port of the lambda's internal region.
    pub fn add_result(&mut self) -> PortIndex {
        let idx = self.node.add_result(self.ctx);
        PortIndex::Result {
            subregion: 0,
            arg_idx: idx,
        }
    }

    ///Returns the node reference this builder's final node will be inserted as.
    /// Allows you to already connect the node's ports while building.
    pub fn get_node_ref(&self) -> NodeRef {
        self.node_ref
    }
    ///lets you change the behaviour of this node.
    pub fn on_region(&mut self, f: impl FnOnce(&mut RegionBuilder<N, E>)) {
        //setup the builder for the region
        let mut builder = RegionBuilder::new(self.ctx, self.node.body, self.node_ref);
        f(&mut builder);
    }
}

///[δ-node](crate::nodes::DeltaNode) (global variable) builder.
pub struct DeltaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///The node that is being build
    node: DeltaNode,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> DeltaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(Node::Invalid);
        let node = DeltaNode::new(ctx);
        DeltaBuilder {
            ctx,
            node,
            node_ref,
        }
    }

    ///Builds the Lambda node for the borrowed context.
    pub fn build(self) -> NodeRef {
        let DeltaBuilder {
            ctx,
            node,
            node_ref,
        } = self;

        *ctx.node_mut(node_ref) = Node::Delta(node);
        node_ref
    }

    ///Imports the node `import` as a context variable. This means that `import` can be evaluated or used within this delta.
    ///
    /// Returns the index of the context variable in this delta's body.
    pub fn import_context(&mut self, import: NodeRef, port_index: PortIndex) -> usize {
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

        cv_idx
    }

    ///lets you change the behaviour of this node.
    pub fn on_region(&mut self, f: impl FnOnce(&mut RegionBuilder<N, E>)) {
        //setup the builder for the region
        let mut builder = RegionBuilder::new(self.ctx, self.node.body, self.node_ref);
        f(&mut builder);
    }

    ///Connects `dst`'s `port` to the output of this delta node.
    pub fn connect_output(&mut self, dst: NodeRef, port: PortIndex) {
        let edge = self.ctx.new_edge(Edge {
            src: self.node_ref,
            src_index: PortIndex::Output(0),
            dst,
            dst_index: port.clone(),
            ty: E::value_edge(),
        });
        self.node.output.edges.push(edge);
        self.ctx.port_mut(dst, port).unwrap().edges.push(edge);
    }
}

///[ϕ-node](crate::nodes::PhiNode) (mutual-recursion) builder.
///
/// # Disclaimer
/// The Phi node is currently not sound (I think), please have a look at [ϕ-Node](crate::nodes::PhiNode) documentation.
pub struct PhiBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///The node that is being build
    node: PhiNode,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> PhiBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(Node::Invalid);
        let node = PhiNode::new(ctx);
        PhiBuilder {
            ctx,
            node,
            node_ref,
        }
    }

    ///Builds the Lambda node for the borrowed context.
    pub fn build(self) -> NodeRef {
        let PhiBuilder {
            ctx,
            node,
            node_ref,
        } = self;

        *ctx.node_mut(node_ref) = Node::Phi(node);
        node_ref
    }

    ///Imports the node `import` as a context variable. This means that `import` can be evaluated or used within this PhiNode.
    ///
    /// Returns the index of the context variable in this phi's body.
    pub fn import_context(&mut self, import: NodeRef, port_index: PortIndex) -> usize {
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

        cv_idx
    }

    ///Declares a recursion variable to the inner recursion of some lambda. This are usually the values that change per recursion level.
    ///
    /// For instance consider this function (from Figure 3 b. in the source paper):
    /// ```C
    /// unsigned int f(unsigned int x){
    ///     if (1 != x){
    ///         return x * f(x - 1);
    ///     }
    ///     return 1;
    /// }
    /// ```
    ///
    /// In this case a recursion variable is `x` and the result of `f()`. This is a little hard to grasp. However, this means that x
    /// is "somehow involved in the recursion", and the result of the function `f()` is based on that.
    ///
    ///
    /// The function returns the recursion variable index. When calling the ϕ-Node later via an apply node, this will be the `n-th` argument on that phi node.
    //TODO: Find a better way to explain that?
    pub fn add_recursion_variable(&mut self) -> usize {
        let idx = self.node.add_recursion_variable(self.ctx);
        idx
    }
}

///[ω-node](crate::nodes::OmegaNode) (translation-unit) builder. You should usually not create this node (or builder) yourself. Instead use
/// the [RVSDG's](crate::Rvsdg) [on_omega_node](crate::Rvsdg::on_omega_node) helper.
pub struct OmegaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    pub(crate) ctx: &'a mut Rvsdg<N, E>,
    ///The node that is being build
    pub(crate) node: OmegaNode,
    ///Preallocated invalid node ref
    pub(crate) node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> OmegaBuilder<'a, N, E> {
    /*
        pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
            let node_ref = ctx.new_node(Node::Invalid);
            let node = OmegaNode::new(ctx);
            OmegaBuilder {
                ctx,
                node,
                node_ref,
            }
        }

        ///Builds the Lambda node for the borrowed context.
        pub fn build(self) -> NodeRef {
            let OmegaBuilder {
                ctx,
                node,
                node_ref,
            } = self;

            *ctx.node_mut(node_ref) = Node::Omega(node);
            node_ref
        }
    */
    ///Adds an import port with the given label. The label will used for the import of the function or value.
    /// Returns the argument_index of the port allocated.
    pub fn import(&mut self, label: impl Into<String>) -> PortIndex {
        let idx = self.node.add_import(self.ctx);
        //annotate the port with the given label
        self.ctx.push_label(
            LabelLoc::Port {
                node: self.node_ref,
                port: PortIndex::StdArg {
                    subregion: 0,
                    arg_idx: idx,
                },
            },
            label.into(),
        );
        PortIndex::StdArg {
            subregion: 0,
            arg_idx: idx,
        }
    }

    ///Adds an export port with the given label. The label will used for the export of the function or value (read C-FFI style).
    /// Returns the argument_index of the port allocated.
    pub fn export(&mut self, label: &str) -> PortIndex {
        let idx = self.node.add_export(self.ctx);
        //annotate the port with the given label
        self.ctx.push_label(
            LabelLoc::Port {
                node: self.node_ref,
                port: PortIndex::StdResult {
                    subregion: 0,
                    arg_idx: idx,
                },
            },
            label.into(),
        );

        PortIndex::StdResult {
            subregion: 0,
            arg_idx: idx,
        }
    }

    ///Allows you to add a global value ([δ-Node](crate::nodes::DeltaNode)) to the translation unit.
    ///
    /// Returns the node reference to the created value.
    pub fn new_global(&mut self, f: impl FnOnce(&mut DeltaBuilder<N, E>)) -> NodeRef {
        let created = {
            let mut builder = DeltaBuilder::new(self.ctx);
            f(&mut builder);
            builder.build()
        };

        //add the created node to our region
        self.ctx.region_mut(self.node.body).nodes.insert(created);
        created
    }

    ///Allows you to add a function ([λ-Node](crate::nodes::LambdaNode)) to the translation unit.
    ///
    /// Returns the node reference to the created value.
    pub fn new_function(&mut self, f: impl FnOnce(&mut LambdaBuilder<N, E>)) -> NodeRef {
        let created = {
            let mut builder = LambdaBuilder::new(self.ctx);
            f(&mut builder);
            builder.build()
        };

        //add the created node to our region
        self.ctx.region_mut(self.node.body).nodes.insert(created);
        created
    }
}
