//! privately declares all inter-procedural nodes, which are lambda, delta, phi and omega nodes.
//!
use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
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
        let node = LambdaNode::new();
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

    ///Adds a context variable to the LambdaNode's signature and body
    pub fn add_context_variable(&mut self) -> usize {
        self.node.add_context_variable()
    }

    ///Adds an argument to the lambda node's body. Returns the argument index it was added at.
    pub fn add_argument(&mut self) -> OutportLocation {
        let idx = self.node.add_argument();
        OutportLocation {
            node: self.node_ref,
            output: OutputType::Argument(idx),
        }
    }

    ///Adds an result port the the function's body. Returns the created result_port of the lambda's internal region.
    pub fn add_result(&mut self) -> InportLocation {
        let idx = self.node.add_result();
        InportLocation {
            node: self.node_ref,
            input: InputType::Result(idx),
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
        let mut builder = RegionBuilder::new(self.ctx, &mut self.node.body, self.node_ref);
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
        let node = DeltaNode::new();
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

    ///Adds a context variable to the DeltaNode's signature and body
    pub fn add_context_variable(&mut self) -> usize {
        self.node.add_context_variable()
    }

    ///lets you change the behaviour of this node.
    pub fn on_region(&mut self, f: impl FnOnce(&mut RegionBuilder<N, E>)) {
        //setup the builder for the region
        let mut builder = RegionBuilder::new(self.ctx, &mut self.node.body, self.node_ref);
        f(&mut builder);
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
        let node = PhiNode::new();
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

    ///Adds a context variable to the PhiNode's signature and body
    pub fn add_context_variable(&mut self) -> usize {
        self.node.add_context_variable()
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
        self.node.add_recursion_variable()
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
    pub fn import(&mut self, label: impl Into<String>) -> OutportLocation {
        let idx = self.node.add_import();
        let portloc = OutportLocation {
            node: self.node_ref,
            output: OutputType::Argument(idx),
        };
        //annotate the port with the given label
        self.ctx
            .push_label(LabelLoc::OutPort(portloc.clone()), label.into());
        portloc
    }

    ///Adds an export port with the given label. The label will used for the export of the function or value (read C-FFI style).
    /// Returns the argument_index of the port allocated.
    pub fn export(&mut self, label: String) -> InportLocation {
        let idx = self.node.add_export();
        let portloc = InportLocation {
            node: self.node_ref,
            input: InputType::Result(idx),
        };
        //annotate the port with the given label
        self.ctx
            .push_label(LabelLoc::InPort(portloc.clone()), label.into());
        portloc
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
        self.node.body.nodes.insert(created);
        created
    }

    ///Allows you to add a function ([λ-Node](crate::nodes::LambdaNode)) to the translation unit.
    ///
    /// If `export` is `Some(label)`, it will be added to the exported functions of the translation unit under the name of `label`.
    ///
    /// Returns the node reference to the created value.
    pub fn new_function(
        &mut self,
        export: Option<String>,
        f: impl FnOnce(&mut LambdaBuilder<N, E>),
    ) -> NodeRef {
        let created = {
            let mut builder = LambdaBuilder::new(self.ctx);
            f(&mut builder);
            builder.build()
        };

        //add the created node to our region
        self.node.body.nodes.insert(created);

        if let Some(explabel) = export {
            let export_port = self.export(explabel);
            //add a connection to the label
            self.node
                .body
                .connect(
                    self.node_ref,
                    self.ctx,
                    OutportLocation {
                        node: created,
                        output: OutputType::LambdaDecleration,
                    },
                    export_port,
                    E::value_edge(),
                )
                .unwrap();
        }
        created
    }

    ///lets you change the behaviour of this node.
    pub fn on_region(&mut self, f: impl FnOnce(&mut RegionBuilder<N, E>)) {
        //setup the builder for the region
        let mut builder = RegionBuilder::new(self.ctx, &mut self.node.body, self.node_ref);
        f(&mut builder);
    }
}
