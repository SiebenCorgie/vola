//! privately declares all inter-procedural nodes, which are lambda, delta, phi and omega nodes.
//!
use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::{DeltaNode, LambdaNode, LangNode, NodeType, OmegaNode, PhiNode},
    NodeRef, Rvsdg,
};

use super::RegionBuilder;

///[λ-node](crate::nodes::LambdaNode) (function declaration) builder.
pub struct LambdaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> LambdaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(NodeType::Lambda(LambdaNode::new()));
        LambdaBuilder { ctx, node_ref }
    }

    pub fn node(&self) -> &LambdaNode {
        if let NodeType::Lambda(l) = &self.ctx.node(self.node_ref).node_type {
            l
        } else {
            panic!("not lambda!")
        }
    }

    pub fn node_mut(&mut self) -> &mut LambdaNode {
        if let NodeType::Lambda(l) = &mut self.ctx.node_mut(self.node_ref).node_type {
            l
        } else {
            panic!("not lambda!")
        }
    }

    ///Builds the Lambda node for the borrowed context.
    pub fn build(self) -> NodeRef {
        //TODO: do some legalization already, or wait for a legalization pass?
        let LambdaBuilder { ctx: _, node_ref } = self;
        node_ref
    }

    ///Adds a context variable to the LambdaNode's signature and body. Returns (CV_Input, CV_Argument).
    pub fn add_context_variable(&mut self) -> (InportLocation, OutportLocation) {
        let idx = self.node_mut().add_context_variable();

        (
            InportLocation {
                node: self.node_ref,
                input: InputType::ContextVariableInput(idx),
            },
            OutportLocation {
                node: self.node_ref,
                output: OutputType::ContextVariableArgument(idx),
            },
        )
    }

    ///Adds an argument to the lambda node's body. Returns the argument index it was added at.
    pub fn add_argument(&mut self) -> OutportLocation {
        let idx = self.node_mut().add_argument();
        OutportLocation {
            node: self.node_ref,
            output: OutputType::Argument(idx),
        }
    }

    ///Adds an result port the the function's body. Returns the created result_port of the lambda's internal region.
    pub fn add_result(&mut self) -> InportLocation {
        let idx = self.node_mut().add_result();
        InportLocation {
            node: self.node_ref,
            input: InputType::Result(idx),
        }
    }

    ///Lets you change the behaviour of this node. You can use `R` to export state from _inside_ the builder outside.
    pub fn on_region<R>(&mut self, f: impl FnOnce(&mut RegionBuilder<N, E, LambdaNode>) -> R) -> R {
        //setup the builder for the region
        let mut builder = RegionBuilder::new(self.ctx, 0, self.node_ref);
        f(&mut builder)
    }
}

///[δ-node](crate::nodes::DeltaNode) (global variable) builder.
pub struct DeltaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> DeltaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(NodeType::Delta(DeltaNode::new()));
        DeltaBuilder { ctx, node_ref }
    }

    ///Builds the Lambda node for the borrowed context.
    pub fn build(self) -> NodeRef {
        let DeltaBuilder { ctx: _, node_ref } = self;
        node_ref
    }

    pub fn node(&self) -> &DeltaNode {
        if let NodeType::Delta(d) = &self.ctx.node(self.node_ref).node_type {
            d
        } else {
            panic!("not delta!")
        }
    }

    pub fn node_mut(&mut self) -> &mut DeltaNode {
        if let NodeType::Delta(d) = &mut self.ctx.node_mut(self.node_ref).node_type {
            d
        } else {
            panic!("not delta!")
        }
    }

    ///Adds a context variable to the DeltaNode's signature and body. Returns (CV_Input, CV_Argument).
    pub fn add_context_variable(&mut self) -> (InportLocation, OutportLocation) {
        let idx = self.node_mut().add_context_variable();

        (
            InportLocation {
                node: self.node_ref,
                input: InputType::ContextVariableInput(idx),
            },
            OutportLocation {
                node: self.node_ref,
                output: OutputType::ContextVariableArgument(idx),
            },
        )
    }

    ///lets you change the behaviour of this node.
    pub fn on_region<R>(&mut self, f: impl FnOnce(&mut RegionBuilder<N, E, DeltaNode>) -> R) -> R {
        //setup the builder for the region
        let mut builder = RegionBuilder::new(self.ctx, 0, self.node_ref);
        f(&mut builder)
    }
}

///[ϕ-node](crate::nodes::PhiNode) (mutual-recursion) builder.
///
/// # Disclaimer
/// The Phi node is currently not sound (I think), please have a look at [ϕ-Node](crate::nodes::PhiNode) documentation.
pub struct PhiBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> PhiBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let node_ref = ctx.new_node(NodeType::Phi(PhiNode::new()));
        PhiBuilder { ctx, node_ref }
    }

    ///Builds the Lambda node for the borrowed context.
    pub fn build(self) -> NodeRef {
        let PhiBuilder { ctx: _, node_ref } = self;

        node_ref
    }

    pub fn node(&self) -> &PhiNode {
        if let NodeType::Phi(p) = &self.ctx.node(self.node_ref).node_type {
            p
        } else {
            panic!("not phi!")
        }
    }

    pub fn node_mut(&mut self) -> &mut PhiNode {
        if let NodeType::Phi(p) = &mut self.ctx.node_mut(self.node_ref).node_type {
            p
        } else {
            panic!("not phi!")
        }
    }

    ///Adds a context variable to the PhiNode's signature and body. Returns (CV_Input, CV_Argument).
    pub fn add_context_variable(&mut self) -> (InportLocation, OutportLocation) {
        let idx = self.node_mut().add_context_variable();
        (
            InportLocation {
                node: self.node_ref,
                input: InputType::ContextVariableInput(idx),
            },
            OutportLocation {
                node: self.node_ref,
                output: OutputType::ContextVariableArgument(idx),
            },
        )
    }
    ///Adds an argument to the phi-node's body. Returns the argument index it was added at.
    pub fn add_argument(&mut self) -> OutportLocation {
        let idx = self.node_mut().add_argument();
        OutportLocation {
            node: self.node_ref,
            output: OutputType::Argument(idx),
        }
    }

    ///Adds an result port the the phi's body. Returns the created result_port of the lambda's internal region.
    pub fn add_result(&mut self) -> InportLocation {
        let idx = self.node_mut().add_result();
        InportLocation {
            node: self.node_ref,
            input: InputType::Result(idx),
        }
    }

    ///lets you change the behaviour of this node.
    pub fn on_region<R>(&mut self, f: impl FnOnce(&mut RegionBuilder<N, E, PhiNode>) -> R) -> R {
        //setup the builder for the region
        let mut builder = RegionBuilder::new(self.ctx, 0, self.node_ref);
        f(&mut builder)
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
    /// In that case `f()` needs to import `f()` (since we do a recursive call), and also needs to export the definition of `f()`. This is modeled by
    /// defining `f` via the recursion variable result (element 0 of the return value). We allow f to call itself, by also defining a new argument to the Phi's region
    /// (element 1 of the return value). This is basically a _special_ context variable that is not set from the outside via an input port, but from the inside by the former
    /// mentioned rv_result.
    ///
    /// Finally we want to be able to call the `f()` from outside the recursion, which is why we define a new output to the PhiNode, which is the non-recursive definition of `f()`,
    /// which can be called outside the PhiNode.
    //TODO: Find a better way to explain that?
    pub fn add_recursion_variable(&mut self) -> (InportLocation, OutportLocation, OutportLocation) {
        let idx = self.node_mut().add_recursion_variable();

        (
            InportLocation {
                node: self.node_ref,
                input: InputType::RecursionVariableResult(idx),
            },
            OutportLocation {
                node: self.node_ref,
                output: OutputType::RecursionVariableArgument(idx),
            },
            OutportLocation {
                node: self.node_ref,
                output: OutputType::RecursionVariableOutput(idx),
            },
        )
    }
}

///[ω-node](crate::nodes::OmegaNode) (translation-unit) builder. You should usually not create this node (or builder) yourself. Instead use
/// the [RVSDG's](crate::Rvsdg) [on_omega_node](crate::Rvsdg::on_omega_node) helper.
///
/// Exposes some shortcuts for common operations. You can also use [OmegaBuilder::on_region] to fully customize the toplevel region of your
/// translation unit.
pub struct OmegaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    pub(crate) ctx: &'a mut Rvsdg<N, E>,
    ///Preallocated invalid node ref
    pub(crate) node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> OmegaBuilder<'a, N, E> {
    pub fn node(&self) -> &OmegaNode {
        if let NodeType::Omega(o) = &self.ctx.node(self.node_ref).node_type {
            o
        } else {
            panic!("not omega!")
        }
    }

    pub fn node_mut(&mut self) -> &mut OmegaNode {
        if let NodeType::Omega(o) = &mut self.ctx.node_mut(self.node_ref).node_type {
            o
        } else {
            panic!("not omeg!")
        }
    }

    ///Adds an import port.
    /// Returns the argument_index of the port allocated.
    pub fn import(&mut self) -> OutportLocation {
        let idx = self.node_mut().add_import();
        let portloc = OutportLocation {
            node: self.node_ref,
            output: OutputType::Argument(idx),
        };
        portloc
    }

    ///Adds an export port.
    /// Returns the argument_index of the port allocated.
    pub fn export(&mut self) -> InportLocation {
        let idx = self.node_mut().add_export();
        let portloc = InportLocation {
            node: self.node_ref,
            input: InputType::Result(idx),
        };
        //annotate the port with the given label
        portloc
    }

    ///Allows you to add a global value ([δ-Node](crate::nodes::DeltaNode)) to the translation unit.
    ///
    /// Returns the node reference to the created value.
    pub fn new_global<R: 'static>(
        &mut self,
        f: impl FnOnce(&mut DeltaBuilder<N, E>) -> R,
    ) -> (NodeRef, R) {
        let (created, res) = {
            let mut builder = DeltaBuilder::new(self.ctx);
            let res = f(&mut builder);
            (builder.build(), res)
        };

        //add the created node to our region
        self.node_mut().body.nodes.insert(created);
        (created, res)
    }

    ///Allows you to add a function ([λ-Node](crate::nodes::LambdaNode)) to the translation unit.
    ///
    /// If `export` is `Some(label)`, it will be added to the exported functions of the translation unit under the name of `label`.
    ///
    /// Returns the node reference to the created value.
    pub fn new_function<R: 'static>(
        &mut self,
        export: bool,
        f: impl FnOnce(&mut LambdaBuilder<N, E>) -> R,
    ) -> (NodeRef, R) {
        let (created, res) = {
            let mut builder = LambdaBuilder::new(self.ctx);
            let res = f(&mut builder);
            (builder.build(), res)
        };

        //add the created node to our region
        self.node_mut().body.nodes.insert(created);

        if export {
            let export_port = self.export();
            self.ctx
                .connect(
                    OutportLocation {
                        node: created,
                        output: OutputType::LambdaDecleration,
                    },
                    export_port,
                    E::value_edge(),
                )
                .unwrap();
        }
        (created, res)
    }
    ///lets you change the behaviour of this node.
    pub fn on_region<R>(&mut self, f: impl FnOnce(&mut RegionBuilder<N, E, OmegaNode>) -> R) -> R {
        //setup the builder for the region
        let mut builder = RegionBuilder::new(self.ctx, 0, self.node_ref);
        f(&mut builder)
    }
}
