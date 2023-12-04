//! private mod declaring intra procedural node builder.
//! Those are simple nodes, gamma nodes, and theta nodes.

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::{GammaNode, LangNode, Node, ThetaNode},
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

    ///Creates a new branch for this decision point. Next to the builder the branch index is also supplied to the `branch_builder` callback.
    /// This index should be used whenever referencing entry/exit variables within this branch.
    ///
    /// # Example
    ///
    /// ```rust
    ///gamma.new_branch(|builder, branch_index|{
    ///    //Connects the first entry variable of this branch to the first exit variable
    ///    builder.ctx_mut().connect(
    ///        OutportLocation{
    ///            node: parent,
    ///            output: OutputType::EntryVariableArgument{
    ///                branch: branch_index,
    ///                entry_variable: 0
    ///            }
    ///        },
    ///        InportLocation{
    ///            node: parent,
    ///            input: InputType::ExitVariableResult{
    ///                branch: 0,
    ///                exit_variable: ex0
    ///            }
    ///        }
    ///        E::value_edge()
    ///   );
    ///})
    ///
    /// ```
    pub fn new_branch<R>(
        &mut self,
        branch_builder: impl FnOnce(&mut RegionBuilder<N, E, GammaNode>, usize) -> R,
    ) -> (usize, R) {
        self.node_mut().add_region();
        let idx = self.node().regions.len() - 1;
        let mut builder = RegionBuilder::new(self.ctx, idx, self.node_ref);
        let res = branch_builder(&mut builder, idx);

        (idx, res)
    }

    ///Adds a new variable that is used as an argument to all branches.
    ///
    /// Returns the ev_input port, as well as the index of that port in the regions.
    /// use [OutputType::EntryVariableArgument](crate::edge::OutputType)'s entry_variable field to reference the created ev.
    pub fn add_entry_variable(&mut self) -> (InportLocation, usize) {
        self.node_mut().add_entry_var();
        let idx = self.node().entry_var_count - 1;
        (
            InportLocation {
                node: self.node_ref,
                input: InputType::EntryVariableInput(idx),
            },
            idx,
        )
    }

    ///Adds a new variable that is used as a result of all branches.
    ///
    /// Returns the ex_output port, as well as the index of that port in the regions.
    /// use [InputType::ExitVariableResult](crate::edge::InputType)'s exit_variable field to reference the created ev.
    pub fn add_exit_variable(&mut self) -> (usize, OutportLocation) {
        self.node_mut().add_exit_var();
        let idx = self.node().exit_var_count - 1;
        (
            idx,
            OutportLocation {
                node: self.node_ref,
                output: OutputType::ExitVariableOutput(idx),
            },
        )
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
    ///Returns the tuple (Input, Argument, Result, Output) referencing this loop variable.
    pub fn add_loop_variable(
        &mut self,
    ) -> (
        InportLocation,
        OutportLocation,
        InportLocation,
        OutportLocation,
    ) {
        let created_at_idx = self.node_mut().add_loop_variable();
        let input = InportLocation {
            node: self.node_ref,
            input: InputType::Input(created_at_idx),
        };
        let result = InportLocation {
            node: self.node_ref,
            input: InputType::LoopVariableResult(created_at_idx),
        };
        let arg = OutportLocation {
            node: self.node_ref,
            output: OutputType::Argument(created_at_idx),
        };
        let out = OutportLocation {
            node: self.node_ref,
            output: OutputType::Output(created_at_idx),
        };
        (input, arg, result, out)
    }
}
