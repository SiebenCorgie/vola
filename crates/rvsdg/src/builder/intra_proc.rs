//! private mod declaring intra procedural node builder.
//! Those are simple nodes, gamma nodes, and theta nodes.

use tinyvec::ArrayVec;

use crate::{
    edge::{Edge, LangEdge, PortIndex},
    nodes::{GammaNode, LangNode, Node},
    NodeRef, Rvsdg,
};

///[γ-region](crate::nodes::GammaNode) builder.
pub struct GammaBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    ///The node that is being build
    node: GammaNode,
    ///Preallocated invalid node ref
    node_ref: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> GammaBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>) -> Self {
        let body = ctx.new_region();
        let node_ref = ctx.new_node(Node::Invalid);
        GammaBuilder {
            ctx,
            node: GammaNode::new(),
            node_ref,
        }
    }

    ///Creates a new branch for this decision point.
    pub fn new_branch(mut self) -> (Self, usize) {
        self.node.add_region(self.ctx);
        let idx = self.node.regions.len() - 1;
        (self, idx)
    }

    ///Adds a new variable that is used as an argument to all branches.
    pub fn add_entry_variable(mut self) -> (Self, usize) {
        self.node.add_entry_var(self.ctx);
        let idx = self.node.entry_var_count - 1;
        (self, idx)
    }

    ///Adds a new variable that is used as a result of all branches.
    pub fn add_exit_variable(mut self) -> (Self, usize) {
        self.node.add_exit_var(self.ctx);
        let idx = self.node.exit_var_count - 1;
        (self, idx)
    }

    ///Connects the gamma nodes predicate port to `predicate`'s `port_index`.
    pub fn connect_criteria(mut self, predicate: NodeRef, port_index: PortIndex) -> Self {
        let edge = self.ctx.new_edge(Edge {
            src: predicate,
            src_index: port_index.clone(),
            dst: self.node_ref,
            dst_index: PortIndex::Predicate,
            ty: E::value_edge(),
        });

        self.ctx
            .port_mut(predicate, port_index)
            .unwrap()
            .edges
            .push(edge);
        self.node.inputs[0].edges.push(edge);

        self
    }

    ///Connects the `src`'nodes `port_index` to the `entry`-th entry variable of this node.
    pub fn connect_entry(mut self, src: NodeRef, port_index: PortIndex, entry: usize) -> Self {
        let entry_index = PortIndex::EntryVar {
            var_index: entry,
            region_index: None,
        };
        let my_input_location = entry_index
            .into_location(&Node::Gamma::<N>(self.node.clone()))
            .unwrap();
        let edge = self.ctx.new_edge(Edge {
            src,
            src_index: port_index.clone(),
            dst: self.node_ref,
            dst_index: entry_index,
            //FIXME: is this always right, or could state also be a edge. Intuitively not, since we'd have to assure that state is resolved before
            // the branch is reached. However, we could also build some really nice "resolve state if(xy), otherwise, take you time"-style scheduling.
            ty: E::value_edge(),
        });

        //Notify both nodes
        self.ctx.port_mut(src, port_index).unwrap().edges.push(edge);

        self.node.inputs[my_input_location.unwrap_input()]
            .edges
            .push(edge);

        self
    }

    ///Connects the `src`'nodes `port_index` to the `exit`-th exit variable of this node.
    pub fn connect_exit(mut self, dst: NodeRef, port_index: PortIndex, exit: usize) -> Self {
        let exit_index = PortIndex::ExitVar {
            var_index: exit,
            region_index: None,
        };
        let my_output_location = exit_index
            .into_location(&Node::Gamma::<N>(self.node.clone()))
            .unwrap();
        let edge = self.ctx.new_edge(Edge {
            src: self.node_ref,
            src_index: exit_index.clone(),
            dst,
            dst_index: port_index.clone(),
            //FIXME: is this always right, or could state also be a edge. Intuitively not, since we'd have to assure that state is resolved before
            // the branch is reached. However, we could also build some really nice "resolve state if(xy), otherwise, take you time"-style scheduling.
            ty: E::value_edge(),
        });

        //Notify both nodes
        self.ctx.port_mut(dst, port_index).unwrap().edges.push(edge);

        self.node.outputs[my_output_location.unwrap_output()]
            .edges
            .push(edge);

        self
    }
}
