//! RVSDG building utilities. If your source is based on structured-control flow, the standart RVSDG builder should give
//! you everything needed to setup a RVSDG representation.
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

mod inter_proc;
mod intra_proc;
use crate::{
    edge::{Edge, InportLocation, InputType, LangEdge, OutportLocation},
    err::{BuilderError, GraphError},
    nodes::{ApplyNode, LangNode, Node},
    region::Region,
    EdgeRef, NodeRef, Rvsdg,
};
pub use inter_proc::{DeltaBuilder, LambdaBuilder, OmegaBuilder, PhiBuilder};
pub use intra_proc::{GammaBuilder, ThetaBuilder};
use tinyvec::ArrayVec;

///Probably the most used builder. Represents a simple [Region](crate::region::Region) within one of the higher level nodes.
pub struct RegionBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    region_ref: &'a mut Region,
    parent_node: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> RegionBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>, rref: &'a mut Region, parent_node: NodeRef) -> Self {
        RegionBuilder {
            ctx,
            region_ref: rref,
            parent_node,
        }
    }

    pub fn ctx(&self) -> &Rvsdg<N, E> {
        &self.ctx
    }

    pub fn ctx_mut(&mut self) -> &mut Rvsdg<N, E> {
        self.ctx
    }

    ///Returns the parent node of this region. Note that we might not be the only region of that node.
    ///
    /// The parent also might not (yet) be hooked up completely in the graph, as specially when calling this from
    /// a nested builder context.
    pub fn parent(&self) -> NodeRef {
        self.parent_node
    }

    ///Adds `node` to this region, returns the ref it was registered as
    pub fn insert_node(&mut self, node: N) -> NodeRef {
        let nref = self.ctx.new_node(Node::Simple(node));
        self.region_ref.nodes.insert(nref);

        nref
    }

    ///Connects two nodes. Fails if either the port types don't match up, or the nodes are not part of this region.
    ///
    /// If successful, returns the reference to the edge that is registered.
    ///
    /// You can use [disconnect](crate::Rvsdg::disconnect) do disconnect nodes using an edge ref, as well as [crate::EdgeRef::disconnect].
    pub fn connect(
        &mut self,
        src: OutportLocation,
        dst: InportLocation,
        edge_type: E,
    ) -> Result<EdgeRef, BuilderError> {
        //Check that its legal to connect the nodes
        if !self.region_ref.nodes.contains(&src.node) || src.node != self.parent() {
            return Err(BuilderError::NodeNotInRegion(src.node));
        }
        if !self.region_ref.nodes.contains(&dst.node) || dst.node != self.parent() {
            return Err(BuilderError::NodeNotInRegion(dst.node));
        }

        //TODO: check that the port_types are okay? Currently can't think of an invariant.

        //all right, hookup
        let edge = self.ctx.new_edge(Edge {
            src: src.clone(),
            dst: dst.clone(),
            ty: edge_type,
        });

        //Notify ports of connection
        self.ctx
            .node_mut(src.node)
            .outport_mut(&src.output)
            .unwrap()
            .edges
            .push(edge);

        //If an input was already set, notify the old input of the disconnect.
        let mut was_replaced = None;
        if let Some(old_edge) = self.ctx.node(dst.node).inport(&dst.input).unwrap().edge {
            self.ctx.disconnect(old_edge);
            was_replaced = Some(BuilderError::EdgeOverwrite(old_edge));
        }

        debug_assert!(
            self.ctx
                .node(dst.node)
                .inport(&dst.input)
                .unwrap()
                .edge
                .is_none(),
            "No edge should be connected to input at this point!"
        );
        self.ctx
            .node_mut(dst.node)
            .inport_mut(&dst.input)
            .unwrap()
            .edge = Some(edge);

        if let Some(err) = was_replaced {
            return Err(err);
        } else {
            Ok(edge)
        }
    }

    ///Creates a new `node`, creates enough inputs to connect all `src` list entries to the node.
    /// Returns the list of edges that where created. Use that to change the edge type, by default all edges are _value edges_.
    pub fn connect_node(
        &mut self,
        node: N,
        src: &[OutportLocation],
    ) -> Result<(NodeRef, ArrayVec<[EdgeRef; 3]>), BuilderError> {
        let created_node = self.insert_node(node);
        let mut edges = ArrayVec::default();
        for (dst_idx, src) in src.into_iter().enumerate() {
            edges.push(self.connect(
                src.clone(),
                InportLocation {
                    node: created_node,
                    input: InputType::Input(dst_idx),
                },
                E::value_edge(),
            )?)
        }

        Ok((created_node, edges))
    }

    ///Spawn a new loop-node/[θ-Node](crate::nodes::ThetaNode) in this region. Returns the reference under which the loop is created.
    pub fn new_loop(&mut self, building: impl FnOnce(&mut ThetaBuilder<N, E>)) -> NodeRef {
        let created = {
            let mut builder = ThetaBuilder::new(self.ctx);
            building(&mut builder);
            builder.build()
        };
        //add to our region
        self.region_ref.nodes.insert(created);
        created
    }

    ///Spawns a new decision-node/[γ-Node](crate::node::GammaNode) in this region. Returns the reference under which the decision is created.
    pub fn new_decission(&mut self, building: impl FnOnce(&mut GammaBuilder<N, E>)) -> NodeRef {
        let created = {
            let mut builder = GammaBuilder::new(self.ctx);
            building(&mut builder);
            builder.build()
        };
        //add to our region
        self.region_ref.nodes.insert(created);
        created
    }

    ///Allows you to spawn a new function-node/[λ-Node](crates::nodes::LambdaNode) in this region. Returns the reference under which the function is created.
    pub fn new_function(&mut self, building: impl FnOnce(&mut LambdaBuilder<N, E>)) -> NodeRef {
        let created = {
            let mut builder = LambdaBuilder::new(self.ctx);
            building(&mut builder);
            builder.build()
        };
        //add to our region
        self.region_ref.nodes.insert(created);
        created
    }

    ///Allows you to spawn a new recursive-node/[ϕ-Node](crates::nodes::PhiNode) in this region. Returns the reference under which the function is created.
    pub fn new_recursive_region(
        &mut self,
        building: impl FnOnce(&mut PhiBuilder<N, E>),
    ) -> NodeRef {
        let created = {
            let mut builder = PhiBuilder::new(self.ctx);
            building(&mut builder);
            builder.build()
        };
        //add to our region
        self.region_ref.nodes.insert(created);
        created
    }

    ///Allows you to spawn a new global-value/[δ-Node](crates::nodes::DeltaNode) in this region. Returns the reference under which the function is created.
    pub fn new_global(&mut self, building: impl FnOnce(&mut DeltaBuilder<N, E>)) -> NodeRef {
        let created = {
            let mut builder = DeltaBuilder::new(self.ctx);
            building(&mut builder);
            builder.build()
        };
        //add to our region
        self.region_ref.nodes.insert(created);
        created
    }

    ///Creates an apply node that calls `function` from `src_port` (either a lambda node and its output-port, or a context variable from the parent node).
    /// adds all `arguments` to the call. Those should/must match up with the signature of the `function`.
    ///
    /// The resulting apply node will have the amount of output-ports the `function` declaration has as results.
    pub fn call_function(
        &mut self,
        function_src: OutportLocation,
        arguments: &[OutportLocation],
    ) -> Result<(NodeRef, ArrayVec<[EdgeRef; 3]>), GraphError> {
        let apply_node = if let Some(funct_def) = self.ctx.find_callabel_def(function_src.clone()) {
            if let Node::Lambda(l) = self.ctx.node(funct_def) {
                ApplyNode::new_for_lambda(l)
            } else {
                return Err(GraphError::NotCallable(function_src.node));
            }
        } else {
            return Err(GraphError::NotCallable(function_src.node));
        };

        //insert into graph
        let node_ref = self.ctx.new_node(Node::Apply(apply_node));
        //add to block
        self.region_ref.nodes.insert(node_ref);

        //connect function input and arguments, collect created edges

        let mut arg_edges = ArrayVec::default();
        let call_edge = self
            .connect(
                function_src,
                InportLocation {
                    node: node_ref,
                    input: InputType::Input(0),
                },
                E::value_edge(),
            )
            .unwrap();
        arg_edges.push(call_edge);

        //connect function argument.
        for (idx, arg_src) in arguments.iter().enumerate() {
            let edg = self
                .connect(
                    arg_src.clone(),
                    InportLocation {
                        node: node_ref,
                        input: InputType::Input(1 + idx),
                    },
                    E::value_edge(),
                )
                .unwrap();
            arg_edges.push(edg);
        }

        Ok((node_ref, arg_edges))
    }
}
