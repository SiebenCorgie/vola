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
    edge::{Edge, LangEdge, PortIndex, PortLocation},
    err::{BuilderError, GraphError},
    nodes::{ApplyNode, LangNode, Node},
    EdgeRef, NodeRef, RegionRef, Rvsdg,
};
pub use inter_proc::{DeltaBuilder, LambdaBuilder, OmegaBuilder, PhiBuilder};
pub use intra_proc::{GammaBuilder, ThetaBuilder};
use tinyvec::ArrayVec;

///Probably the most used builder. Represents a simple [Region](crate::region::Region) within one of the higher level nodes.
pub struct RegionBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    ctx: &'a mut Rvsdg<N, E>,
    region_ref: RegionRef,
    parent_node: NodeRef,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> RegionBuilder<'a, N, E> {
    pub fn new(ctx: &'a mut Rvsdg<N, E>, rref: RegionRef, parent_node: NodeRef) -> Self {
        RegionBuilder {
            ctx,
            region_ref: rref,
            parent_node,
        }
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
        self.ctx.region_mut(self.region_ref).nodes.insert(nref);

        nref
    }

    ///Connects two nodes. Fails if either the port types don't match up, or the nodes are not part of this region.
    ///
    /// If successful, returns the reference to the edge that is registered.
    ///
    /// You can use [disconnect](crate::Rvsdg::disconnect) do disconnect nodes using an edge ref, as well as [crate::EdgeRef::disconnect].
    pub fn connect(
        &mut self,
        src: NodeRef,
        src_port: PortIndex,
        dst: NodeRef,
        dst_port: PortIndex,
        edge_type: E,
    ) -> Result<EdgeRef, BuilderError> {
        //Check that its legal to connect the nodes
        if !self.ctx.region(self.region_ref).nodes.contains(&src) || src != self.parent() {
            return Err(BuilderError::NodeNotInRegion(src));
        }
        if !self.ctx.region(self.region_ref).nodes.contains(&dst) || dst != self.parent() {
            return Err(BuilderError::NodeNotInRegion(dst));
        }

        //check that the port_types are okay
        match src_port.into_location(self.ctx.node(src)) {
            Some(PortLocation::Arguments { .. }) | Some(PortLocation::Outputs(_)) => {}
            _ => {
                return Err(BuilderError::UnexpectedPortIndex {
                    node: src,
                    port: src_port,
                    additional_info:
                        "Expected Argument or Output for the source node of a connection".to_owned(),
                })
            }
        }
        match dst_port.into_location(self.ctx.node(dst)) {
            Some(PortLocation::Results { .. }) | Some(PortLocation::Inputs(_)) => {}
            _ => {
                return Err(BuilderError::UnexpectedPortIndex {
                    node: dst,
                    port: dst_port,
                    additional_info:
                        "Expected Result or Input for the destination node of a connection"
                            .to_owned(),
                })
            }
        }

        //all right, hookup
        let edge = self.ctx.new_edge(Edge {
            src,
            src_index: src_port,
            dst,
            dst_index: dst_port,
            ty: edge_type,
        });

        Ok(edge)
    }

    ///Creates a new `node`, creates enough inputs to connect all `src` list entries to the node.
    /// Returns the list of edges that where created. Use that to change the edge type, by default all edges are _value edges_.
    pub fn connect_node(
        &mut self,
        node: N,
        src: &[(NodeRef, PortIndex)],
    ) -> Result<(NodeRef, ArrayVec<[EdgeRef; 3]>), BuilderError> {
        let created_node = self.insert_node(node);
        let mut edges = ArrayVec::default();
        for (dst_idx, (src_node, src_port)) in src.into_iter().enumerate() {
            edges.push(self.connect(
                *src_node,
                src_port.clone(),
                created_node,
                PortIndex::Input(dst_idx),
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
        self.ctx.region_mut(self.region_ref).nodes.insert(created);
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
        self.ctx.region_mut(self.region_ref).nodes.insert(created);
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
        self.ctx.region_mut(self.region_ref).nodes.insert(created);
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
        self.ctx.region_mut(self.region_ref).nodes.insert(created);
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
        self.ctx.region_mut(self.region_ref).nodes.insert(created);
        created
    }

    ///Creates an apply node that calls `function` from `src_port` (either a lambda node and its output-port, or a context variable from the parent node).
    /// adds all `arguments` to the call. Those should/must match up with the signature of the `function`.
    ///
    /// The resulting apply node will have the amount of output-ports the `function` declaration has as results.
    pub fn call_function(
        &mut self,
        function: NodeRef,
        src_port: PortIndex,
        arguments: ArrayVec<[(NodeRef, PortIndex); 3]>,
    ) -> Result<NodeRef, GraphError> {
        let apply_node =
            if let Some(funct_def) = self.ctx.find_callabel_def(function, src_port.clone()) {
                if let Node::Lambda(l) = self.ctx.node(funct_def) {
                    ApplyNode::new_for_lambda(self.ctx, l)
                } else {
                    return Err(GraphError::NotCallable(function));
                }
            } else {
                return Err(GraphError::NotCallable(function));
            };

        //insert into graph
        let node_ref = self.ctx.new_node(Node::Apply(apply_node));
        //add to block
        self.ctx.region_mut(self.region_ref).nodes.insert(node_ref);

        //connect function input
        self.connect(
            function,
            src_port,
            node_ref,
            PortIndex::Input(0),
            E::value_edge(),
        )
        .unwrap();

        //connect function argument.
        for (idx, (arg_node, argport)) in arguments.iter().enumerate() {
            self.connect(
                *arg_node,
                argport.clone(),
                node_ref,
                PortIndex::Input(1 + idx),
                E::value_edge(),
            )
            .unwrap();
        }

        Ok(node_ref)
    }
}
