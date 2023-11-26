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
use std::marker::PhantomData;

use crate::{
    edge::{Edge, InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::{BuilderError, GraphError},
    nodes::{ApplyNode, LangNode, Node, StructuralNode},
    region::Region,
    EdgeRef, NodeRef, Rvsdg,
};
pub use inter_proc::{DeltaBuilder, LambdaBuilder, OmegaBuilder, PhiBuilder};
pub use intra_proc::{GammaBuilder, ThetaBuilder};
use tinyvec::TinyVec;

///Probably the most used builder. Represents a simple [Region](crate::region::Region) within one of the higher level nodes.
pub struct RegionBuilder<'a, N: LangNode + 'static, E: LangEdge + 'static, PARENT: StructuralNode> {
    ctx: &'a mut Rvsdg<N, E>,
    parent_region_index: usize,
    parent_ref: NodeRef,
    parent: PhantomData<PARENT>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static, PARENT: StructuralNode>
    RegionBuilder<'a, N, E, PARENT>
{
    pub fn new(ctx: &'a mut Rvsdg<N, E>, parent_region_index: usize, parent_ref: NodeRef) -> Self {
        RegionBuilder {
            ctx,
            parent: PhantomData,
            parent_region_index,
            parent_ref,
        }
    }

    pub fn ctx(&self) -> &Rvsdg<N, E> {
        &self.ctx
    }

    pub fn ctx_mut(&mut self) -> &mut Rvsdg<N, E> {
        self.ctx
    }

    pub fn region(&self) -> &Region {
        &self.ctx.node(self.parent_ref).regions()[self.parent_region_index]
    }

    pub fn region_mut(&mut self) -> &mut Region {
        let p = self.parent_ref;
        let reg_idx = self.parent_region_index;
        &mut self.ctx_mut().node_mut(p).regions_mut()[reg_idx]
    }

    ///Adds `node` to this region, returns the ref it was registered as
    pub fn insert_node(&mut self, node: N) -> NodeRef {
        let nref = self.ctx.new_node(Node::Simple(node));
        self.region_mut().nodes.insert(nref);

        nref
    }

    pub fn parent(&self) -> NodeRef {
        self.parent_ref
    }

    /*
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
            //NOTE: This is a different connect compared to Rvsdg<N,E>::connect, since
            // self.parent() is usually not yet inserted into its parent.
            //
            // we therefore skip some checks the OG function does, at the expense of possibly invalid edges, which
            // would be detected later
            // TODO: fix that // find all invariant and make it sound?

            //Check if its either a reference to our parent (which should be an argument or result)
            // or, if not, if its within the region
            if src.node != self.parent() {
                if !self.region().nodes.contains(&src.node) {
                    return Err(BuilderError::NodeNotInRegion(src.node));
                }

                //is in region, so check if the port itself exists
                if let None = self.ctx.node(src.node).outport(&src.output) {
                    return Err(BuilderError::ExpectedOutport(src));
                }
            } else {
                if let OutputType::Output(_) = src.output {
                    return Err(BuilderError::Other("If connecting to the parent node, Output is not a valid port, since its outside of the region".to_owned()));
                }
            }

            if dst.node != self.parent() {
                if !self.region().nodes.contains(&dst.node) {
                    return Err(BuilderError::NodeNotInRegion(dst.node));
                }

                if let None = self.ctx.node(dst.node).inport(&dst.input) {
                    return Err(BuilderError::ExpectedInport(dst));
                }
            } else {
                if let InputType::Input(_) = dst.input {
                    return Err(BuilderError::Other("If connecting to the parent node, Input is not a valid port, since its outside of the region".to_owned()));
                }
            }

            //all right, hookup
            let edge = self.ctx.new_edge(Edge {
                src: src.clone(),
                dst: dst.clone(),
                ty: edge_type,
            });

            //Notify ports of connection. This is the spicy part thats wildly different to the OG connect function.
            // Since we can't be sure that the node already exists in the nodes map of the Rvsdg. So we have to check
            // where to we are connecting exactly, and use the appropriate access
            if src.node != self.parent() {
                //normal write
                self.ctx
                    .node_mut(src.node)
                    .outport_mut(&src.output)
                    .unwrap()
                    .edges
                    .push(edge);
            } else {
                //writing to our self. we can be sure though that this is not an output port (which was checked earlier)
                if let Some(port) = self.ctx.node_mut(self.parent_ref).outport_mut(&src.output) {
                    port.edges.push(edge)
                }
            }

            if dst.node != self.parent() {
                debug_assert!(
                    self.ctx
                        .node_mut(dst.node)
                        .inport_mut(&dst.input)
                        .unwrap()
                        .edge
                        .is_none(),
                    "the input should not be connected already"
                );

                //normal write
                self.ctx
                    .node_mut(dst.node)
                    .inport_mut(&dst.input)
                    .unwrap()
                    .edge = Some(edge);
            } else {
                //writing to our self. we can be sure though that this is not an output port (which was checked earlier)
                let pref = self.parent_ref;
                if let Some(port) = self.ctx_mut().node_mut(pref).inport_mut(&dst.input) {
                    debug_assert!(
                        port.edge.is_none(),
                        "the input should not be connected already!"
                    );
                    port.edge = Some(edge);
                }
            }

            Ok(edge)
        }
    */
    ///Creates a new `node`, creates enough inputs to connect all `src` list entries to the node.
    /// Returns the list of edges that where created. Use that to change the edge type, by default all edges are _value edges_.
    pub fn connect_node(
        &mut self,
        node: N,
        src: &[OutportLocation],
    ) -> Result<(NodeRef, TinyVec<[EdgeRef; 3]>), GraphError> {
        let created_node = self.insert_node(node);
        let mut edges = TinyVec::default();
        for (dst_idx, src_port) in src.into_iter().enumerate() {
            edges.push(self.ctx_mut().connect(
                src_port.clone(),
                InportLocation {
                    node: created_node,
                    input: InputType::Input(dst_idx),
                },
                E::value_edge(),
            )?);
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
        self.region_mut().nodes.insert(created);
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
        self.region_mut().nodes.insert(created);
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
        self.region_mut().nodes.insert(created);
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
        self.region_mut().nodes.insert(created);
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
        self.region_mut().nodes.insert(created);
        created
    }

    ///Creates an apply node that calls `function` from `src_port` (either a lambda node and its output-port, or a context variable from the parent node).
    /// adds all `arguments` to the call. Those should/must match up with the signature of the `function`.
    ///
    /// The resulting apply node will have the amount of output-ports the `function` declaration has as results.
    pub fn call(
        &mut self,
        callable_src: OutportLocation,
        arguments: &[OutportLocation],
    ) -> Result<(NodeRef, TinyVec<[EdgeRef; 3]>), GraphError> {
        let apply_node = if let Some(funct_def) = self.ctx.find_callabel_def(callable_src.clone()) {
            if let Node::Lambda(l) = self.ctx.node(funct_def) {
                ApplyNode::new_for_lambda(l)
            } else {
                println!("Callable for phi not implemented!");
                return Err(GraphError::NotCallable(callable_src.node));
            }
        } else {
            return Err(GraphError::NotCallable(callable_src.node));
        };

        //insert into graph
        let node_ref = self.ctx.new_node(Node::Apply(apply_node));
        //add to block
        self.region_mut().nodes.insert(node_ref);

        //connect function input and arguments, collect created edges

        let mut arg_edges = TinyVec::default();
        let call_edge = self
            .ctx_mut()
            .connect(
                callable_src,
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
                .ctx_mut()
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
