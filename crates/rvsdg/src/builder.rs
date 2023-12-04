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
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
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

    ///Connects the port `src` to this region's `dst_ty` result. `dst` must be Result, ExitVariableResult or RecursionVariableResult.
    ///
    /// The connection is a value edge by default.
    pub fn connect_to_result(
        &mut self,
        src: OutportLocation,
        dst_ty: InputType,
    ) -> Result<EdgeRef, GraphError> {
        if !dst_ty.is_result() {
            return Err(GraphError::ExpectedResult(dst_ty));
        }
        let parent = self.parent();
        self.ctx_mut()
            .connect(src, parent.as_inport_location(dst_ty), E::value_edge())
    }

    ///Connects `src_ty` type argument of this region to the `dst` port.
    ///
    /// The connection is a value-edge by default.
    pub fn connect_arg_to(
        &mut self,
        src_ty: OutputType,
        dst: InportLocation,
    ) -> Result<EdgeRef, GraphError> {
        if !src_ty.is_argument() {
            return Err(GraphError::ExpectedArgument(src_ty));
        }

        let parent = self.parent();
        self.ctx_mut()
            .connect(parent.as_outport_location(src_ty), dst, E::value_edge())
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
    ///
    /// Will fail if the `callable_src` has a producer (connected node), but that producer is not an callabale node (λ|ϕ).
    /// In case of an defined producer, the call will also fail, if the argument count does not match.
    pub fn call(
        &mut self,
        callable_src: OutportLocation,
        arguments: &[OutportLocation],
    ) -> Result<(NodeRef, TinyVec<[EdgeRef; 3]>), GraphError> {
        let (apply_node, is_defined) =
            if let Some(funct_def) = self.ctx.find_callabel_def(callable_src.clone()) {
                if let Node::Lambda(l) = self.ctx.node(funct_def) {
                    (ApplyNode::new_for_lambda(l), true)
                } else {
                    println!("Callable for phi not implemented!");
                    return Err(GraphError::NotCallable(callable_src.node));
                }
            } else {
                (ApplyNode::new(), false)
            };

        //insert into graph
        let apply_node_ref = self.ctx.new_node(Node::Apply(apply_node));
        //add to block
        self.region_mut().nodes.insert(apply_node_ref);

        //connect function input and arguments, collect created edges

        let mut arg_edges = TinyVec::default();
        let call_edge = self
            .ctx_mut()
            .connect(
                callable_src,
                InportLocation {
                    node: apply_node_ref,
                    input: InputType::Input(0),
                },
                E::value_edge(),
            )
            .unwrap();
        arg_edges.push(call_edge);

        //connect function argument.
        for (idx, arg_src) in arguments.iter().enumerate() {
            //If producer is not defined, push all args we connect to.
            if !is_defined {
                if let Node::Apply(an) = self.ctx.node_mut(apply_node_ref) {
                    let at = an.add_input();
                    assert!(idx == at - 1, "argument count missmatch");
                } else {
                    panic!("Undefined apply node ref");
                }
            }

            let edg = self
                .ctx_mut()
                .connect(
                    arg_src.clone(),
                    InportLocation {
                        node: apply_node_ref,
                        input: InputType::Input(1 + idx),
                    },
                    E::value_edge(),
                )
                .unwrap();
            arg_edges.push(edg);
        }

        Ok((apply_node_ref, arg_edges))
    }
}
