//! # RVSDG
//!
//! Vola's [RVSDG](https://dl.acm.org/doi/abs/10.1145/3391902) implementation.
//!
//! The crate tries to stay independent of Vola and implements the paper in a *generic* way. Nodes and edges can be defined
//! by the user. This allows you to use the RVSDG implementation in different high and low level scenarios.
//!
//! Naming follows the source paper, type aliases are included, to make reading the code easier. After all, _θ-Node ~ LoopNode_. Those _common-language_ names
//! might sometimes not be as precise though as their _real_ names.
//!
//! We expose some helper types and functions to get up to speed easily for *normal* languages. This includes
//! a generic type system for nodes and edges as well builder for common constructs like loops, if-else nodes and function
//! calls. Those things reside in the [common] module.
use std::fmt::Display;

use ahash::AHashMap;
use builder::OmegaBuilder;
use edge::{Edge, InportLocation, InputType, LangEdge, OutportLocation, OutputType};
use err::GraphError;
use nodes::{LangNode, Node, NodeType, OmegaNode};
use region::{Region, RegionLocation};
use slotmap::{new_key_type, SlotMap};

pub mod analyze;
pub mod attrib;
pub mod builder;
pub mod common;
pub mod edge;
pub mod err;
pub mod nodes;
pub mod region;
pub mod util;
pub mod verify;

new_key_type! {pub struct NodeRef;}
impl Display for NodeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NodeRef({:?})", self.0)
    }
}

impl NodeRef {
    pub fn as_inport_location(self, port_type: InputType) -> InportLocation {
        InportLocation {
            node: self,
            input: port_type,
        }
    }
    pub fn as_outport_location(self, port_type: OutputType) -> OutportLocation {
        OutportLocation {
            node: self,
            output: port_type,
        }
    }
    pub fn as_ffi(&self) -> u64 {
        self.0.as_ffi()
    }
}

new_key_type! {pub struct EdgeRef;}
impl Display for EdgeRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "EdgeRef({:?})", self.0)
    }
}

impl EdgeRef {
    ///removes this edge from the graph (if possible). See [Rvsdg::disconnect] for further documentation.
    pub fn disconnect<N: LangNode + 'static, E: LangEdge + 'static>(
        self,
        ctx: &mut Rvsdg<N, E>,
    ) -> Result<(), GraphError> {
        ctx.disconnect(self)
    }

    pub fn as_ffi(&self) -> u64 {
        self.0.as_ffi()
    }
}

///The RVSDG state. Contains the actual nodes as well as edge definitions.
///
/// `N`: being your language's Nodes, and `E` being the edges. Note that RVSDGs usually have at least two
/// edges types. A _value-typed_, representing a data dependency, and _state-typed_, representing execution ordering constrains.
///
/// However, you are free to represent any kind of other dependency.
///
/// For ease of use you can always type N as `Box<dyn SomeNodeTrait + 'static>`.
pub struct Rvsdg<N: LangNode + 'static, E: LangEdge + 'static> {
    pub(crate) nodes: SlotMap<NodeRef, Node<N>>,
    pub(crate) edges: SlotMap<EdgeRef, Edge<E>>,
    ///Entrypoint of this translation unit.
    pub(crate) omega: NodeRef,
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    pub fn new() -> Self {
        //Pre-create the omega node we use to track imports/exports
        let mut nodes = SlotMap::default();
        let omega = nodes.insert(Node {
            node_type: NodeType::Omega(OmegaNode {
                body: Region::new(),
            }),
            //Omega will never have a parent
            parent: None,
        });

        Rvsdg {
            edges: SlotMap::default(),
            nodes,
            omega,
        }
    }

    ///Allows you to modify the graph from its entry point, the [ω-Node](crate::nodes::OmegaNode).
    pub fn on_omega_node(&mut self, f: impl FnOnce(&mut OmegaBuilder<N, E>)) {
        //we do this by replacing the actual omega node, setup the builder, call it,
        // and then substituding it again.
        let omega_ref = self.omega;
        let mut builder = OmegaBuilder {
            ctx: self,
            node_ref: omega_ref,
        };

        //call the user closure
        f(&mut builder)
    }

    ///Returns the reference to the translation unit / [ω-Node](crate::nodes::OmegaNode).
    pub fn entry_node(&self) -> NodeRef {
        self.omega
    }

    ///Creates a new node for `node_type`. Returns the reference to that node in `self`.
    pub fn new_node(&mut self, node_type: NodeType<N>) -> NodeRef {
        self.nodes.insert(Node {
            node_type,
            parent: None,
        })
    }

    ///Returns reference to the node, assuming that it exists. Panics if it does not exist.
    pub fn node(&self, nref: NodeRef) -> &Node<N> {
        self.nodes.get(nref).as_ref().unwrap()
    }

    ///Returns reference to the node, assuming that it exists. Panics if it does not exist.
    pub fn node_mut(&mut self, nref: NodeRef) -> &mut Node<N> {
        self.nodes.get_mut(nref).unwrap()
    }

    pub fn on_node<T: 'static>(&self, n: NodeRef, f: impl FnOnce(&Node<N>) -> T) -> T {
        f(self.node(n))
    }

    pub fn on_node_mut<T: 'static>(&mut self, n: NodeRef, f: impl FnOnce(&mut Node<N>) -> T) -> T {
        f(self.node_mut(n))
    }

    pub fn new_edge(&mut self, edge: Edge<E>) -> EdgeRef {
        self.edges.insert(edge)
    }

    ///Returns reference to the edge, assuming that it exists. Panics if it does not exist.
    pub fn edge(&self, eref: EdgeRef) -> &Edge<E> {
        self.edges.get(eref).as_ref().unwrap()
    }

    ///Returns reference to the edge, assuming that it exists. Panics if it does not exist.
    pub fn edge_mut(&mut self, eref: EdgeRef) -> &mut Edge<E> {
        self.edges.get_mut(eref).unwrap()
    }

    ///Returns the region at the given location, if it exists.
    /// Returns none if either the `region.node` does not exist, or the `region.region_index`-th region on that node does not exist.
    pub fn region(&self, region: &RegionLocation) -> Option<&Region> {
        if let Some(node) = self.nodes.get(region.node) {
            node.regions().get(region.region_index)
        } else {
            None
        }
    }

    ///Returns the region at the given location, if it exists.
    /// Returns none if either the `region.node` does not exist, or the `region.region_index`-th region on that node does not exist.
    pub fn region_mut(&mut self, region: &RegionLocation) -> Option<&mut Region> {
        if let Some(node) = self.nodes.get_mut(region.node) {
            node.regions_mut().get_mut(region.region_index)
        } else {
            None
        }
    }

    ///Deletes the `edge` from the graph. Note that any further access to `edge` becomes invalid after that.
    ///
    /// Returns Ok if the disconnect was successful. Otherwise an error describing what went wrong. In general the disconnect will be
    /// executed as good as possible. Meaning, if one of the two nodes is invalid, the other one will be notified of the disconnect regardless.
    /// However, you should consider your graph unstable regardless after an failed disconnect.
    pub fn disconnect(&mut self, edge: EdgeRef) -> Result<(), GraphError> {
        if let Some(edge_val) = self.edges.remove(edge) {
            let Edge { src, dst, ty: _ } = edge_val;

            let mut err = None;
            //Notify src
            if let Some(port) = self.node_mut(src.node).outport_mut(&src.output) {
                port.edges.retain(|e| *e != edge);
            } else {
                err = Some(GraphError::InvalidNode(src.node));
            }

            if let Some(port) = self.node_mut(dst.node).inport_mut(&dst.input) {
                //Mark as unconnected
                port.edge = None;
            } else {
                err = Some(GraphError::InvalidNode(dst.node));
            }

            if let Some(e) = err {
                Err(e)
            } else {
                Ok(())
            }
        } else {
            Err(GraphError::InvalidEdge(edge))
        }
    }

    ///Connects the output `src` to the input `dst`. Note that both ports must be in the same region,
    /// otherwise the connection will fail.
    ///
    /// _"In the same region"_ formally means:
    ///
    /// - `src` is either an `Argument` of the region, or a output port of one of the region's nodes
    /// - `dst` is either a `Result` of the region, or an input port of one of the region's nodes
    pub fn connect(
        &mut self,
        src: OutportLocation,
        dst: InportLocation,
        ty: E,
    ) -> Result<EdgeRef, GraphError> {
        //Find the region this port is defined in.
        // This is always the parent port, except for argument(like) ports, that are defined _on_ the region
        let src_parent_region = if let Some(reg_idx) = src.output.argument_region_index() {
            RegionLocation {
                node: src.node,
                region_index: reg_idx,
            }
        } else {
            self.node(src.node)
                .parent
                .clone()
                .expect("Expected src node to have a parent")
        };

        //Similarly, all result-like outputs are defined on their source region, all others are defined in their parent's region.
        let dst_parent_region = if let Some(reg_idx) = dst.input.result_region_index() {
            RegionLocation {
                node: dst.node,
                region_index: reg_idx,
            }
        } else {
            self.node(dst.node)
                .parent
                .clone()
                .expect("Expected dst to have a src node")
        };

        if src_parent_region != dst_parent_region {
            return Err(GraphError::NodesNotInSameRegion {
                src: src_parent_region,
                dst: dst_parent_region,
            });
        }

        //check that outport exists
        if self.node(src.node).outport(&src.output).is_none() {
            return Err(GraphError::InvalidOutport(src));
        }

        //check that the inport is unused, and exists
        if let Some(port) = self.node(dst.node).inport(&dst.input) {
            if port.edge.is_some() {
                return Err(GraphError::InportInUse(dst));
            }
        } else {
            return Err(GraphError::InvalidInport(dst));
        }

        //are in same region, so we can safely connect
        let edge = self.new_edge(Edge {
            src: src.clone(),
            dst: dst.clone(),
            ty,
        });

        //notify both ports
        self.node_mut(src.node)
            .outport_mut(&src.output)
            .unwrap()
            .edges
            .push(edge);

        self.node_mut(dst.node).inport_mut(&dst.input).unwrap().edge = Some(edge);

        //now notify the region of this new edge
        self.node_mut(src_parent_region.node)
            .regions_mut()
            .get_mut(src_parent_region.region_index)
            .expect("Expected region to exist!")
            .edges
            .insert(edge);

        Ok(edge)
    }
}
