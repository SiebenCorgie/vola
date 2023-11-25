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
use label::LabelLoc;
use nodes::{LangNode, Node, OmegaNode};
use region::Region;
use slotmap::{new_key_type, SlotMap};
use tinyvec::TinyVec;

pub mod analyze;
pub mod builder;
pub mod common;
pub mod edge;
pub mod err;
pub mod label;
pub mod nodes;
pub mod region;

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

    pub(crate) labels: AHashMap<LabelLoc, TinyVec<[String; 1]>>,

    ///Entrypoint of this translation unit.
    pub(crate) omega: NodeRef,
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    pub fn new() -> Self {
        //Pre-create the omega node we use to track imports/exports
        let mut nodes = SlotMap::default();
        let omega = nodes.insert(Node::Omega(OmegaNode {
            body: Region::new(),
        }));

        Rvsdg {
            edges: SlotMap::default(),
            labels: AHashMap::default(),
            nodes,
            omega,
        }
    }

    ///Allows you to modify the graph from its entry point, the [ω-Node](crate::nodes::OmegaNode).
    pub fn on_omega_node(&mut self, f: impl FnOnce(OmegaBuilder<N, E>) -> OmegaBuilder<N, E>) {
        //we do this by replacing the actual omega node, setup the builder, call it,
        // and then substituding it again.
        let omega_ref = self.omega;
        let omega_node = if let Node::Omega(on) = {
            let mut node = Node::Invalid;
            let dummy = self.node_mut(self.omega);
            std::mem::swap(dummy, &mut node);
            node
        } {
            on
        } else {
            panic!("Failed to unwrap temporary omgea node");
        };

        let builder = OmegaBuilder {
            ctx: self,
            node: omega_node,
            node_ref: omega_ref,
        };

        //call the user closure
        let OmegaBuilder {
            ctx,
            node,
            node_ref,
        } = f(builder);

        //now re substitude
        *ctx.node_mut(node_ref) = Node::Omega(node);
    }

    ///Returns the reference to the translation unit / [ω-Node](crate::nodes::OmegaNode).
    pub fn entry_node(&self) -> &NodeRef {
        &self.omega
    }

    pub fn new_node(&mut self, node: Node<N>) -> NodeRef {
        self.nodes.insert(node)
    }

    ///Returns reference to the node, assuming that it exists. Panics if it does not exist.
    pub fn node(&self, nref: NodeRef) -> &Node<N> {
        self.nodes.get(nref).as_ref().unwrap()
    }

    pub fn push_label(&mut self, label: LabelLoc, value: String) {
        if let Some(vals) = self.labels.get_mut(&label) {
            vals.push(value);
        } else {
            let mut new_vec = TinyVec::default();
            new_vec.push(value);
            self.labels.insert(label, new_vec);
        }
    }

    pub fn labels(&self, label: &LabelLoc) -> Option<&TinyVec<[String; 1]>> {
        self.labels.get(label)
    }

    pub fn labels_mut(&mut self, label: &LabelLoc) -> Option<&mut TinyVec<[String; 1]>> {
        self.labels.get_mut(label)
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
    /*
        ///Tries to access the port with the given PortIndex on `node`
        pub fn port(&self, node: NodeRef, port: PortIndex) -> Option<&Port> {
            let mapping = port.into_location::<N>(self.node(node));
            if let Some(mapping) = mapping {
                match mapping {
                    PortLocation::Inputs(idx) => self.node(node).inputs().get(idx),
                    PortLocation::Arguments { subregion, arg_idx } => {
                        if let Some(subreg) = self.node(node).regions().get(subregion) {
                            subreg.arguments.get(arg_idx)
                        } else {
                            None
                        }
                    }
                    PortLocation::Results { subregion, arg_idx } => {
                        if let Some(subreg) = self.node(node).regions().get(subregion) {
                            subreg.results.get(arg_idx)
                        } else {
                            None
                        }
                    }
                    PortLocation::Outputs(idx) => self.node(node).outputs().get(idx),
                }
            } else {
                None
            }
        }

        ///Tries to access the port with the given PortIndex on `node`
        pub fn port_mut(&mut self, node: NodeRef, port: PortIndex) -> Option<&mut Port> {
            let mapping = port.into_location::<N>(self.node(node));
            if let Some(mapping) = mapping {
                match mapping {
                    PortLocation::Inputs(idx) => self.node_mut(node).inputs_mut().get_mut(idx),
                    PortLocation::Arguments { subregion, arg_idx } => {
                        if let Some(subreg) = self.node_mut(node).regions_mut().get_mut(subregion) {
                            subreg.arguments.get_mut(arg_idx)
                        } else {
                            None
                        }
                    }
                    PortLocation::Results { subregion, arg_idx } => {
                        if let Some(subreg) = self.node_mut(node).regions_mut().get_mut(subregion) {
                            subreg.results.get_mut(arg_idx)
                        } else {
                            None
                        }
                    }
                    PortLocation::Outputs(idx) => self.node_mut(node).outputs_mut().get_mut(idx),
                }
            } else {
                None
            }
        }
    */
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
}
