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
use ahash::AHashMap;
use builder::LambdaBuilder;
use edge::{Edge, LangEdge, PortIndex, PortLocation};
use label::LabelLoc;
use nodes::{LangNode, Node, OmegaNode};
use region::{Port, Region};
use slotmap::{new_key_type, SlotMap};
use tinyvec::{array_vec, ArrayVec};

pub mod builder;
pub mod common;
pub mod edge;
pub mod err;
pub mod label;
pub mod nodes;
pub mod region;

new_key_type! {pub struct NodeRef;}
new_key_type! {pub struct EdgeRef;}
new_key_type! {pub struct RegionRef;}

///The RVSDG state. Contains the actual nodes as well as edge definitions.
///
/// `N`: being your language's Nodes, and `E` being the edges. Note that RVSDGs usually have at least two
/// edges types. A _value-typed_, representing a data dependency, and _state-typed_, representing execution ordering constrains.
///
/// However, you are free to represent any kind of other dependency.
///
/// For ease of use you can always type N as `Box<dyn SomeNodeTrait + 'static>`.
pub struct Rvsdg<N: LangNode + 'static, E: LangEdge + 'static> {
    pub(crate) regions: SlotMap<RegionRef, Region>,
    pub(crate) nodes: SlotMap<NodeRef, Node<N>>,
    pub(crate) edges: SlotMap<EdgeRef, Edge<E>>,

    pub(crate) labels: AHashMap<LabelLoc, ArrayVec<[String; 1]>>,

    ///Entrypoint of this translation unit.
    pub(crate) omega: NodeRef,
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    pub fn new() -> Self {
        //Pre-create the omega node we use to track imports/exports
        let mut nodes = SlotMap::default();
        let mut regions = SlotMap::default();
        let omega = nodes.insert(Node::Omega(OmegaNode {
            body: regions.insert(Region::new()),
            outputs: ArrayVec::default(),
            inputs: ArrayVec::default(),
        }));

        Rvsdg {
            edges: SlotMap::default(),
            labels: AHashMap::default(),
            regions,
            nodes,
            omega,
        }
    }

    ///Creates a new top-level lambda node for the graph. If `export` is set, the function will be exported from the
    /// RVSDG. Otherwise it can only be used within the RVSDV.
    pub fn new_lambda(
        mut self,
        export: bool,
        building: impl FnOnce(LambdaBuilder<N, E>) -> LambdaBuilder<N, E>,
    ) -> Self {
        let fnref = {
            let builder = LambdaBuilder::new(&mut self);
            building(builder).build()
        };

        if export {
            todo!("Register lambda {:?} in Omega node {:?}", fnref, self.omega);
        }

        self
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
            let mut new_vec = ArrayVec::default();
            new_vec.push(value);
            self.labels.insert(label, new_vec);
        }
    }

    pub fn labels(&self, label: &LabelLoc) -> Option<&ArrayVec<[String; 1]>> {
        self.labels.get(label)
    }

    pub fn labels_mut(&mut self, label: &LabelLoc) -> Option<&mut ArrayVec<[String; 1]>> {
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

    ///Tries to access the port with the given PortIndex on `node`
    pub fn port(&self, node: NodeRef, port: PortIndex) -> Option<&Port> {
        let mapping = port.into_location::<N>(self.node(node));
        if let Some(mapping) = mapping {
            match mapping {
                PortLocation::Inputs(idx) => self.node(node).inputs().get(idx),
                PortLocation::Arguments { subregion, arg_idx } => {
                    if let Some(subreg) = self.node(node).regions().get(subregion) {
                        if let Some(reg) = self.regions.get(*subreg) {
                            reg.arguments.get(arg_idx)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                PortLocation::Results { subregion, arg_idx } => {
                    if let Some(subreg) = self.node(node).regions().get(subregion) {
                        if let Some(reg) = self.regions.get(*subreg) {
                            reg.results.get(arg_idx)
                        } else {
                            None
                        }
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
                    if let Some(subreg) = self.node(node).regions().get(subregion) {
                        if let Some(reg) = self.regions.get_mut(*subreg) {
                            reg.arguments.get_mut(arg_idx)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                PortLocation::Results { subregion, arg_idx } => {
                    if let Some(subreg) = self.node(node).regions().get(subregion) {
                        if let Some(reg) = self.regions.get_mut(*subreg) {
                            reg.results.get_mut(arg_idx)
                        } else {
                            None
                        }
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

    pub fn new_region(&mut self) -> RegionRef {
        self.regions.insert(Region::new())
    }

    ///Returns reference to the region, assuming that it exists. Panics if it does not exist.
    pub fn region(&self, rref: RegionRef) -> &Region {
        self.regions.get(rref).as_ref().unwrap()
    }

    ///Returns reference to the region, assuming that it exists. Panics if it does not exist.
    pub fn region_mut(&mut self, rref: RegionRef) -> &mut Region {
        self.regions.get_mut(rref).unwrap()
    }

    pub fn on_region<T: 'static>(&self, r: RegionRef, f: impl FnOnce(&Region) -> T) -> T {
        f(self.region(r))
    }

    pub fn on_region_mut<T: 'static>(
        &mut self,
        r: RegionRef,
        f: impl FnOnce(&mut Region) -> T,
    ) -> T {
        f(self.region_mut(r))
    }
}
