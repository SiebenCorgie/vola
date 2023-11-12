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
use nodes::{LanguageNode, Node};
use region::Region;
use slotmap::{new_key_type, SlotMap};

pub mod builder;
pub mod common;
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
pub struct Rvsdg<N: LanguageNode + 'static, E: 'static> {
    pub regions: SlotMap<RegionRef, Region>,
    pub nodes: SlotMap<NodeRef, Node<N>>,
    pub edges: SlotMap<EdgeRef, E>,
}

impl<N: LanguageNode + 'static, E: 'static> Rvsdg<N, E> {
    pub fn new() -> Self {
        Rvsdg {
            regions: SlotMap::default(),
            nodes: SlotMap::default(),
            edges: SlotMap::default(),
        }
    }

    ///Allows you to use the [RvsdgBuilder](builder::RvsdgBuilder) utility to create a new translation-unit / [ω-Node](nodes::OmegaNode).
    pub fn builder<'a>(&'a mut self) -> builder::RvsdgBuilder<'a, N, E> {
        builder::RvsdgBuilder::on_rvsd(self)
    }
}
