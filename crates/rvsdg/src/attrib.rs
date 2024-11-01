/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # Attributes
//!
//! Helper that lets you associate any item within the graph with a custom attribute.

use std::fmt::Display;

use ahash::AHashMap;
use smallvec::SmallVec;

use crate::{
    edge::{InportLocation, LangEdge, OutportLocation},
    nodes::LangNode,
    region::RegionLocation,
    EdgeRef, NodeRef, Rvsdg,
};

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AttribLocation {
    InPort(InportLocation),
    OutPort(OutportLocation),
    Region(RegionLocation),
    Node(NodeRef),
    Edge(EdgeRef),
}

impl Display for AttribLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InPort(p) => write!(f, "{p}"),
            Self::OutPort(p) => write!(f, "{p}"),
            Self::Region(r) => write!(f, "{r}"),
            Self::Node(n) => write!(f, "{n}"),
            Self::Edge(e) => write!(f, "{e}"),
        }
    }
}

impl From<RegionLocation> for AttribLocation {
    fn from(value: RegionLocation) -> Self {
        AttribLocation::Region(value)
    }
}
impl From<&RegionLocation> for AttribLocation {
    fn from(value: &RegionLocation) -> Self {
        AttribLocation::Region(value.clone())
    }
}

impl From<InportLocation> for AttribLocation {
    fn from(value: InportLocation) -> Self {
        AttribLocation::InPort(value)
    }
}
impl From<&InportLocation> for AttribLocation {
    fn from(value: &InportLocation) -> Self {
        AttribLocation::InPort(value.clone())
    }
}

impl From<OutportLocation> for AttribLocation {
    fn from(value: OutportLocation) -> Self {
        AttribLocation::OutPort(value)
    }
}
impl From<&OutportLocation> for AttribLocation {
    fn from(value: &OutportLocation) -> Self {
        AttribLocation::OutPort(value.clone())
    }
}

impl From<NodeRef> for AttribLocation {
    fn from(value: NodeRef) -> Self {
        AttribLocation::Node(value)
    }
}
impl From<&NodeRef> for AttribLocation {
    fn from(value: &NodeRef) -> Self {
        AttribLocation::Node(value.clone())
    }
}

impl From<EdgeRef> for AttribLocation {
    fn from(value: EdgeRef) -> Self {
        AttribLocation::Edge(value)
    }
}
impl From<&EdgeRef> for AttribLocation {
    fn from(value: &EdgeRef) -> Self {
        AttribLocation::Edge(value.clone())
    }
}

impl AttribLocation {
    ///Exchanges the `node` field of any attribute location that is
    /// not an EdgeRef.
    /// For edges, it does nothing.
    pub fn change_node(self, new_node: NodeRef) -> Self {
        match self {
            AttribLocation::Edge(e) => AttribLocation::Edge(e),
            AttribLocation::Node(_n) => AttribLocation::Node(new_node),
            AttribLocation::InPort(InportLocation { node: _, input }) => {
                AttribLocation::InPort(InportLocation {
                    node: new_node,
                    input,
                })
            }
            AttribLocation::OutPort(OutportLocation { node: _, output }) => {
                AttribLocation::OutPort(OutportLocation {
                    node: new_node,
                    output,
                })
            }
            AttribLocation::Region(RegionLocation {
                node: _,
                region_index,
            }) => AttribLocation::Region(RegionLocation {
                node: new_node,
                region_index,
            }),
        }
    }
}

pub struct AttribLocIter {
    //Note since we don't want to borrow the Graph
    //itself, which would make handling the whole thing kinda wonky,
    //we pre allocate _all_ and to the iteration over the vec.
    attribs: SmallVec<[AttribLocation; 32]>,
    idx: usize,
}

impl Iterator for AttribLocIter {
    type Item = AttribLocation;
    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.attribs.len() {
            None
        } else {
            let element = Some(self.attribs[self.idx].clone());
            self.idx += 1;
            element
        }
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        let access = self.idx + n;
        let element = self.attribs.get(access).cloned();
        self.idx = access;
        element
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let rest_size = self.attribs.len() - self.idx;
        (rest_size, Some(rest_size))
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Iterates all [AttribLocation]s that are directly connected to this node.
    ///
    ///This includes (in that order):
    ///
    /// - The Node itself
    /// - All input-ports
    /// - All output-pors
    /// - All sub-regions (if there are any)
    /// - All Argument ports to any subregion
    /// - All Result ports to any subregion
    pub fn iter_node_attribs(&self, node: NodeRef) -> AttribLocIter {
        let mut attribs = SmallVec::new();

        attribs.push(node.into());
        for input in self[node].inport_types() {
            attribs.push(InportLocation { node, input }.into());
        }
        for output in self[node].outport_types() {
            attribs.push(OutportLocation { node, output }.into());
        }
        let regcount = self[node].regions().len();
        for regidx in 0..regcount {
            attribs.push(
                RegionLocation {
                    node,
                    region_index: regidx,
                }
                .into(),
            );
        }

        if regcount > 0 {
            //Push all arguments
            for region_index in 0..regcount {
                for output in self[node].argument_types(region_index) {
                    attribs.push(OutportLocation { node, output }.into());
                }
            }
            //Push all results
            for region_index in 0..regcount {
                for input in self[node].result_types(region_index) {
                    attribs.push(InportLocation { node, input }.into());
                }
            }
        }

        AttribLocIter { attribs, idx: 0 }
    }
}

///Stores multiple attributes for any component of the RVSDG. If you only want to store
/// one attribute. Have a look at [FlagStore].
#[derive(Debug, Clone)]
pub struct AttribStore<ATTRIB: 'static> {
    pub attribs: AHashMap<AttribLocation, Vec<ATTRIB>>,
}

impl<ATTRIB: 'static> AttribStore<ATTRIB> {
    pub fn new() -> Self {
        AttribStore {
            attribs: AHashMap::default(),
        }
    }
    pub fn attrib(&self, location: &AttribLocation) -> Option<&[ATTRIB]> {
        self.attribs.get(location).map(|s| s.as_slice())
    }
    pub fn attrib_mut(&mut self, location: &AttribLocation) -> Option<&mut Vec<ATTRIB>> {
        self.attribs.get_mut(location)
    }

    pub fn push_attrib(&mut self, location: &AttribLocation, attrib: ATTRIB) {
        if let Some(coll) = self.attribs.get_mut(&location) {
            coll.push(attrib);
        } else {
            self.attribs.insert(location.clone(), vec![attrib]);
        }
    }
}

///Stores zero or one single flag of type `F` for any component of the RVSDG.
#[derive(Clone, Debug)]
pub struct FlagStore<F: 'static> {
    pub flags: AHashMap<AttribLocation, F>,
}

impl<F: 'static> FlagStore<F> {
    pub fn new() -> Self {
        FlagStore {
            flags: AHashMap::default(),
        }
    }

    pub fn get(&self, location: &AttribLocation) -> Option<&F> {
        self.flags.get(location)
    }

    pub fn get_mut(&mut self, location: &AttribLocation) -> Option<&mut F> {
        self.flags.get_mut(location)
    }

    ///Sets or overwrites the flag. If there was already one set, returns the old one.
    pub fn set(&mut self, location: AttribLocation, flag: F) -> Option<F> {
        self.flags.insert(location, flag)
    }
}

impl<F: Clone + 'static> FlagStore<F> {
    ///Copies the set, or unset attribute from one location to another. Note that
    ///if the attribute is not set, then the `dst` won't be set as well.
    ///
    /// Returns a value if `dst`'s state was already set.
    pub fn copy(&mut self, src: &AttribLocation, dst: AttribLocation) -> Option<F> {
        if let Some(state) = self.get(src).cloned() {
            self.set(dst, state)
        } else {
            None
        }
    }
}

impl FlagStore<bool> {
    ///Returns true if the location is set, and the value is true.
    pub fn is_set(&self, location: &AttribLocation) -> bool {
        if let Some(val) = self.get(location) {
            *val
        } else {
            false
        }
    }
}
