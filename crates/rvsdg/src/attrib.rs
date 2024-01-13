//! # Attributes
//!
//! Helper that lets you associate any item within the graph with a custom attribute.

use ahash::AHashMap;

use crate::{
    edge::{InportLocation, OutportLocation},
    region::RegionLocation,
    EdgeRef, NodeRef,
};

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum AttribLocation {
    InPort(InportLocation),
    OutPort(OutportLocation),
    Region(RegionLocation),
    Node(NodeRef),
    Edge(EdgeRef),
}

impl From<RegionLocation> for AttribLocation {
    fn from(value: RegionLocation) -> Self {
        AttribLocation::Region(value)
    }
}
impl From<InportLocation> for AttribLocation {
    fn from(value: InportLocation) -> Self {
        AttribLocation::InPort(value)
    }
}

impl From<OutportLocation> for AttribLocation {
    fn from(value: OutportLocation) -> Self {
        AttribLocation::OutPort(value)
    }
}

impl From<NodeRef> for AttribLocation {
    fn from(value: NodeRef) -> Self {
        AttribLocation::Node(value)
    }
}

impl From<EdgeRef> for AttribLocation {
    fn from(value: EdgeRef) -> Self {
        AttribLocation::Edge(value)
    }
}

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
