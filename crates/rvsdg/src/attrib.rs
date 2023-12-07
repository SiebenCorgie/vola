//! # Attributes
//!
//! Helper that lets you associate any item within the graph with a custom attribute.

use ahash::AHashMap;

use crate::{
    edge::{InportLocation, OutportLocation},
    EdgeRef, NodeRef,
};

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum AttribLocation {
    InPort(InportLocation),
    OutPort(OutportLocation),
    Node(NodeRef),
    Edge(EdgeRef),
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

pub struct AttribStore<ATTRIB: 'static> {
    pub attribs: AHashMap<AttribLocation, Vec<ATTRIB>>,
}

impl<ATTRIB: 'static> AttribStore<ATTRIB> {
    pub fn new() -> Self {
        AttribStore {
            attribs: AHashMap::default(),
        }
    }
    pub fn attrib(&self, location: impl AsRef<AttribLocation>) -> Option<&[ATTRIB]> {
        self.attribs.get(location.as_ref()).map(|s| s.as_slice())
    }
    pub fn attrib_mut(&mut self, location: impl AsRef<AttribLocation>) -> Option<&mut Vec<ATTRIB>> {
        self.attribs.get_mut(location.as_ref())
    }

    pub fn push_attrib(&mut self, location: impl Into<AttribLocation>, attrib: ATTRIB) {
        let loc = location.into();
        if let Some(coll) = self.attribs.get_mut(&loc) {
            coll.push(attrib);
        } else {
            self.attribs.insert(loc, vec![attrib]);
        }
    }
}
