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

use crate::{
    edge::{InportLocation, OutportLocation},
    region::RegionLocation,
    EdgeRef, NodeRef,
};

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
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
            Self::InPort(p) => write!(f, "Inport - {p:?}"),
            Self::OutPort(p) => write!(f, "Outport - {p:?}"),
            Self::Region(r) => write!(f, "{r:?}"),
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
