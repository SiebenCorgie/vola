/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::path::Path;

use crate::{into_primitive_tree, layout::LayoutConfig, primitives::PrimTree, View};
use ahash::AHashMap;
use rvsdg::{
    attrib::{AttribLocation, AttribStore, FlagStore},
    edge::LangEdge,
    nodes::LangNode,
    Rvsdg,
};
use serde::{Deserialize, Serialize};
use smallvec::{smallvec, SmallVec};

#[derive(Debug, Serialize, Deserialize)]
pub struct GraphState {
    pub display: PrimTree,
    pub name: String,
    ///Auxilary data of the graph safed in a way, that it is easyly accessible.
    ///
    /// This basically safes a Set of _attributes_, keyed by the attribute's name for each
    /// attrib location that has _any_ attribute associated.
    pub auxilary_data: AHashMap<AttribLocation, AHashMap<String, SmallVec<[String; 3]>>>,
}

pub struct GraphStateBuilder<'a> {
    viewer_state: &'a mut ViewerState,
    inner: GraphState,
}

impl<'a> GraphStateBuilder<'a> {
    fn push_str_loc(&mut self, loc: &AttribLocation, attrib_name: &str, attrib: String) {
        if let Some(attribstore) = self.inner.auxilary_data.get_mut(loc) {
            if let Some(stored) = attribstore.get_mut(attrib_name) {
                stored.push(attrib);
            } else {
                attribstore.insert(attrib_name.to_owned(), smallvec![attrib]);
            }
        } else {
            //build a new one
            let mut store = AHashMap::default();
            store.insert(attrib_name.to_owned(), smallvec![attrib]);

            self.inner.auxilary_data.insert(loc.clone(), store);
        }
    }

    ///Adds the given _displayable_ flags to the currently build graph state
    pub fn with_flags<T: ToString>(mut self, name: &str, flags: &FlagStore<T>) -> Self {
        for (loc, attr) in flags.flags.iter() {
            self.push_str_loc(loc, name, attr.to_string());
        }
        self
    }

    pub fn with_store<T: ToString>(mut self, name: &str, store: &AttribStore<T>) -> Self {
        for (loc, attribs) in store.attribs.iter() {
            for attrib in attribs {
                self.push_str_loc(loc, name, attrib.to_string());
            }
        }

        self
    }

    pub fn build(self) {
        self.viewer_state.states.push(self.inner);
    }
}

///Helper that safes multiple runtime states of the graph and its auxilary data and is
///able to be serialized and deserialized.
#[derive(Debug, Serialize, Deserialize)]
pub struct ViewerState {
    pub states: Vec<GraphState>,
}

impl ViewerState {
    pub fn new() -> Self {
        ViewerState {
            states: Vec::with_capacity(10),
        }
    }

    pub fn new_state_builder<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static>(
        &'a mut self,
        name: &str,
        graph: &Rvsdg<N, E>,
        layout_config: &LayoutConfig,
    ) -> GraphStateBuilder<'a> {
        //layout the current graph
        let tree = into_primitive_tree(graph, layout_config);

        GraphStateBuilder {
            viewer_state: self,
            inner: GraphState {
                display: tree,
                name: name.to_owned(),
                auxilary_data: AHashMap::default(),
            },
        }
    }

    ///Serializes `Self` and writes it to `path`.
    ///
    /// Panics is either deserialization fails, or wirting to disk fails.
    pub fn write_to_file(&self, path: &dyn AsRef<Path>) {
        //We use bincode to/from write to disk
        let code = bincode::serialize(&self).unwrap();
        std::fs::write(path, code).unwrap()
    }

    pub fn read_from_file(path: &dyn AsRef<Path>) -> Option<Self> {
        if let Ok(code) = std::fs::read(path) {
            if let Ok(s) = bincode::deserialize(&code) {
                Some(s)
            } else {
                None
            }
        } else {
            None
        }
    }
}
