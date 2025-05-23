use std::fmt::Display;

/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use ahash::AHashSet;
use smallvec::smallvec;

use crate::{
    edge::{InportLocation, LangEdge, OutportLocation},
    nodes::LangNode,
    EdgeRef, NodeRef, Rvsdg, SmallColl,
};

///A Outport allows us to define multiple destination edges. It is the base type for [Output] and [Argument].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Outport {
    pub edges: SmallColl<EdgeRef>,
}

impl Default for Outport {
    fn default() -> Self {
        Outport {
            edges: SmallColl::default(),
        }
    }
}

pub type Output = Outport;
pub type Argument = Outport;

///A in port allows us to define at most a single src edge. It is the base type for [Input] and [RegResult].
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Inport {
    pub edge: Option<EdgeRef>,
}

impl Default for Inport {
    fn default() -> Self {
        Inport { edge: None }
    }
}
pub type Input = Inport;
pub type RegResult = Inport;

///Region as defined in 4.0 in the source paper.
///
/// A region R = (A, N, E, R) is characterised through a set of arguments A, its internal nodes N and edges E, and a result tuple R.
#[derive(Debug, Clone)]
pub struct Region {
    pub arguments: SmallColl<Argument>,
    pub results: SmallColl<RegResult>,
    pub nodes: AHashSet<NodeRef>,
    pub edges: AHashSet<EdgeRef>,
}

impl Region {
    pub fn new() -> Self {
        Region {
            nodes: AHashSet::default(),
            edges: AHashSet::default(),
            arguments: SmallColl::default(),
            results: SmallColl::default(),
        }
    }

    ///Return the OutportLocation that result is connected to.
    pub fn result_src<N: LangNode + 'static, E: LangEdge + 'static>(
        &self,
        ctx: &Rvsdg<N, E>,
        result_index: usize,
    ) -> Option<OutportLocation> {
        if let Some(port) = self.results.get(result_index) {
            if let Some(edg) = port.edge {
                Some(ctx.edge(edg).src)
            } else {
                None
            }
        } else {
            None
        }
    }

    ///Collects all InportLocations that `argument_index` is connected to.
    pub fn argument_dst<N: LangNode + 'static, E: LangEdge + 'static>(
        &self,
        ctx: &Rvsdg<N, E>,
        argument_index: usize,
    ) -> Option<SmallColl<InportLocation>> {
        if let Some(port) = self.arguments.get(argument_index) {
            let mut coll = SmallColl::new();
            for edg in port.edges.iter() {
                coll.push(ctx.edge(*edg).dst);
            }
            Some(coll)
        } else {
            None
        }
    }

    ///Creates a copy of this region with the same argument/result setup, but no nodes or edges
    pub fn copy_config(&self) -> Self {
        Region {
            arguments: smallvec![Argument::default(); self.arguments.len()],
            results: smallvec![RegResult::default(); self.results.len()],
            nodes: AHashSet::default(),
            edges: AHashSet::default(),
        }
    }
}

impl Default for Region {
    fn default() -> Self {
        Region::new()
    }
}

///Specifies the location of a certain `node`'s `region_index`-th region.
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct RegionLocation {
    pub node: NodeRef,
    pub region_index: usize,
}

impl Display for RegionLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Region[{}, {}]", self.node, self.region_index)
    }
}
