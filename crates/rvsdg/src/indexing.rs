/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::ops::{Index, IndexMut};

use crate::{
    edge::{Edge, InportLocation, LangEdge, OutportLocation},
    nodes::{LangNode, Node},
    region::{Input, Outport, Region, RegionLocation},
    EdgeRef, NodeRef, Rvsdg,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Index<NodeRef> for Rvsdg<N, E> {
    type Output = Node<N>;

    fn index(&self, index: NodeRef) -> &Self::Output {
        self.node(index)
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> IndexMut<NodeRef> for Rvsdg<N, E> {
    fn index_mut(&mut self, index: NodeRef) -> &mut Self::Output {
        self.node_mut(index)
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Index<EdgeRef> for Rvsdg<N, E> {
    type Output = Edge<E>;

    fn index(&self, index: EdgeRef) -> &Self::Output {
        self.edge(index)
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> IndexMut<EdgeRef> for Rvsdg<N, E> {
    fn index_mut(&mut self, index: EdgeRef) -> &mut Self::Output {
        self.edge_mut(index)
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Index<RegionLocation> for Rvsdg<N, E> {
    type Output = Region;

    fn index(&self, index: RegionLocation) -> &Self::Output {
        self.region(&index).unwrap()
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> IndexMut<RegionLocation> for Rvsdg<N, E> {
    fn index_mut(&mut self, index: RegionLocation) -> &mut Self::Output {
        self.region_mut(&index).unwrap()
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Index<InportLocation> for Rvsdg<N, E> {
    type Output = Input;

    fn index(&self, index: InportLocation) -> &Self::Output {
        self.node(index.node).inport(&index.input).unwrap()
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> IndexMut<InportLocation> for Rvsdg<N, E> {
    fn index_mut(&mut self, index: InportLocation) -> &mut Self::Output {
        self.node_mut(index.node).inport_mut(&index.input).unwrap()
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> Index<OutportLocation> for Rvsdg<N, E> {
    type Output = Outport;

    fn index(&self, index: OutportLocation) -> &Self::Output {
        self.node(index.node).outport(&index.output).unwrap()
    }
}

impl<N: LangNode + 'static, E: LangEdge + 'static> IndexMut<OutportLocation> for Rvsdg<N, E> {
    fn index_mut(&mut self, index: OutportLocation) -> &mut Self::Output {
        self.node_mut(index.node)
            .outport_mut(&index.output)
            .unwrap()
    }
}
