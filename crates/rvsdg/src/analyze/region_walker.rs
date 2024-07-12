/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::collections::VecDeque;

use crate::{
    edge::LangEdge,
    nodes::LangNode,
    region::{Region, RegionLocation},
    Rvsdg,
};

///Iterates over all regions, returns the [RegionLocation](crate::region::RegionLocation)s in a breadth-first / top-down style.
pub struct RegionLocationWalker<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    pub(crate) graph: &'a Rvsdg<N, E>,
    pub(crate) region_queue: VecDeque<RegionLocation>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for RegionLocationWalker<'a, N, E> {
    type Item = RegionLocation;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_region) = self.region_queue.pop_back() {
            let region = self.graph.region(&next_region).unwrap();

            //find all nodes with sub regions and enque them
            for n in &region.nodes {
                let regions = self.graph.node(*n).regions();
                for ridx in 0..regions.len() {
                    self.region_queue.push_front(RegionLocation {
                        node: *n,
                        region_index: ridx,
                    });
                }
            }
            Some(next_region)
        } else {
            None
        }
    }
}

///Iterates all regions the same as [RegionLocationWalker], but returns the concrete region.
pub struct RegionWalker<'a, N: LangNode + 'static, E: LangEdge + 'static> {
    pub(crate) location_walker: RegionLocationWalker<'a, N, E>,
}

impl<'a, N: LangNode + 'static, E: LangEdge + 'static> Iterator for RegionWalker<'a, N, E> {
    type Item = &'a Region;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_reg) = self.location_walker.next() {
            Some(self.location_walker.graph.region(&next_reg).unwrap())
        } else {
            None
        }
    }
}
