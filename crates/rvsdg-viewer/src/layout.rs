//! The new, improved layouting mechanism for the viewer.
//!
//! We build the graph layout in multiple steps
//!
//! 1. Explore graph, setup region hierarchy
//! 2. Initial node layout, based on connectivity, bottom-up
//! 3. Bottom up region extent fitting.
//! 4. Layout fitting, makes sure any successor node is strictly "lower" then its predecessor
//! 5. Edge routing, for each region
//!     5.1 Initial routing grid, blacklist node's regions
//!     5.2 For each edge, build _any_ path consisting only of horizontal and _down_ movement. Edeges are only allowed to cross. A* it'll be I guess.
//! 6. For any edge that couldn't be routed, insert a straight path.
//!

use ahash::AHashMap;
use macroquad::math::Vec2;
use rvsdg::{edge::LangEdge, nodes::LangNode, region::RegionLocation, NodeRef, Rvsdg};

use crate::View;

mod initial;

pub struct LayoutNode {
    location: Vec2,
    extent: Vec2,
    src: NodeRef,
    sub_regions: Vec<RegionLayout>,
}

pub struct LayoutEdge {
    ///Path points. Can be read like a svg path "xy -> zw -> ab", all
    /// in region local coordinates.
    path: Vec<Vec2>,
}

pub struct RegionLayout {
    //Source loc of the region
    src: RegionLocation,
    nodes: AHashMap<NodeRef, LayoutNode>,
}

impl RegionLayout {
    fn build_for_region<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static>(
        rvsdg: &Rvsdg<N, E>,
        region: RegionLocation,
    ) -> RegionLayout {
        //For each node in the region, build a LayoutNode, and if applicable, build sub-regions

        let nodes = rvsdg
            .region(&region)
            .unwrap()
            .nodes
            .iter()
            .map(|nref| {
                let node = rvsdg.node(*nref);
                let sub_regions = if node.regions().len() > 0 {
                    let mut sub_regions = Vec::with_capacity(node.regions().len());
                    for subidx in 0..node.regions().len() {
                        sub_regions.push(RegionLayout::build_for_region(
                            rvsdg,
                            RegionLocation {
                                node: *nref,
                                region_index: subidx,
                            },
                        ));
                    }

                    sub_regions
                } else {
                    Vec::with_capacity(0)
                };

                (
                    *nref,
                    LayoutNode {
                        location: Vec2::ZERO,
                        extent: Vec2::ZERO,
                        src: *nref,
                        sub_regions,
                    },
                )
            })
            .collect();

        RegionLayout { src: region, nodes }
    }
}

pub struct Layout<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static> {
    src_graph: &'a Rvsdg<N, E>,
    region_tree: RegionLayout,
}

impl<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static> Layout<'a, N, E> {
    pub fn for_rvsdg(rvsdg: &'a Rvsdg<N, E>) -> Self {
        let tlregion = rvsdg.toplevel_region();
        let mut region_tree = RegionLayout::build_for_region(rvsdg, tlregion);

        region_tree.initial_layouting(rvsdg);

        Layout {
            src_graph: rvsdg,
            region_tree,
        }
    }
}
