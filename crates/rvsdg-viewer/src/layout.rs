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
use rvsdg::{edge::LangEdge, nodes::LangNode, region::RegionLocation, EdgeRef, NodeRef, Rvsdg};

use crate::{primitives::PrimTree, View};

use self::initial::NodeGrid;

mod edge_routing;
mod initial;
mod toprims;

pub struct LayoutConfig {
    //The _height_ of a node's outer border to the inner region
    pub vertical_node_padding: usize,
    //The _width_ of a node's outer border to the inner region
    pub horizontal_node_padding: usize,
    //horizontal and vertical padding between nodes in a grid layout
    pub grid_padding: usize,
    pub font_size: usize,
    pub grid_empty_spacing: usize,
    pub port_spacing: usize,
    pub port_width: usize,
    pub port_height: usize,

    pub routing_cell_size: f32,
    pub routing_dead_padding: f32,
}

impl Default for LayoutConfig {
    fn default() -> Self {
        LayoutConfig {
            vertical_node_padding: 10,
            horizontal_node_padding: 10,
            grid_padding: 20,
            font_size: 10,
            grid_empty_spacing: 10,
            port_spacing: 10,
            port_width: 3,
            port_height: 2,
            routing_cell_size: 2.0,
            routing_dead_padding: 5.0,
        }
    }
}

pub struct LayoutNode {
    location: Vec2,
    extent: Vec2,
    src: NodeRef,
    inports: Vec<Vec2>,
    outports: Vec<Vec2>,
    sub_regions: Vec<RegionLayout>,
}

pub struct LayoutEdge {
    ///Path points. Can be read like a svg path "xy -> zw -> ab", all
    /// in region local coordinates.
    path: Vec<Vec2>,
    src: EdgeRef,
}

pub struct RegionLayout {
    //Source loc of the region
    src: RegionLocation,
    arg_ports: Vec<Vec2>,
    res_ports: Vec<Vec2>,
    extent: Vec2,
    node_grid: Option<NodeGrid>,
    nodes: AHashMap<NodeRef, LayoutNode>,
    edges: Vec<LayoutEdge>,
}

impl RegionLayout {
    fn build_for_region<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static>(
        rvsdg: &Rvsdg<N, E>,
        region: RegionLocation,
        config: &LayoutConfig,
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
                            config,
                        ));
                    }

                    sub_regions
                } else {
                    Vec::with_capacity(0)
                };

                let inports = node
                    .inputs()
                    .iter()
                    .enumerate()
                    .map(|(idx, _p)| {
                        Vec2::new(
                            config.port_width as f32 + config.port_spacing as f32 * idx as f32,
                            0.0,
                        )
                    })
                    .collect();
                let outports = node
                    .outputs()
                    .iter()
                    .enumerate()
                    .map(|(idx, _p)| {
                        Vec2::new(
                            config.port_width as f32 + config.port_spacing as f32 * idx as f32,
                            -(config.port_height as f32),
                        )
                    })
                    .collect();

                (
                    *nref,
                    LayoutNode {
                        location: Vec2::ZERO,
                        extent: Vec2::ZERO,
                        src: *nref,
                        inports,
                        outports,
                        sub_regions,
                    },
                )
            })
            .collect();

        let arg_ports = rvsdg
            .region(&region)
            .unwrap()
            .arguments
            .iter()
            .enumerate()
            .map(|(idx, _)| {
                Vec2::new(
                    config.port_width as f32 + config.port_spacing as f32 * idx as f32,
                    0.0,
                )
            })
            .collect();

        let res_ports = rvsdg
            .region(&region)
            .unwrap()
            .results
            .iter()
            .enumerate()
            .map(|(idx, _)| {
                Vec2::new(
                    config.port_width as f32 + config.port_spacing as f32 * idx as f32,
                    0.0,
                )
            })
            .collect();

        RegionLayout {
            src: region,
            nodes,
            arg_ports,
            res_ports,
            edges: Vec::with_capacity(rvsdg.region(&region).unwrap().edges.len()),
            node_grid: None,
            extent: Vec2::ZERO,
        }
    }
}

pub struct Layout<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static> {
    pub(crate) src_graph: &'a Rvsdg<N, E>,
    pub(crate) config: LayoutConfig,
    pub(crate) region_tree: RegionLayout,
}

//NOTE: Most interesting implementation happens in the submodules!
impl<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static> Layout<'a, N, E> {
    pub fn for_rvsdg_default(rvsdg: &'a Rvsdg<N, E>) -> Self {
        Self::for_rvsdg(rvsdg, LayoutConfig::default())
    }
    pub fn for_rvsdg(rvsdg: &'a Rvsdg<N, E>, config: LayoutConfig) -> Self {
        let tlregion = rvsdg.toplevel_region();
        let mut region_tree = RegionLayout::build_for_region(rvsdg, tlregion, &config);

        region_tree.initial_layouting(rvsdg);

        region_tree.bottom_up_transfer_grid(rvsdg, &config);

        let regheight = region_tree.extent.y;
        region_tree.set_height(regheight, &config);

        let mut layout = Layout {
            src_graph: rvsdg,
            config,
            region_tree,
        };

        layout.route_edges();

        layout
    }

    pub fn into_primitive_tree(&self) -> PrimTree {
        self.to_prim_tree(&self.config)
    }
}
