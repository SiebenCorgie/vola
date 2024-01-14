use std::ops::Not;

use ahash::AHashMap;
use macroquad::math::Vec2;
use rvsdg::{edge::LangEdge, nodes::LangNode, NodeRef, Rvsdg};

use crate::View;

use super::{LayoutConfig, RegionLayout};

pub struct NodeGrid {
    known: AHashMap<NodeRef, (usize, usize)>,
    rows: Vec<Vec<Option<NodeRef>>>,
}

impl NodeGrid {
    pub fn print(&self) {
        println!("known:\n{:?}\n\n", self.known);

        for row in self.rows.iter().rev() {
            for col in row {
                if col.is_some() {
                    print!(" x");
                } else {
                    print!("  ");
                }
            }
            print!("\n");
        }
    }

    fn move_at_least(&mut self, node: NodeRef, dst_y: usize) {
        let location = self.known.get(&node).unwrap();
        let removed = self.rows[location.1][location.0].take().unwrap();
        assert!(removed == node);
        //now insert at new height, same x offset
        self.rows[dst_y].insert(location.0, Some(node));
        //NOTE: need to rebuild, cause we invalidated the positions
        self.rebuild_locations()
    }

    //Rebuilds the "known" index based on `rows`
    fn rebuild_locations(&mut self) {
        self.known.clear();
        for (y_idx, row) in self.rows.iter_mut().enumerate() {
            for (x_idx, col) in row.iter_mut().enumerate() {
                if let Some(node_key) = col {
                    assert!(self.known.insert(*node_key, (x_idx, y_idx)).is_none());
                }
            }
        }
    }
}

impl RegionLayout {
    pub fn sub_layout<'a, N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
    ) {
        for n in self.nodes.values_mut() {
            for reg in &mut n.sub_regions {
                reg.initial_layouting(rvsdg);
            }
        }
    }

    fn node_grid_explore<N: LangNode + 'static, E: LangEdge + 'static>(
        &self,
        rvsdg: &Rvsdg<N, E>,
        grid: &mut NodeGrid,
        node: NodeRef,
        offset: (usize, usize),
    ) {
        //check if that node has been layouted before, in that case all
        // predecessors have been layouted as well.
        if grid.known.contains_key(&node) {
            return;
        }

        //add our selfs to the grid
        //We do that by indexing into the rows, and then searching for an _open_ spot starting at the
        // x value of offset.
        if grid.rows.len() <= offset.1 {
            grid.rows.resize(offset.1 + 1, Vec::with_capacity(0));
        }
        if grid.rows[offset.1].len() <= offset.0 {
            grid.rows[offset.1].resize(offset.0 + 1, None);
        }

        //Now insert our node directly at offest
        grid.rows[offset.1].insert(offset.0, Some(node));
        grid.known.insert(node, offset);

        let num_pred = rvsdg.node(node).inputs().len();
        for (idx, pred) in rvsdg.node(node).pred(rvsdg).enumerate() {
            //do not walk out of region
            if !rvsdg.region(&self.src).unwrap().nodes.contains(&pred.node) {
                continue;
            }

            //We try to spread the predecessors evenly to the left and right.
            let local_offset = idx as isize - (num_pred as isize / 2);
            self.node_grid_explore(
                rvsdg,
                grid,
                pred.node,
                (
                    (offset.0 as isize + local_offset).max(0) as usize,
                    offset.1 + 1,
                ),
            )
        }
    }

    fn fix_grid_criteria<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
        grid: &mut NodeGrid,
        node: NodeRef,
    ) {
        //this one checks that all successor nodes are strictly _below_ the their predecessors.
        let this_location = *grid.known.get(&node).unwrap();
        for pred in rvsdg.node(node).pred(rvsdg) {
            if !rvsdg.region(&self.src).unwrap().nodes.contains(&pred.node) {
                continue;
            }
            if let Some(pred_loc) = grid.known.get(&pred.node) {
                if pred_loc.1 <= this_location.1 {
                    println!("TODO: not yet touched: diverting node {node}");
                    grid.move_at_least(pred.node, this_location.1);
                }
            }
        }

        //now go up the tree
        for pred in rvsdg.node(node).pred(rvsdg) {
            if !rvsdg.region(&self.src).unwrap().nodes.contains(&pred.node) {
                continue;
            }
            self.fix_grid_criteria(rvsdg, grid, pred.node);
        }
    }

    ///Creates an initial layout for the region and its children
    pub fn initial_layouting<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
    ) {
        self.sub_layout(rvsdg);

        //For layouting we use a grid with variable width/height for each row and column.
        // We first sort in all nodes depending on their successor / predecessor relation.
        // After that we execute a _fix_ pass, that makes sure that all successors are strictly _below_ a
        // predecessor.
        // If thats not the case, the predecessor is moved "up" until it is above. If the cell at the desired "up"
        // location is not available, a new row is inserted.

        let mut grid = NodeGrid {
            rows: Vec::new(),
            known: AHashMap::default(),
        };
        let region = rvsdg.region(&self.src).unwrap();

        let mut seed_index = 0usize;
        while let Some(seed_node) = region.result_src(rvsdg, seed_index) {
            seed_index += 1;
            if !region.nodes.contains(&seed_node.node) {
                continue;
            }
            self.node_grid_explore(rvsdg, &mut grid, seed_node.node, (seed_index, 0));
        }

        //Now restart and fix all grid criterias
        let mut seed_index = 0usize;
        while let Some(seed_node) = region.result_src(rvsdg, seed_index) {
            seed_index += 1;
            if !region.nodes.contains(&seed_node.node) {
                continue;
            }
            self.fix_grid_criteria(rvsdg, &mut grid, seed_node.node);
        }

        self.node_grid = Some(grid);
    }

    pub fn ext_for_label<N: LangNode + View + 'static, E: LangEdge + View + 'static>(
        node: NodeRef,
        rvsdg: &Rvsdg<N, E>,
        config: &LayoutConfig,
    ) -> Vec2 {
        let name = rvsdg.node(node).name();
        let port_count = {
            let n = rvsdg.node(node);
            n.inputs().len().max(n.outputs().len())
        };
        let num_chars = name.chars().count();
        let port_width = (port_count * config.port_width)
            + (config.horizontal_node_padding * 2)
            + (config.port_width * 2);
        Vec2::new(
            (num_chars * config.font_size).max(port_width) as f32,
            config.font_size as f32,
        )
    }

    ///Bottom up builds all region extents based on the previously created grid,
    /// which lets us calculate each node size in the grid.
    pub fn bottom_up_transfer_grid<N: LangNode + View + 'static, E: LangEdge + View + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
        config: &LayoutConfig,
    ) {
        //first gather all node extents in this region, by recursively building sub regions,
        // then using that information to build the node's extent
        for node in self.nodes.values_mut() {
            for sub_reg in node.sub_regions.iter_mut() {
                sub_reg.bottom_up_transfer_grid(rvsdg, config);
            }

            //now build the node extent. Each node consists at least of one label, and possibly sub regions.
            let label_ext = Self::ext_for_label(node.src, rvsdg, config);
            //This one does two things, it calculates _how wide_ this node needs to be,
            // and it finds the heighest and widest sub region.
            let (node_region_ext, max_subreg_ext) =
                rvsdg.node(node.src).regions().iter().enumerate().fold(
                    (
                        Vec2::new(
                            config.horizontal_node_padding as f32,
                            config.vertical_node_padding as f32,
                        ),
                        Vec2::ZERO,
                    ),
                    |(mut ext, max_reg_ext), (reg_idx, _sub_reg)| {
                        let this_reg_ext = node.sub_regions[reg_idx].extent;
                        ext.x += this_reg_ext.x.max(config.grid_empty_spacing as f32)
                            + config.horizontal_node_padding as f32;
                        ext.y = ext
                            .y
                            .max(this_reg_ext.y + (2 * config.vertical_node_padding) as f32);
                        (ext, max_reg_ext.max(this_reg_ext))
                    },
                );

            let ext = Vec2::new(
                label_ext.x.max(node_region_ext.x),
                label_ext.y + node_region_ext.y,
            );

            //Post fix up all sub regions heights. This will make sub regions of theta-nodes the same height.
            for subreg in node.sub_regions.iter_mut() {
                subreg.set_height(max_subreg_ext.y, config)
            }

            //overwrite our node ext
            node.extent = ext;
            //update the inports location based on the extent
            for inp in node.inports.iter_mut() {
                inp.y = node.extent.y;
            }
        }

        let mut offset_y = config.grid_padding as f32;
        let mut max_x = 0.0f32;
        for row in self.node_grid.as_ref().unwrap().rows.iter() {
            let mut offset_x = config.grid_padding as f32;
            let max_height = row.iter().fold(0.0f32, |f, n| {
                if let Some(n) = n {
                    f.max(self.nodes.get(n).unwrap().extent.y)
                } else {
                    f
                }
            });

            //we now use max height to position our nodes _lowest possible_,
            // as well as advancing the general _height_ variable later.
            for node in row {
                if let Some(node) = node {
                    //TODO move down to line
                    let node = self.nodes.get_mut(node).unwrap();
                    node.location = Vec2::new(offset_x, offset_y);
                    offset_x += node.extent.x + config.grid_padding as f32;
                } else {
                    //if no node here, move a standard width
                    offset_x += config.grid_empty_spacing as f32;
                }
            }

            offset_y += max_height + config.grid_padding as f32;

            max_x = max_x.max(offset_x);
        }

        self.extent = Vec2::new(
            max_x.max(config.grid_empty_spacing as f32),
            offset_y.max(config.grid_empty_spacing as f32),
        )
    }

    pub fn set_height(&mut self, height: f32, config: &LayoutConfig) {
        self.extent.y = height;
        for arg in &mut self.arg_ports {
            arg.y = height - config.port_height as f32;
        }
    }
}
