use macroquad::math::Vec2;
use rvsdg::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::LangNode,
    EdgeRef, Rvsdg,
};

use crate::View;

use super::{Layout, LayoutConfig, LayoutEdge, LayoutNode, RegionLayout};

#[derive(Clone, Debug)]
struct ELCell {
    active: bool,
    in_use: bool,
}

impl Default for ELCell {
    fn default() -> Self {
        ELCell {
            active: true,
            in_use: false,
        }
    }
}

impl ELCell {
    //checks if we can claim horizontal or vertical route in this cell
    pub fn can_route(&mut self) -> bool {
        if !self.active {
            false
        } else {
            !self.in_use
        }
    }
}

/// A regular grid we use to layout our edges using A*
///
/// The process first calculates the size of the grid, then marks
/// all cells used by a node as "dead",
/// and finally does the DFS routing.
///
/// However, we don't allow "up" movement. So x is free to change, and y can only increment.
///
/// Another criteria is, that each cell can only be used once for a horizontal, and once for a vertical line.
struct EdgeLayoutGrid {
    resolution: (usize, usize),
    cell_size: f32,
    grid: Vec<Vec<ELCell>>,
}

#[derive(Debug)]
enum Dir {
    Up,
    Left,
    Right,
}

impl Dir {
    fn is_vertical(&self) -> bool {
        if let Dir::Up = self {
            true
        } else {
            false
        }
    }
}

impl Dir {
    const fn into_vec2(&self) -> Vec2 {
        match self {
            Dir::Up => Vec2 { x: 0.0, y: -1.0 },
            Dir::Left => Vec2 { x: -1.0, y: 0.0 },
            Dir::Right => Vec2 { x: 1.0, y: 0.0 },
        }
    }
}

impl EdgeLayoutGrid {
    pub fn new_for_reg(reg: &RegionLayout, config: &LayoutConfig) -> Self {
        let resolution = (reg.extent / config.routing_cell_size).ceil() + 1.0;
        let resolution = (resolution.x as usize, resolution.y as usize);

        let grid = vec![vec![ELCell::default(); resolution.1]; resolution.0];

        EdgeLayoutGrid {
            resolution,
            cell_size: config.routing_cell_size,
            grid,
        }
    }

    fn deactivate(&mut self, node: &LayoutNode, config: &LayoutConfig) {
        let mut off = Vec2::ZERO;
        while off.x < (node.extent.x + config.routing_dead_padding) {
            off.y = 0.0;
            while off.y < (node.extent.y + config.routing_dead_padding) {
                let cell =
                    self.loc_to_cell(node.location + off - (config.routing_dead_padding / 2.0));
                self.grid[cell.0][cell.1].active = false;
                off.y += self.cell_size;
            }
            off.x += self.cell_size;
        }
    }

    fn loc_to_cell(&self, loc: Vec2) -> (usize, usize) {
        let clamped = (loc / self.cell_size).round();
        (clamped.x as usize, clamped.y as usize)
    }

    fn cell_to_loc(&self, cell: (usize, usize)) -> Vec2 {
        Vec2::new(
            cell.0 as f32 * self.cell_size,
            cell.1 as f32 * self.cell_size,
        )
    }

    #[allow(dead_code)]
    fn print_active(&self) {
        for col in &self.grid {
            for cell in col.iter() {
                if cell.active {
                    print!("x");
                } else {
                    print!(" ");
                }
            }
            print!("\n");
        }
    }

    #[allow(dead_code)]
    fn print_all(&self) {
        for y in 0..self.resolution.1 {
            for x in 0..self.resolution.0 {
                let cell = &self.grid[x][y];
                if cell.active {
                    if cell.in_use {
                        print!("*");
                    } else {
                        print!(" ")
                    }
                } else {
                    print!(" ")
                }
            }
            println!("");
        }
    }

    fn in_bound(&self, coord: (isize, isize)) -> bool {
        coord.0 < self.resolution.0 as isize
            && coord.1 < self.resolution.1 as isize
            && coord.0 >= 0
            && coord.1 >= 0
    }

    fn mark_use(&mut self, coord: (usize, usize)) {
        self.grid[coord.0][coord.1].in_use = true;
    }

    fn unmark_use(&mut self, coord: (usize, usize)) {
        self.grid[coord.0][coord.1].in_use = false;
    }

    fn route_direction(&self, dir: Dir, pos: Vec2, target: Vec2, edge: &mut LayoutEdge) -> bool {
        //println!("{dir:?} {pos} -> {target}");
        //In vicinity aka. _reached target_
        if (pos - target).length() <= 2.0 {
            return true;
        }

        //route by following "dir" till we reach either the x or y coord of `target`
        let step = dir.into_vec2();

        //Check if that routing advance us at all
        if ((pos + step) - target).length() > (pos - target).length() {
            return false;
        }

        //now advance `step` until we reach one coord of `target` < step_size OR
        // until we reach an inactive cell

        let mut offset = Vec2::ZERO;
        let mut best_dist = (pos - target).length();
        loop {
            let next = pos + offset;
            //println!("   {next}");
            let cellid = self.loc_to_cell(next);

            if !self.in_bound((cellid.0 as isize, cellid.1 as isize)) {
                //println!("Not In bound");
                break;
            }

            if !self.grid[cellid.0][cellid.1].active {
                //println!("Not active");
                break;
            }

            let this_dist = (next - target).length();

            if this_dist < 1.0 {
                //println!("in vicinity");
                best_dist = this_dist;
                break;
            }

            //Also break if we are moving away again
            if this_dist > best_dist {
                //println!("wrong direction @ {next}");
                break;
            } else {
                best_dist = this_dist;
            }

            offset += step;
        }

        let mut new_pos = pos + offset;

        if new_pos == pos {
            //println!("Didn't move at all");
            return false;
        }

        if new_pos.y < target.y {
            //println!("Fixing pos y");
            new_pos.y = target.y;
        }

        if best_dist < 2.0 {
            new_pos.x = target.x;
        }

        if dir.is_vertical() {
            //now try to plot route in all directions from this location
            if self.route_direction(Dir::Left, new_pos, target, edge) {
                //add this pos to the edge list

                edge.path.push(new_pos);
                return true;
            }
            if self.route_direction(Dir::Right, new_pos, target, edge) {
                //add this pos to the edge list
                edge.path.push(new_pos);
                return true;
            }
        } else {
            if self.route_direction(Dir::Up, new_pos, target, edge) {
                //add this pos to the edge list
                edge.path.push(new_pos);
                return true;
            }
        }
        false
    }
}

impl RegionLayout {
    ///Translates the `port` to an actual location. Panics if
    /// the node is not in the region, or the port is not an argument/result to this region.
    fn out_port_to_location<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static>(
        &self,
        rvsdg: &Rvsdg<N, E>,
        port: &OutportLocation,
        edge: EdgeRef,
    ) -> Vec2 {
        if port.node == self.src.node {
            //Similarly to below, search for the edge in all argument ports
            for (idx, arg) in rvsdg
                .region(&self.src)
                .unwrap()
                .arguments
                .iter()
                .enumerate()
            {
                if arg.edges.contains(&edge) {
                    return self.arg_ports[idx];
                }
            }
            panic!("Edge was not contained in arguments!");
        } else {
            //TODO no OutputType -> index mapping atm. So we just search for the output edge
            // in all output ports.
            if let Some(node) = self.nodes.get(&port.node) {
                for (idx, outport) in rvsdg.node(port.node).outputs().iter().enumerate() {
                    if outport.edges.contains(&edge) {
                        return node.outports[idx] + node.location;
                    }
                }
            }
            panic!("Could not find output port on node {:?}", port);
        }
    }

    ///Translates the `port` to an actual location. Panics if
    /// the node is not in the region, or the port is not an argument/result to this region.
    fn in_port_to_location<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static>(
        &self,
        rvsdg: &Rvsdg<N, E>,
        port: &InportLocation,
        edge: EdgeRef,
    ) -> Vec2 {
        if port.node == self.src.node {
            //Similarly to below, search for the edge in all result ports
            for (idx, res) in rvsdg.region(&self.src).unwrap().results.iter().enumerate() {
                if res.edge == Some(edge) {
                    return self.res_ports[idx];
                }
            }
            panic!("Edge was not contained in results!");
        } else {
            //TODO no OutputType -> index mapping atm. So we just search for the output edge
            // in all output ports.
            if let Some(node) = self.nodes.get(&port.node) {
                for (idx, inports) in rvsdg.node(port.node).inputs().iter().enumerate() {
                    if inports.edge == Some(edge) {
                        return node.inports[idx] + node.location;
                    }
                }
            }
            panic!("Could not find output port on node {:?}", port);
        }
    }

    ///Recursively routes all children's edges, and then its own
    pub fn route_region<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
        config: &LayoutConfig,
    ) {
        for node in self.nodes.values_mut() {
            for sr in node.sub_regions.iter_mut() {
                sr.route_region(rvsdg, config);
            }
        }

        let mut grid = EdgeLayoutGrid::new_for_reg(self, config);

        for node in self.nodes.values() {
            grid.deactivate(node, config);
        }

        //now use the grid to route any port edges.
        for edgeref in &rvsdg.region(&self.src).unwrap().edges {
            let edge = rvsdg.edge(*edgeref);
            let start_pos = self.out_port_to_location(rvsdg, edge.src(), *edgeref);
            let end_pos = self.in_port_to_location(rvsdg, edge.dst(), *edgeref);

            let mut edge = LayoutEdge {
                src: *edgeref,
                path: Vec::with_capacity(grid.resolution.0 + grid.resolution.1),
            };

            let local_offset =
                Vec2::new(config.port_width as f32 / 2.0, config.routing_dead_padding);
            let offseted_start_pos = start_pos + (local_offset * Vec2::new(1.0, -1.0));
            let offseted_end_pos = end_pos + local_offset;

            edge.path
                .push(end_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));

            if grid.route_direction(Dir::Up, offseted_start_pos, offseted_end_pos, &mut edge) {
                edge.path
                    .push(start_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));
            } else {
                println!("Could not route");
                edge.path.clear();
                edge.path
                    .push(start_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));
                edge.path
                    .push(end_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));
            }

            self.edges.push(edge);
        }

        //grid.print_all();
        //println!();
    }
}

impl<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static> Layout<'a, N, E> {
    pub fn route_edges(&mut self) {
        self.region_tree.route_region(&self.src_graph, &self.config)
    }
}
