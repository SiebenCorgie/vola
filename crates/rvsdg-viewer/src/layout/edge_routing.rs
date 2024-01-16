use std::mem;

use ahash::{AHashMap, AHashSet};
use macroquad::math::Vec2;
use priority_queue::{DoublePriorityQueue, PriorityQueue};
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

type GridCoord = (usize, usize);
struct AStarAttrib {
    //The node, preceding this node on its _best_ path
    came_from: Option<GridCoord>,
    //the cost of the cheapest pathc from start to this node currently known.
    gscore: isize,
}

impl AStarAttrib {
    fn reset(&mut self) {
        self.gscore = isize::MAX;
        self.came_from = None;
    }
}

impl Default for AStarAttrib {
    fn default() -> Self {
        AStarAttrib {
            came_from: None,
            gscore: isize::MAX,
        }
    }
}

struct AStarSearch {
    openset: DoublePriorityQueue<GridCoord, isize>,
    attrib_map: AHashMap<GridCoord, AStarAttrib>,
    start: GridCoord,
    end: GridCoord,
    extent: (usize, usize),
}

impl AStarSearch {
    fn new() -> Self {
        AStarSearch {
            openset: DoublePriorityQueue::default(),
            attrib_map: AHashMap::default(),
            start: (0, 0),
            end: (0, 0),
            extent: (0, 0),
        }
    }

    fn fscore_for(&self, coord: GridCoord) -> isize {
        self.attrib_map
            .get(&coord)
            .unwrap()
            .gscore
            .checked_add(self.h(coord))
            .unwrap_or(isize::MAX)
    }

    //heuristical score, currently Manhattan distance
    fn h(&self, coord: GridCoord) -> isize {
        let end_vec = (
            (coord.0 as isize - self.start.0 as isize).abs(),
            (coord.1 as isize - self.start.1 as isize).abs(),
        );

        end_vec.0 + end_vec.1
    }

    ///Calculates how much it costs going from current to next
    fn d(&self, current: GridCoord, next: GridCoord) -> isize {
        //Penalize direction change
        let delta_add = if let Some(from) = self.attrib_map.get(&current).unwrap().came_from {
            let deltax = from.0 as isize - next.0 as isize;
            let deltay = from.1 as isize - next.1 as isize;

            if deltax != 0 && deltay != 0 {
                1
            } else {
                0
            }
        } else {
            0
        };
        let diffx = (current.0 as isize - next.0 as isize).abs();
        let diffy = (current.1 as isize - next.1 as isize).abs();
        diffx + diffy + delta_add
    }

    fn start_for(&mut self, resolution: (usize, usize), start: GridCoord, end: GridCoord) {
        self.openset.clear();
        self.openset.push(start, self.h(self.start));

        for x in 0..resolution.0 {
            for y in 0..resolution.1 {
                if let Some(attr) = self.attrib_map.get_mut(&(x, y)) {
                    attr.reset();
                } else {
                    self.attrib_map.insert((x, y), AStarAttrib::default());
                }
            }
        }

        //Init search
        self.attrib_map.get_mut(&start).unwrap().gscore = 0;
        assert!(self.fscore_for(start) == self.h(start));

        self.start = start;
        self.end = end;
    }

    fn offset_coord(&self, coord: GridCoord, offset: (isize, isize)) -> Option<GridCoord> {
        let x = coord.0 as isize + offset.0;
        if x < 0 || x >= self.extent.0 as isize {
            return None;
        }
        let y = coord.1 as isize + offset.1;
        if y < 0 || y >= self.extent.1 as isize {
            return None;
        }

        Some((x as usize, y as usize))
    }

    fn search(
        &mut self,
        grid: &Vec<Vec<ELCell>>,
        resolution: (usize, usize),
        start: GridCoord,
        end: GridCoord,
    ) -> Option<Vec<GridCoord>> {
        ///Neighbor offsets in the A* search. We _preffer_ short vertical over long horizontal
        /// movement.
        const OFFSETS: &'static [(isize, isize)] = &[
            (0, 1),
            (0, -1),
            //(1, -1),
            (-1, 0),
            //(-1, -1),
            (1, 0),
            (0, -2),
            (0, 2),
            (-2, 0),
            (2, 0),
        ];

        self.extent = resolution;

        //Reset data structure for new search.
        self.start_for(resolution, start, end);

        while !self.openset.is_empty() {
            let (current, _fscore) = self.openset.pop_min().unwrap();
            if current == end {
                return Some(self.reconstruct_path(current.clone()));
            }

            //Iterate all neighbors for seeding a search
            for offset in OFFSETS {
                let coord = if let Some(c) = self.offset_coord(current, *offset) {
                    c
                } else {
                    continue;
                };

                if !grid[coord.0][coord.1].active || grid[coord.0][coord.1].in_use {
                    continue;
                }

                let tentative_gscore =
                    self.attrib_map.get(&current).unwrap().gscore + self.d(current, coord);
                //Check if it is cheaper to go from current to neighbor, compared to the last known "best way" for neighbor.
                if tentative_gscore < self.attrib_map.get(&coord).unwrap().gscore {
                    //is better, update neighbor and enrow
                    let attrib = self.attrib_map.get_mut(&coord).unwrap();
                    attrib.came_from = Some(current);
                    attrib.gscore = tentative_gscore;
                    //Change the fscore, or add if not already in queue
                    let new_fscore = tentative_gscore + self.h(coord);
                    if self.openset.change_priority(&coord, new_fscore).is_none() {
                        self.openset.push(coord, new_fscore);
                    }
                }
            }
        }

        None
    }

    fn reconstruct_path(&mut self, current: GridCoord) -> Vec<GridCoord> {
        let mut last_key = current;
        let mut total_path = Vec::with_capacity(self.extent.0 + self.extent.1);
        while last_key != self.start {
            //find the came_from of the last key, use that as the next, and push last key
            let next_key = self
                .attrib_map
                .get(&last_key)
                .unwrap()
                .came_from
                .expect("Expected set last key!");
            total_path.push(last_key);
            last_key = next_key;
        }

        total_path.push(self.start);

        total_path
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
    search_helper: AStarSearch,
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

        let mut grid = vec![vec![ELCell::default(); resolution.1]; resolution.0];

        //mark all "outer" cells as "inactive, to prevent edges from clipping to the
        // outer walls
        for x in 0..resolution.0 {
            grid[x][0].active = false;
            grid[x][resolution.1 - 1].active = false;
        }

        for y in 0..resolution.1 {
            grid[0][y].active = false;
            grid[resolution.0 - 1][y].active = false;
        }

        EdgeLayoutGrid {
            resolution,
            cell_size: config.routing_cell_size,
            grid,
            search_helper: AStarSearch::new(),
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
                        print!("O");
                    } else {
                        print!("X")
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

    fn is_useable(&self, pos: Vec2) -> bool {
        let coord = self.loc_to_cell(pos);
        if !self.in_bound((coord.0 as isize, coord.1 as isize)) {
            return false;
        }

        !self.grid[coord.0][coord.1].in_use
    }

    fn set_used_if_intersects(&mut self, line: (Vec2, Vec2)) {
        let mut start_coord = self.loc_to_cell(line.0);
        let mut end_coord = self.loc_to_cell(line.1);
        if start_coord.0 > end_coord.0 {
            std::mem::swap(&mut start_coord.0, &mut end_coord.0);
        }
        if start_coord.1 > end_coord.1 {
            std::mem::swap(&mut start_coord.1, &mut end_coord.1);
        }
        start_coord.0 = start_coord.0.checked_sub(1).unwrap_or(0);
        start_coord.1 = start_coord.1.checked_sub(1).unwrap_or(0);
        end_coord.0 += 1;
        end_coord.1 += 1;

        let line_vector = (line.1 - line.0).normalize();

        //println!("{start_coord:?} -> {end_coord:?}");
        for x in start_coord.0..end_coord.0 {
            for y in start_coord.1..end_coord.1 {
                //we use vector projection to find out if cell is in b. For that we project the cell
                // onto the line vector. If the projection (a2 in this article: https://en.wikipedia.org/wiki/Vector_projection)
                // is furhter than `cell_size` we ignore, otherwise we mark.

                let a = self.cell_to_loc((x, y)) - line.0;
                let a2 = a - ((a.dot(line_vector)) * line_vector);
                if a2.length() < (self.cell_size / 2.0) {
                    //println!("marking: {x},{y}");
                    self.mark_use((x, y));
                }
            }
        }
    }

    ///Sets all cells _in_use_ that are touched by `edge`.
    fn set_in_use(&mut self, edge: &LayoutEdge) {
        //TODO: This is a little _unoptimized_. We constrain the iteration per line segment, and then check
        // each cell in the cluster agains the line for intersection.

        let num_segments = edge.path.len() - 1;
        for seg in 0..num_segments {
            let start = edge.path[seg];
            let end = edge.path[seg + 1];

            self.set_used_if_intersects((start, end));
        }
    }

    ///Finds a unused location _around_ `loc`. y_mul and from_right are used to weight the search to the left/right and
    /// clamp to +y or -y from `loc`.
    fn find_valid_around(&self, loc: Vec2, y_mul: isize, from_right: isize) -> Option<Vec2> {
        const SEARCH_OFFSETS: &'static [(isize, isize)] = &[
            //directly on port
            (0, 0),
            //1 below port
            (0, 1),
            (1, 1),
            (-1, 1),
            (2, 1),
            (-2, 1),
            (3, 1),
            (-3, 1),
            //2 below port
            (0, 2),
            (1, 2),
            (-1, 2),
            (2, 2),
            (-2, 2),
            (3, 2),
            (-3, 2),
        ];

        for offset in SEARCH_OFFSETS {
            let pos = loc
                + Vec2::new(
                    offset.0 as f32 * self.cell_size * from_right as f32,
                    offset.1 as f32 * self.cell_size * y_mul as f32,
                );
            if self.is_useable(pos) {
                return Some(pos);
            }
        }

        None
    }

    fn route_edge(&mut self, src: EdgeRef, start: GridCoord, end: GridCoord) -> Option<LayoutEdge> {
        let gridcoord_path = self
            .search_helper
            .search(&self.grid, self.resolution, start, end)?;
        let mut edge = LayoutEdge {
            src,
            path: Vec::with_capacity(gridcoord_path.len()),
        };
        for coord in gridcoord_path {
            edge.path.push(self.cell_to_loc(coord));
        }

        Some(edge)
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

            let start_port_location = start_pos
                + Vec2::new(
                    config.port_width as f32 / 2.0,
                    config.port_height as f32 / 2.0,
                );
            let end_port_location = end_pos
                + Vec2::new(
                    config.port_width as f32 / 2.0,
                    config.port_height as f32 / 2.0,
                );

            //Offset the start / end position vertically to not _fuse_ the edge with the node
            let local_offset = Vec2::new(0.0, config.routing_dead_padding);
            let offseted_start_pos = start_port_location + (local_offset * Vec2::new(1.0, -1.0));
            let offseted_end_pos = end_port_location + local_offset;

            //Before starting search, make sure we have a valid start/end position
            let start_to_right = if offseted_start_pos.x > offseted_end_pos.x {
                0
            } else {
                1
            };
            let route_start_pos = grid.find_valid_around(offseted_start_pos, -1, start_to_right);
            let route_end_pos = grid.find_valid_around(offseted_end_pos, 1, -1 * start_to_right);

            let edge = if let (Some(start), Some(end)) = (route_start_pos, route_end_pos) {
                if let Some(mut edge) =
                    grid.route_edge(*edgeref, grid.loc_to_cell(start), grid.loc_to_cell(end))
                {
                    grid.set_in_use(&edge);

                    //edge.path[0].x = end_port_location.x;
                    //edge.path.last_mut().unwrap().x = start_port_location.x;

                    edge.path.push(start_port_location);
                    edge.path.insert(0, end_port_location);
                    Some(edge)
                } else {
                    println!("Could not route fo real");
                    None
                }
            } else {
                println!(
                    "No port found: s={} e={}",
                    route_start_pos.is_some(),
                    route_end_pos.is_some()
                );
                None
            };

            let edge = if let Some(edg) = edge {
                edg
            } else {
                println!("Could not route");
                let mut edge = LayoutEdge {
                    src: *edgeref,
                    path: Vec::with_capacity(2),
                };
                edge.path.clear();
                edge.path.push(start_port_location);
                edge.path.push(end_port_location);
                edge
            };

            /*
            //Before starting the routing, make sure the startpoint is unused. Otherwise the path finding will not
            // work.
            //
            // We try to find a start point left or right from the offsetted start/end pos that is not in use
            //
            // We use `radius` for that and tab left and right to the port till we found something.
            let valid_start_pos = grid.find_valid_around(offseted_start_pos, false);
            let valid_end_pos = grid.find_valid_around(offseted_end_pos, true);

            if valid_end_pos.is_empty() || valid_start_pos.is_empty() {
                println!("Could not find valid start/end pos for port, falling back");
                edge.path.clear();
                edge.path
                    .push(start_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));
                edge.path
                    .push(end_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));
                continue;
            } else {
                //pre-push the port's location to have a visual connection
                edge.path
                    .push(end_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));

                let mut found_route = false;
                'search: for valid_start in valid_start_pos {
                    for valid_end in &valid_end_pos {
                        //now route based on the pre-checked valid start/end positions
                        if grid.route_direction(Dir::Up, valid_start, *valid_end, &mut edge) {
                            //post push port location for visual connection.
                            edge.path
                                .push(start_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));
                            //now mark the used path as _in use_.
                            grid.set_in_use(&edge);
                            found_route = true;
                            break 'search;
                        }
                    }
                }

                if !found_route {
                    println!("Could not route");
                    edge.path.clear();
                    edge.path
                        .push(start_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));
                    edge.path
                        .push(end_pos + Vec2::new(config.port_width as f32 / 2.0, 0.0));
                }
            }
            */
            self.edges.push(edge);
        }

        //grid.print_active();
        //println!();
        //grid.print_all();
    }
}

impl<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static> Layout<'a, N, E> {
    pub fn route_edges(&mut self) {
        self.region_tree.route_region(&self.src_graph, &self.config)
    }
}
