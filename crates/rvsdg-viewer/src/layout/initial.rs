use std::ops::Not;

use ahash::{AHashMap, AHashSet};
use rvsdg::{edge::LangEdge, nodes::LangNode, NodeRef, Rvsdg};

use super::RegionLayout;

struct NodeGrid {
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
            self.node_grid_explore(rvsdg, &mut grid, seed_node.node, (seed_index, 0));
            seed_index += 1;
        }

        grid.print();
        //Now restart and fix all grid criterias
        let mut seed_index = 0usize;
        while let Some(seed_node) = region.result_src(rvsdg, seed_index) {
            self.fix_grid_criteria(rvsdg, &mut grid, seed_node.node);
            seed_index += 1;
        }

        grid.print();
    }
}
