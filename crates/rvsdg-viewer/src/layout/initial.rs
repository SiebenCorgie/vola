use ahash::AHashSet;
use glam::Vec2;
use rvsdg::{edge::LangEdge, nodes::LangNode, NodeRef, Rvsdg};

use crate::View;

use super::{LayoutConfig, RegionLayout};

pub struct NodeGrid {
    rows: Vec<Vec<NodeRef>>,
}

impl RegionLayout {
    pub fn sub_layout<'a, N: LangNode + View + 'static, E: LangEdge + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
        ignore_dead_nodes: bool,
    ) {
        for n in self.nodes.values_mut() {
            for reg in &mut n.sub_regions {
                reg.initial_layouting(rvsdg, ignore_dead_nodes);
            }
        }
    }

    ///Creates an initial layout for the region and its children
    pub fn initial_layouting<N: LangNode + View + 'static, E: LangEdge + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
        ignore_dead_nodes: bool,
    ) {
        log::trace!("Setting up initial layouting");

        #[derive(Debug, PartialEq, Eq, Hash)]
        struct WaitingNode {
            node: NodeRef,
            successors: Vec<NodeRef>,
        }

        self.sub_layout(rvsdg, ignore_dead_nodes);

        //For layouting we use an adapted bottom-up BFS.
        // The adaption being, that we defer the insertion of a node, if not all predecessors have been layouted yet.

        let region = rvsdg.region(&self.src).unwrap();

        let mut layouted: AHashSet<NodeRef> = AHashSet::new();
        let mut waiting: Vec<WaitingNode> = Vec::new();

        //Build the waiting-node for each in the region
        for (noderef, _) in self.nodes.iter() {
            let mut successor_set = AHashSet::new();

            let mut had_any_succ_connection = false;
            for succ in rvsdg.node(*noderef).succ(rvsdg) {
                //NOTE: do not add succ nod, if succ is not contained in the
                //      region layout. Otherwise we might block enqueing later on,
                //      cause a dead-node is never reached
                if !self.nodes.contains_key(&succ.node) && succ.node != self.src.node {
                    continue;
                }

                had_any_succ_connection = true;
                //branch prevents us from adding the sourrounding node.
                if region.nodes.contains(&succ.node) {
                    successor_set.insert(succ.node);
                }
            }

            //If the node has no successor, it is a dead node by definition.
            if ignore_dead_nodes && !had_any_succ_connection {
                //but mark as layouted anyways
                layouted.insert(*noderef);
                continue;
            }

            let waiting_node = WaitingNode {
                node: *noderef,
                successors: successor_set.into_iter().collect(),
            };
            waiting.push(waiting_node);
        }

        //Now, until all are layouted, or we found a cycle:
        // for each level, insert all nodes that have all predecessors layouted already
        let mut grid = NodeGrid { rows: Vec::new() };

        for y in 0.. {
            if waiting.is_empty() {
                break;
            }

            grid.rows.push(Vec::new());
            let mut layouted_any = false;
            //Move all waiting elements into the pingpong buffer. with_capacity(0) prevents
            // allocation here.
            let mut waiting_ping_pong = Vec::with_capacity(0);
            waiting_ping_pong.append(&mut waiting);

            //Collects all nodes layouted on this level. Othewise the
            // _layouted_ test later on might fire too early.
            let mut waiting_to_be_added = Vec::new();

            for w in waiting_ping_pong {
                let mut all_succ_layouted = true;
                for s in &w.successors {
                    if !layouted.contains(&s) {
                        all_succ_layouted = false;
                        break;
                    }
                }
                if all_succ_layouted {
                    layouted_any = true;
                    grid.rows[y].push(w.node);
                    waiting_to_be_added.push(w.node);
                } else {
                    waiting.push(w);
                }
            }

            for wtba in waiting_to_be_added {
                layouted.insert(wtba);
            }

            if !layouted_any && !waiting.is_empty() {
                log::error!(
                    "error: Could not layout all nodes, there are still {} nodes!",
                    waiting.len()
                );
                for w in &waiting {
                    log::error!("node named: {}", rvsdg.node(w.node).name());
                }
                break;
            }
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
        log::trace!("Setup Bottom-Up transfer grid");
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

            let node_port_max_x = node
                .inports
                .iter()
                .chain(node.outports.iter())
                .fold(0.0f32, |max, loc| max.max(loc.0.x));

            let ext = Vec2::new(
                label_ext
                    .x
                    .max(node_region_ext.x)
                    .max(node_port_max_x + config.port_width as f32 + 2.0),
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
                inp.0.y = node.extent.y;
            }
        }

        let mut offset_y = config.grid_padding as f32;
        let mut max_x = 0.0f32;
        for row in self.node_grid.as_ref().unwrap().rows.iter() {
            let mut offset_x = config.grid_padding as f32;
            let max_height = row
                .iter()
                .fold(0.0f32, |f, n| f.max(self.nodes.get(n).unwrap().extent.y));

            //we now use max height to position our nodes _lowest possible_,
            // as well as advancing the general _height_ variable later.
            for node in row {
                //TODO move down to line
                let node = self.nodes.get_mut(node).unwrap();
                node.location = Vec2::new(offset_x, offset_y);
                offset_x += node.extent.x + config.grid_padding as f32;
            }

            offset_y += max_height + config.grid_padding as f32;

            max_x = max_x.max(offset_x);
        }

        self.extent = Vec2::new(
            max_x.max(config.grid_empty_spacing as f32).max(
                self.arg_ports
                    .iter()
                    .chain(self.res_ports.iter())
                    .fold(0.0f32, |a, p| a.max(p.0.x + config.port_spacing as f32)),
            ),
            offset_y.max(config.grid_empty_spacing as f32),
        );
    }

    pub fn set_height(&mut self, height: f32, config: &LayoutConfig) {
        self.extent.y = height;
        for arg in &mut self.arg_ports {
            arg.0.y = height - config.port_height as f32;
        }
    }
}
