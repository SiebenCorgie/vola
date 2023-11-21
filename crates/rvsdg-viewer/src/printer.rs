use std::ops::Range;

use ahash::{AHashMap, AHashSet};
use rvsdg::{
    edge::LangEdge,
    label::LabelLoc,
    nodes::{LangNode, Node},
    NodeRef, RegionRef, Rvsdg,
};

use crate::{
    primitives::{Line, Point, Rect, Text},
    View,
};

///The printer works in three passes.
/// 1. Bottom up build list of blocks. This allocates space for regions, ports etc.
///    All Region locations / Port locations are safed in a Map, to later reference them.
/// 2. Draw edges. This iterates over all edges and draws a line on top of all blocks from the src and dst ports.
/// 3. Draw Label. Finally, we draw the label of blocks and edges to the formerly defined locations.
///
///
/// When finished, we can generate an ordered list of primitives that can be drawn either by the SVG printer, or the
/// macroquad renderer.
pub struct Printer {
    ///Location mapping for nodes, regions and ports. We abuse the label loc for now to reference any box or port.
    pub rect_mapping: AHashMap<LabelLoc, usize>,
    pub rects: Vec<Rect>,
    pub lines: Vec<Line>,
    pub label: Vec<Text>,
}

impl Printer {
    fn get_rect_area(&self, range: Range<usize>) -> (Point, Point) {
        let mut min = Point::splat(f32::INFINITY);
        let mut max = Point::splat(f32::NEG_INFINITY);

        for rect in &self.rects[range] {
            min = min.min(rect.from);
            max = max.max(rect.to);
        }
        (min, max)
    }

    ///Collects all nodes in that region. Also does the layouting of those nodes.
    /// Returns the range in `collection` that belongs to this region.
    fn rect_region_collector<N: View + LangNode + 'static, E: View + LangEdge + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
        reg: RegionRef,
    ) -> Range<usize> {
        //We build the regions nodes by stepping in parallel from all outputs, up the node chain, till we finished each tree.
        // at this point we assume that a region always consists of a tree. However, since the viewer will be used as a debugging
        // tool, we still check that criteria via the visited list.
        //
        // All in all the node location/exploration thingy is kinda a breadth-first bottom-up style algorithm.
        let region = rvsdg.region(reg);
        let mut visited = AHashSet::default();

        //Init with all src nodes of all result ports. So all nodes that are connected to a result
        let mut visit_list: Vec<NodeRef> = region
            .results
            .iter()
            .map(|port| port.edges.iter().map(|edge| rvsdg.edge(*edge).src))
            .flatten()
            .collect();
        let mut new_visit_list: Vec<NodeRef> = Vec::new();

        let rect_start = self.rects.len();

        let mut level = 0;
        'explorer: loop {
            //work on this level of nodes.
            //

            //If visit list is empty, we found the last node
            if visit_list.len() == 0 {
                break 'explorer;
            }
            //traverse full visit list, and build new visit list
            for to_visit in &visit_list {
                //Do not revist if already visited
                if visited.contains(&to_visit) {
                    continue;
                }

                //add this node to (being) visited.
                visited.insert(to_visit);

                //found an node we have to touch, therefore let this node do its own allocation,
                // then use that area information to schedule this level's area.
                let node_indices = self.rect_node_collector(rvsdg, *to_visit);
                let node_area = self.get_rect_area(node_indices);
            }

            std::mem::swap(&mut visit_list, &mut new_visit_list);
        }

        rect_start..self.rects.len()
    }

    ///Layouts sub regions / nodes of this node, then places self starting at the center.
    fn rect_node_collector<N: View + LangNode + 'static, E: View + LangEdge + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
        node: NodeRef,
    ) -> Range<usize> {
        let regions = rvsdg.node(node).regions();

        let this_rects_start = self.rects.len();
        let mut this_rects_end = self.rects.len();
        if regions.len() > 1 {
            //Add regions next to each other.
        } else {
            //Single region, just find area and nodes.
        }

        //register nodes input and output ports. Regions should already be done
        let node = rvsdg.node(node);
        let inp_count = node.inputs().len();
        for (i, inp) in node.inputs().iter().enumerate() {
            //
        }

        let out_count = node.outputs().len();
        for (i, out) in node.outputs().iter().enumerate() {
            //calc port location
            //register location
            //emit rect
        }

        0..1
    }

    ///Creates a printer for the `graph`.
    pub fn new<N: View + LangNode + 'static, E: View + LangEdge + 'static>(
        graph: &Rvsdg<N, E>,
    ) -> Self {
        let mut printer = Printer {
            rect_mapping: AHashMap::default(),
            rects: Vec::with_capacity(100),
            lines: Vec::with_capacity(100),
            label: Vec::with_capacity(100),
        };

        //collect all boxes and build location mapping
        let _gamma_range = printer.rect_node_collector(graph, *graph.entry_node());

        printer
    }
}
