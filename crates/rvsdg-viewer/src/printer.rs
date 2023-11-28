use std::ops::Range;

use ahash::{AHashMap, AHashSet};
use macroquad::prelude::{Color, WHITE};
use rvsdg::{
    edge::LangEdge,
    label::LabelLoc,
    nodes::{LangNode, Node},
    region::Region,
    NodeRef, Rvsdg,
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
///
///
/// # Technical details.
///
/// We assume that a region/node, when build always originates at 0.0, which is the nodes lower-left corner
/// The positive quadrand is top-right.
pub struct Printer {
    ///Location mapping for nodes, regions and ports. We abuse the label loc for now to reference any box or port.
    pub rect_mapping: AHashMap<LabelLoc, usize>,
    pub rects: Vec<Rect>,
    pub lines: Vec<Line>,
    pub label: Vec<Text>,
}

impl Printer {
    pub const PADDING: f32 = 10.0;

    fn get_rect_area(&self, range: Range<usize>) -> (Point, Point) {
        let mut min = Point::splat(f32::INFINITY);
        let mut max = Point::splat(f32::NEG_INFINITY);

        for rect in &self.rects[range] {
            min = min.min(rect.from);
            max = max.max(rect.to);
        }
        (min, max)
    }

    ///Offsets all nodes in `range` for `offset`
    fn offset_rect(&mut self, range: Range<usize>, offset: Point) {
        for i in range {
            let rect = &mut self.rects[i];
            rect.from += offset;
            rect.to += offset;
        }
    }

    ///Collects all nodes in that region. Also does the layouting of those nodes.
    /// Returns the range in `collection` that belongs to this region.
    fn rect_region_collector<N: View + LangNode + 'static, E: View + LangEdge + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
        region: &Region,
    ) -> Range<usize> {
        println!("Region!");
        //We build the regions nodes by stepping in parallel from all outputs, up the node chain, till we finished each tree.
        // at this point we assume that a region always consists of a tree. However, since the viewer will be used as a debugging
        // tool, we still check that criteria via the visited list.
        //
        // All in all the node location/exploration thingy is kinda a breadth-first bottom-up style algorithm.
        let mut visited = AHashSet::default();

        //Init with all src nodes of all result ports. So all nodes that are connected to a result
        let mut visit_list: Vec<NodeRef> = region
            .results
            .iter()
            .map(|port| port.edge.iter().map(|edge| rvsdg.edge(*edge).src.node))
            .flatten()
            .collect();

        let rect_start = self.rects.len();

        //Vertical offest of the nodes being placed
        let mut ver_offset = 0.0;
        let mut max_hor = 0.0f32;

        'explorer: loop {
            //work on this level of nodes, swap out the visit list, and start a new one for the coming level
            let mut work_list = Vec::new();
            std::mem::swap(&mut work_list, &mut visit_list);

            //Right now, we just position all new nodes of this level next to each other.
            let mut hor_offest = 0.0f32;
            //track level nodes max width and heigh.
            let mut max_height = 0.0f32;
            //If visit list is empty, we found the last node
            if work_list.len() == 0 {
                break 'explorer;
            }

            //traverse full visit list, and build new visit list
            for to_visit in work_list.into_iter() {
                //Do not revisit if already visited
                if visited.contains(&to_visit) {
                    continue;
                }

                //add this node to (being) visited.
                visited.insert(to_visit);

                //found an node we have to touch, therefore let this node do its own allocation,
                // then use that area information to schedule this level's area.
                let node_indices = self.rect_node_collector(rvsdg, to_visit);
                let node_area = self.get_rect_area(node_indices.clone());
                let ext = node_area.1 - node_area.0;
                //move nodes to this slot's offset
                self.offset_rect(node_indices, Point::new(hor_offest, ver_offset));
                //now add the horizontal offset, and update max height
                hor_offest += ext.x;
                max_height = max_height.max(ext.y);
            }
            max_hor = max_hor.max(hor_offest);
            //add horizontal offset
            ver_offset += max_height;
        }

        //After setting up all sub nodes, move them by half the padding, and add a white box, enclosing those
        let sub_regions = rect_start..self.rects.len();
        let sub_ext = (max_hor, ver_offset);
        self.offset_rect(sub_regions, Point::splat(Self::PADDING));
        self.rects.push(Rect {
            from: Point::ZERO,
            to: Point::new(sub_ext.0, sub_ext.1),
            color: WHITE,
        });

        rect_start..self.rects.len()
    }

    ///Layouts sub regions / nodes of this node, then places self starting at the center.
    fn rect_node_collector<N: View + LangNode + 'static, E: View + LangEdge + 'static>(
        &mut self,
        rvsdg: &Rvsdg<N, E>,
        node: NodeRef,
    ) -> Range<usize> {
        //We draw nodes by first letting the inner region(s) draw them selfs
        // then drawing the node around those (+ a little padding for readablity)
        println!("Node!");

        let rect_start = self.rects.len();
        let regions = rvsdg.node(node).regions();
        let mut max_height = 0.0f32;
        let mut hor_offset = 0.0;
        for reg in regions {
            let range = self.rect_region_collector(rvsdg, reg);
            if hor_offset != 0.0 {
                //If offset region to the right.
                self.offset_rect(range.clone(), Point::new(hor_offset, 0.0));
            }

            let area = self.get_rect_area(range);
            let ext = area.1 - area.0;
            max_height = max_height.max(ext.y);
            hor_offset += ext.x;
        }

        //All boxes of the sub region
        let sub_regions_range = rect_start..self.rects.len();
        //Offset all children by half the padding
        self.offset_rect(sub_regions_range.clone(), Point::splat(Self::PADDING / 2.0));
        //Build the we want to color based on the node
        let sub_area = self.get_rect_area(sub_regions_range.clone());
        println!(
            "{} for {:?}, {}..{}",
            node, sub_area, sub_regions_range.start, sub_regions_range.end
        );
        let sub_ext = sub_area.1 - sub_area.0;

        let node_rect = Rect {
            from: Point::ZERO,
            to: sub_ext + Point::splat(Self::PADDING),
            color: rvsdg.node(node).color(),
        };
        self.rects.push(node_rect);

        /*
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
        */

        rect_start..self.rects.len()
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
        let _gamma_range = printer.rect_node_collector(graph, graph.entry_node());

        println!("Collected: {} rects", printer.rects.len());

        printer
    }
}
