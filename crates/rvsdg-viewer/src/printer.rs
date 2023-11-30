use ahash::AHashSet;
use macroquad::prelude::{Vec2, BLACK, WHITE};
use rvsdg::{edge::LangEdge, nodes::LangNode, region::Region, NodeRef, Rvsdg};
use std::collections::VecDeque;
use std::fmt::Debug;

use crate::{
    primitives::{color_styling, Rect},
    View,
};

#[derive(Debug, Clone)]
struct BodyNode {
    //Relative location in the parent
    location: Vec2,
    node: AnyNode,
}

impl BodyNode {
    pub fn flip_y(&mut self, parent_extent: Vec2) {
        self.location.y = parent_extent.y - self.location.y - self.node.node_region.extent().y;
        self.node.flip_y();
    }

    ///Emits this, and all subnodes into the buffer.
    pub fn emit_svg(&self, buffer: &mut Vec<String>) {
        buffer.push(format!(
            "<g id=\"{}\" transform=\"translate({}, {})\">",
            format!("OffsetFor({})", self.node.nref),
            self.location.x,
            self.location.y
        ));

        self.node.emit_svg(buffer);

        buffer.push("</g>".to_string());
    }
}

///The nodes in a region.
/// there are lines of nodes in this body
#[derive(Debug, Clone)]
struct BodyRegion {
    area: Rect,
    nodes: Vec<Vec<BodyNode>>,
    arg_ports: Vec<Rect>,
    res_ports: Vec<Rect>,
}

impl BodyRegion {
    pub fn flip_y(&mut self) {
        let ext = self.area.extent();
        for line in self.nodes.iter_mut() {
            for node in line {
                node.flip_y(ext);
            }
        }
    }

    ///Emits this, and all subnodes into the buffer.
    pub fn emit_svg(&self, buffer: &mut Vec<String>) {
        buffer.push(self.area.emit_svg("".to_owned()));
        for line in &self.nodes {
            for node in line {
                node.emit_svg(buffer);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnyNode {
    nref: NodeRef,
    name: String,
    ///Body defined by `AnyNodes`, a x-index, and a y-index indicating their position
    regions: Vec<BodyRegion>,
    node_region: Rect,
    //input / output ports
    in_ports: Vec<Rect>,
    out_ports: Vec<Rect>,
}

impl AnyNode {
    pub const PADDING: f32 = 10.0;
    pub const FONT_SIZE: f32 = Self::PADDING - 1.0;

    ///Builds the recursive layout for this region, return the Extent of that region
    pub fn build_region_layout<N: LangNode + View + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &Rvsdg<N, E>,
        region: usize,
    ) -> Vec2 {
        let region = &mut self.regions[region];
        let mut yoff = Self::PADDING;
        let mut max_x = Self::PADDING;
        for line in region.nodes.iter_mut() {
            //reset the x_offset
            let mut xoff = Self::PADDING;
            let mut line_height = 0.0f32;
            for node in line.iter_mut() {
                //for each node on this line, let it calculate its inner
                // extent, then use that to
                // - set the node location
                // - advance the offset appropriately
                // - update the region's body extent (y always updates, x might, if we overflow).
                let sub_ext = node.node.build_layout(ctx);
                node.location = Vec2::new(xoff, yoff);
                line_height = line_height.max(sub_ext.y);
                xoff += sub_ext.x + Self::PADDING;
            }
            max_x = max_x.max(xoff);
            //add to the y offset for the next line
            yoff += line_height + Self::PADDING;
        }

        //set the x offset
        region.area = Rect::empty(WHITE);

        let ext = Vec2::new(max_x + Self::PADDING, yoff + Self::PADDING);

        region.area.to = ext;
        ext
    }

    ///Layouts the nodes within this node
    pub fn build_layout<N: LangNode + View + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &Rvsdg<N, E>,
    ) -> Vec2 {
        //build the body region
        let mut reg_xoff = Self::PADDING;
        let mut reg_max_height = Self::PADDING;

        for region in 0..self.regions.len() {
            let ext = self.build_region_layout(ctx, region);

            let local_offset = Vec2::new(reg_xoff, Self::PADDING);
            //now offset that region to its location and update the min/maxes
            self.regions[region].area.from += local_offset;
            self.regions[region].area.to += local_offset;
            reg_xoff += ext.x + Self::PADDING;
            reg_max_height = reg_max_height.max(ext.y + Self::PADDING);
        }

        //Now calculate how much space this node needs to occupy. Incorporate
        // - Text size,
        // - sub region size (if any)
        let min_region = self.min_size();
        self.node_region = Rect::empty(ctx.node(self.nref).color());
        self.node_region.to = Vec2::new(
            reg_xoff.max(min_region.x),
            reg_max_height.max(min_region.y) + Self::PADDING,
        );
        self.node_region.color = ctx.node(self.nref).color();
        self.node_region.extent()
    }

    pub fn flip_y(&mut self) {
        for reg in self.regions.iter_mut() {
            reg.flip_y()
        }
    }

    ///Emits this, and all subnodes into the buffer.
    pub fn emit_svg(&self, buffer: &mut Vec<String>) {
        //emit the rect that fills that node
        buffer.push(
            self.node_region
                .emit_svg(format!("{} - {}", self.name, self.nref)),
        );

        buffer.push(format!(
            "<text x=\"{}\" y=\"{}\" font-size=\"{}\" font-family=\"monospace\">{}</text>",
            Self::FONT_SIZE / 2.0,
            Self::FONT_SIZE - 1.0,
            Self::FONT_SIZE,
            self.name
        ));

        for region in &self.regions {
            buffer.push(format!(
                "<g id=\"{}\" transform=\"translate({}, {})\">",
                format!("AnyNode({})", self.nref),
                region.area.from.x,
                region.area.from.y
            ));

            region.emit_svg(buffer);

            buffer.push("</g>".to_string());
        }
    }

    fn min_size(&self) -> Vec2 {
        let num_chars = self.name.chars().count();
        Vec2::new(num_chars as f32 * Self::FONT_SIZE, Self::FONT_SIZE)
    }
}

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
    pub root: AnyNode,
}

impl Printer {
    fn collect_region_nodes<N: View + LangNode + Debug + 'static, E: View + LangEdge + 'static>(
        ctx: &Rvsdg<N, E>,
        region: &Region,
        parent: NodeRef,
    ) -> BodyRegion {
        let mut nodes = Vec::new();

        let mut seen_nodes = AHashSet::new();
        let mut waiting_nodes = VecDeque::new();
        //init deque
        for res in region.results.iter() {
            if let Some(edg) = res.edge {
                let node_ref = ctx.edge(edg).src.node;
                if node_ref == parent {
                    continue;
                }
                waiting_nodes.push_front(node_ref);
                seen_nodes.insert(node_ref);
                //TODO collect edge as well
            }
        }

        //now do bfs until we found all

        while !waiting_nodes.is_empty() {
            let mut level = Vec::new();
            let mut nodes_on_level = VecDeque::new();
            std::mem::swap(&mut nodes_on_level, &mut waiting_nodes);

            while let Some(node) = nodes_on_level.pop_back() {
                //first check all inputs, then check if we need to recurse, finally
                // push self on level collector;

                for inp in ctx.node(node).inputs() {
                    if let Some(edg) = inp.edge {
                        let node_ref = ctx.edge(edg).src.node;
                        if !seen_nodes.contains(&node_ref) && node_ref != parent {
                            waiting_nodes.push_front(node_ref);
                            seen_nodes.insert(node_ref);
                        }
                    }
                }

                //check node for recursion
                let anynode = if ctx.node(node).regions().len() > 0 {
                    Self::tree_builder(ctx, node)
                } else {
                    let color = ctx.node(node).color();
                    let name = ctx.node(node).name().to_owned();
                    let in_ports = ctx
                        .node(node)
                        .inputs()
                        .iter()
                        .map(|_p| Rect::empty(BLACK))
                        .collect();
                    let out_ports = ctx
                        .node(node)
                        .outputs()
                        .iter()
                        .map(|_p| Rect::empty(BLACK))
                        .collect();
                    AnyNode {
                        nref: node,
                        name,
                        in_ports,
                        out_ports,
                        regions: Vec::with_capacity(0),
                        node_region: Rect::empty(color),
                    }
                };

                //push node into level
                level.push(BodyNode {
                    location: Vec2::ZERO,
                    node: anynode,
                });
            }

            nodes.push(level);
        }

        let arg_ports = region
            .arguments
            .iter()
            .map(|_p| Rect::empty(BLACK))
            .collect();
        let res_ports = region.results.iter().map(|_p| Rect::empty(BLACK)).collect();
        BodyRegion {
            area: Rect::empty(WHITE),
            arg_ports,
            res_ports,
            nodes,
        }
    }

    fn tree_builder<N: View + LangNode + Debug + 'static, E: View + LangEdge + 'static>(
        ctx: &Rvsdg<N, E>,
        node: NodeRef,
    ) -> AnyNode {
        //iterate all reachables // _connected-to-result_ nodes in a breadth-first manor.
        // If a node is structural,
        let mut regions = Vec::with_capacity(0);
        for region in ctx.node(node).regions() {
            //explore all nodes in that region, collecting the body in the process
            regions.push(Self::collect_region_nodes(ctx, region, node));
        }
        let in_ports = ctx
            .node(node)
            .inputs()
            .iter()
            .map(|_p| Rect::empty(BLACK))
            .collect();
        let out_ports = ctx
            .node(node)
            .outputs()
            .iter()
            .map(|_p| Rect::empty(BLACK))
            .collect();

        AnyNode {
            in_ports,
            out_ports,
            name: ctx.node(node).name().to_owned(),
            nref: node,
            regions,
            node_region: Rect::empty(WHITE),
        }
    }

    ///Creates a printer for the `graph`.
    pub fn new<N: View + LangNode + Debug + 'static, E: View + LangEdge + 'static>(
        graph: &Rvsdg<N, E>,
    ) -> Self {
        let rootnode = graph.entry_node();
        Self {
            root: Self::tree_builder(graph, rootnode),
        }
    }

    pub fn layout<N: View + LangNode + Debug + 'static, E: View + LangEdge + 'static>(
        &mut self,
        ctx: &Rvsdg<N, E>,
    ) {
        self.root.build_layout(ctx);
    }

    pub fn emit_svg(&mut self) -> String {
        let mut line_buffer = Vec::new();

        self.root.emit_svg(&mut line_buffer);

        let mut svg = String::from("<svg>\n");
        for line in line_buffer {
            svg.push_str(&format!("{}\n", line));
        }
        svg.push_str("</svg>\n");
        svg
    }
}
