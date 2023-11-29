use ahash::AHashSet;
use macroquad::prelude::{Vec2, WHITE};
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
}

impl BodyRegion {
    ///Emits this, and all subnodes into the buffer.
    pub fn emit_svg(&self, buffer: &mut Vec<String>) {
        buffer.push(format!(
            "<rect width=\"{}\" height=\"{}\" style=\"fill:{}\" />",
            self.area.extend().x,
            self.area.extend().y,
            color_styling(&self.area.color)
        ));

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
}

impl AnyNode {
    pub const PADDING: f32 = 10.0;
    pub const FONT_SIZE: f32 = Self::PADDING - 1.0;
    ///Layouts the nodes within this node
    pub fn build_layout<N: LangNode + View + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &Rvsdg<N, E>,
    ) -> Vec2 {
        //build the body region
        let mut reg_xoff = Self::PADDING;
        let mut reg_max_height = Self::PADDING;
        for region in self.regions.iter_mut() {
            //Iterate over all nodes of that region _in order_
            let mut yoff = Self::PADDING;
            let mut max_x = Self::PADDING;
            for line in region.nodes.iter_mut() {
                //reset the x_offset
                let mut xoff = Self::PADDING;
                let mut line_height = 0.0f32;
                for node in line.iter_mut() {
                    //for each node on this line, let it calculate its inner
                    // extend, then use that to
                    // - set the node location
                    // - advance the offset appropriately
                    // - update the region's body extend (y always updates, x might, if we overflow).
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
            region.area.from = Vec2::new(reg_xoff, Self::PADDING);
            region.area.to = region.area.from + Vec2::new(max_x, yoff + Self::PADDING);
            reg_max_height = reg_max_height.max(region.area.extend().y + Self::PADDING);
            //now update the offset
            reg_xoff += region.area.extend().x + Self::PADDING;
        }

        let min_region = self.min_size();
        self.node_region = Rect::empty(ctx.node(self.nref).color());
        self.node_region.to = Vec2::new(
            reg_xoff.max(min_region.x),
            reg_max_height.max(min_region.y) + Self::PADDING,
        );
        self.node_region.color = ctx.node(self.nref).color();
        self.node_region.extend()
    }

    ///Emits this, and all subnodes into the buffer.
    pub fn emit_svg(&self, buffer: &mut Vec<String>) {
        //emit the rect that fills that node
        buffer.push(format!(
            "<rect id=\"{}\" x=\"0\" y=\"0\" width=\"{}\" height=\"{}\" style=\"fill:{}\" />",
            format!("{} - {}", self.name, self.nref),
            self.node_region.extend().x,
            self.node_region.extend().y,
            color_styling(&self.node_region.color)
        ));

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
                    AnyNode {
                        nref: node,
                        name,
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

        BodyRegion {
            area: Rect::empty(WHITE),
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

        AnyNode {
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

    pub fn emit_svg(&self) -> String {
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
