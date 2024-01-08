use crate::{
    primitives::{color_styling, Rect},
    Stroke, View,
};
use ahash::{AHashMap, AHashSet};
use macroquad::prelude::{Color, Vec2, BLACK, WHITE};
use rvsdg::{edge::LangEdge, nodes::LangNode, region::Region, EdgeRef, NodeRef, Rvsdg};
use std::collections::VecDeque;
use std::fmt::Debug;

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

#[derive(Debug, Clone)]
enum BodyRegionPort {
    Arg(usize),
    Res(usize),
    Inp {
        line: usize,
        column: usize,
        input: usize,
    },
    Out {
        line: usize,
        column: usize,
        output: usize,
    },
}

#[derive(Debug, Clone)]
struct BodyEdge {
    color: Color,
    stroke: Stroke,
    src: BodyRegionPort,
    dst: BodyRegionPort,
}

///The nodes in a region.
/// there are lines of nodes in this body
#[derive(Debug, Clone)]
struct BodyRegion {
    area: Rect,
    nodes: Vec<Vec<BodyNode>>,
    arg_ports: Vec<Rect>,
    res_ports: Vec<Rect>,
    edges: Vec<BodyEdge>,
}

impl BodyRegion {
    pub fn flip_y(&mut self) {
        let ext = self.area.extent();
        for line in self.nodes.iter_mut() {
            for node in line {
                node.flip_y(ext);
            }
        }

        //also flip the arg/res ports.
        for arg in &mut self.arg_ports {
            arg.from.y = 0.0;
            arg.to.y = AnyNode::PORT_HEIGHT;
        }

        for res in &mut self.res_ports {
            res.from.y = ext.y - AnyNode::PORT_HEIGHT;
            res.to.y = ext.y;
        }
    }

    fn body_port_loc(&self, port: &BodyRegionPort) -> Vec2 {
        match port {
            BodyRegionPort::Arg(i) => self.arg_ports[*i].center(),
            BodyRegionPort::Res(i) => self.res_ports[*i].center(),
            BodyRegionPort::Inp {
                line,
                column,
                input,
            } => {
                let node = &self.nodes[*line][*column].node;
                let local_port = node.in_ports[*input].center();
                local_port + self.nodes[*line][*column].location
            }
            BodyRegionPort::Out {
                line,
                column,
                output,
            } => {
                let node = &self.nodes[*line][*column].node;
                let local_port = node.out_ports[*output].center();
                local_port + self.nodes[*line][*column].location
            }
        }
    }

    ///Emits this, and all subnodes into the buffer.
    pub fn emit_svg(&self, buffer: &mut Vec<String>) {
        buffer.push(self.area.emit_svg("REGION".to_owned()));

        buffer.push(format!(
            "<g transform=\"translate({}, {})\">",
            self.area.from.x, self.area.from.y,
        ));
        for line in &self.nodes {
            for node in line {
                node.emit_svg(buffer);
            }
        }

        for arg in &self.arg_ports {
            buffer.push(arg.emit_svg("ARG".to_owned()));
        }
        for res in &self.res_ports {
            buffer.push(res.emit_svg("RES".to_owned()));
        }

        for edge in &self.edges {
            let src_loc = self.body_port_loc(&edge.src);
            let dst_loc = self.body_port_loc(&edge.dst);
            buffer.push(format!(
                "<line x1=\"{}\" y1=\"{}\" x2=\"{}\" y2=\"{}\" {} style=\"stroke:{};stroke-width:0.5\"/>",
                src_loc.x,
                src_loc.y,
                dst_loc.x,
                dst_loc.y,
                edge.stroke.into_svg(),
                color_styling(&edge.color)
            ));
        }

        buffer.push(format!("</g>"));
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
    pub const YSHIFT: f32 = 30.0;
    pub const FONT_SIZE: f32 = Self::PADDING - 1.0;

    pub const PORT_WIDTH: f32 = 5.0;
    pub const PORT_HEIGHT: f32 = 2.0;
    pub const PORT_PADDING: f32 = 7.5;

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
            yoff += line_height + Self::YSHIFT;
        }

        //set the x offset
        region.area = Rect::empty(WHITE);
        let ext = Vec2::new(max_x + Self::PADDING, yoff + Self::PADDING);
        region.area.to = ext;

        //after layouting all nodes, layout argument and result ports
        let mut arg_x_off = Self::PORT_WIDTH;
        for arg in &mut region.arg_ports {
            arg.from = Vec2::new(arg_x_off, ext.y - Self::PORT_HEIGHT);
            arg.to = Vec2::new(arg_x_off + Self::PORT_WIDTH, ext.y);
            arg_x_off += Self::PORT_PADDING;
        }

        let mut res_x_off = Self::PORT_WIDTH;
        for res in &mut region.res_ports {
            res.from = Vec2::new(res_x_off, 0.0);
            res.to = Vec2::new(res_x_off + Self::PORT_WIDTH, Self::PORT_HEIGHT);
            res_x_off += Self::PORT_PADDING;
        }

        region.area.to.x = region
            .area
            .to
            .x
            .max(res_x_off + Self::PORT_WIDTH)
            .max(arg_x_off + Self::PORT_WIDTH);

        region.area.extent()
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

        let ext = self.node_region.extent();

        //After layouting the node itself, add the in/out ports
        // as simple rects. Inports are at the bottom side, outports at the top.
        let mut inport_x_off = Self::PADDING + Self::PORT_WIDTH;
        for inp in self.in_ports.iter_mut() {
            //TODO right now we just add x ports, and hope that it doesn't overflow. We could
            // also center / scale the ports. But ain't nobody got time for that.
            inp.from = Vec2::new(inport_x_off, ext.y);
            inp.to = Vec2::new(inport_x_off + Self::PORT_WIDTH, ext.y + Self::PORT_HEIGHT);
            inport_x_off += Self::PORT_PADDING;
        }

        let mut outport_x_off = Self::PADDING + Self::PORT_WIDTH;
        for outp in self.out_ports.iter_mut() {
            outp.from = Vec2::new(outport_x_off, -Self::PORT_HEIGHT);
            outp.to = Vec2::new(outport_x_off + Self::PORT_WIDTH, 0.0);
            outport_x_off += Self::PORT_PADDING;
        }

        ext
    }

    pub fn flip_y(&mut self) {
        for reg in self.regions.iter_mut() {
            reg.flip_y()
        }
        //also flip the input/output regs.
        for inp in &mut self.in_ports {
            inp.from.y = -Self::PORT_HEIGHT;
            inp.to.y = 0.0;
        }

        let ext = self.node_region.extent();
        for out in &mut self.out_ports {
            out.from.y = ext.y;
            out.to.y = ext.y + Self::PORT_HEIGHT;
        }
    }

    ///Emits this, and all subnodes into the buffer.
    pub fn emit_svg(&self, buffer: &mut Vec<String>) {
        //emit the rect that fills that node
        buffer.push(
            self.node_region
                .emit_svg(format!("{} - {}", self.name, self.nref)),
        );

        //also emit all the ports
        for outport in &self.out_ports {
            buffer.push(outport.emit_svg("OUTPORT".to_string()));
        }

        for inport in &self.in_ports {
            buffer.push(inport.emit_svg("INPORT".to_string()));
        }

        buffer.push(format!(
            "<text x=\"{}\" y=\"{}\" font-size=\"{}\" font-family=\"monospace\">{}</text>",
            Self::FONT_SIZE / 2.0,
            Self::FONT_SIZE - 1.0,
            Self::FONT_SIZE,
            self.name
        ));

        for region in &self.regions {
            region.emit_svg(buffer);
        }
    }

    fn min_size(&self) -> Vec2 {
        let num_chars = self.name.chars().count();
        let port_width = (self.in_ports.len().max(self.out_ports.len()) as f32 * Self::PORT_WIDTH)
            + (Self::PADDING * 2.0)
            + (Self::PORT_WIDTH * 2.0);
        Vec2::new(
            (num_chars as f32 * Self::FONT_SIZE).max(port_width),
            Self::FONT_SIZE,
        )
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

        //we also build two maps while exploring.
        // A edge map, collecting all edges _we know_,
        // as well as an NodeRef->usize map that lets us reference a node in the `nodes` vec by NodeRef,
        // later, when building the edges.
        let mut seen_edges = AHashSet::new();
        let mut node_ref_map: AHashMap<NodeRef, (usize, usize)> = AHashMap::default();
        //init deque
        for res in region.results.iter() {
            if let Some(edg) = res.edge {
                let _ = seen_edges.insert(edg);
                let node_ref = ctx.edge(edg).src().node;
                if !seen_nodes.contains(&node_ref) && node_ref != parent {
                    waiting_nodes.push_front(node_ref);
                    seen_nodes.insert(node_ref);
                }
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
                        let _ = seen_edges.insert(edg);
                        let node_ref = ctx.edge(edg).src().node;
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

                node_ref_map.insert(anynode.nref, (nodes.len(), level.len()));
                //push node into level
                level.push(BodyNode {
                    location: Vec2::ZERO,
                    node: anynode,
                });
            }

            nodes.push(level);
        }

        //Collect argument and result ports
        let arg_ports = region
            .arguments
            .iter()
            .map(|_p| Rect::empty(BLACK))
            .collect();

        let res_ports = region.results.iter().map(|_p| Rect::empty(BLACK)).collect();

        //TODO: The edge builder is currenly a little ugly.
        // Basically we iterate all known edges of the region.
        // We distinguish 4 connection cases
        // 1. arg - node: iterate all args, find the source port, then lookup
        // 2. arg - res: iterate all args, find the source port, then do the same for all res ports
        // 3. node - node: easy lookup
        // 4. node - res: lookup node, then iterate all res ports, searching for the dst port
        //
        // Technically there is a 5-th case: Invalid-edge. But we ignore that for now.

        let lookup_arg = |search_edge: &EdgeRef| {
            let mut found_arg = None;
            for (arg_idx, arg) in region.arguments.iter().enumerate() {
                if arg.edges.contains(search_edge) {
                    found_arg = Some(arg_idx);
                    break;
                }
            }
            found_arg
        };

        let lookup_res = |search_edge: &EdgeRef| {
            let mut found_res = None;
            for (res_idx, res) in region.results.iter().enumerate() {
                if res.edge == Some(*search_edge) {
                    found_res = Some(res_idx);
                    break;
                }
            }
            found_res
        };

        let lookup_output = |search_edge: &EdgeRef, node: NodeRef| {
            let mut found_out = None;
            for (port_idx, output) in ctx.node(node).outputs().iter().enumerate() {
                if output.edges.contains(search_edge) {
                    found_out = Some(port_idx);
                    break;
                }
            }
            found_out
        };

        let lookup_input = |search_edge: &EdgeRef, node: NodeRef| {
            let mut found_inp = None;
            for (port_idx, input) in ctx.node(node).inputs().iter().enumerate() {
                if input.edge == Some(*search_edge) {
                    found_inp = Some(port_idx);
                    break;
                }
            }
            found_inp
        };

        let mut edges = Vec::new();
        for edg in &region.edges {
            let edge = ctx.edge(*edg);
            //resolve both ports, then insert
            let src_port = if edge.src().output.is_argument() {
                if let Some(arg_idx) = lookup_arg(edg) {
                    BodyRegionPort::Arg(arg_idx)
                } else {
                    println!("ERROR: Could not find ArgIdx for output");
                    continue;
                }
            } else {
                if let Some(node_output) = lookup_output(edg, edge.src().node) {
                    let (line, column) = if let Some(e) =
                        node_ref_map.get(&edge.src().node).cloned()
                    {
                        e
                    } else {
                        println!("Node {:?} was not in lookup map, this means there are edges that are not connected to any export!", edge.src().node);
                        continue;
                    };
                    BodyRegionPort::Out {
                        line,
                        column,
                        output: node_output,
                    }
                } else {
                    println!("ERROR: Could not find node output for edge!");
                    continue;
                }
            };

            let dst_port = if edge.dst().input.is_result() {
                if let Some(res_idx) = lookup_res(edg) {
                    BodyRegionPort::Res(res_idx)
                } else {
                    println!("ERROR: Could not find ResIdx for input");
                    continue;
                }
            } else {
                if let Some(input_idx) = lookup_input(edg, edge.dst().node) {
                    let (line, column) = if let Some(e) =
                        node_ref_map.get(&edge.dst().node).cloned()
                    {
                        e
                    } else {
                        println!("Node {:?} was not in lookup map, this means there are edges that ore not connected to any export!", edge.dst().node);
                        continue;
                    };
                    BodyRegionPort::Inp {
                        line,
                        column,
                        input: input_idx,
                    }
                } else {
                    println!("ERROR: Could not find node input for edge!");
                    continue;
                }
            };

            edges.push(BodyEdge {
                color: edge.ty.color(),
                stroke: edge.ty.stroke(),
                src: src_port,
                dst: dst_port,
            });
        }

        BodyRegion {
            area: Rect::empty(WHITE),
            edges,
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
