/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use rvsdg::{edge::LangEdge, nodes::LangNode};

use crate::{
    primitives::{Color, Path, Point, Prim, PrimTree, Rect, Text},
    Stroke, View,
};

use super::{Layout, LayoutConfig, LayoutEdge, LayoutNode, RegionLayout};

impl LayoutEdge {
    fn into_path(&self, color: Color, stroke: Stroke) -> Path {
        //Builds a path from this edge. This allows us to optimize a little.
        // We basically iterate all "steps, but only emit an actual vertex, whenever the direction changes".

        let mut path = Path {
            points: Vec::new(),
            width: 1.0,
            color,
            stroke,
        };

        //push the first vertex
        path.points.push(self.path[0]);

        //TODO merge straight path segments
        for mov in &self.path[1..(self.path.len() - 1).max(1)] {
            path.points.push(*mov);
        }

        //push last vertex
        path.points.push(*self.path.last().unwrap());

        path
    }
}

impl<'a, N: LangNode + View + 'static, E: LangEdge + View + 'static> Layout<'a, N, E> {
    fn node_to_prim_tree(&self, node: &LayoutNode, config: &LayoutConfig) -> PrimTree {
        log::trace!("Transfer nodes to primitive tree");
        //build a prim tree for this node. This will contain the colored box of the node at top-level,
        //children will first be the label, followed by all sub-regions,
        // and finally all ports.

        //move the whole space to "our" location so we can always calculate "from zero"
        let mut pt_wrap = PrimTree {
            id: node.src.into(),
            prim: Prim::Offset(node.location),
            children: Vec::new(),
        };

        let node_color = self.src_graph.node(node.src).color();
        pt_wrap.children.push(PrimTree {
            id: node.src.into(),
            prim: Prim::Box(Rect {
                from: Point::ZERO,
                to: node.extent,
                color: node_color,
            }),
            children: Vec::new(),
        });

        //add the text depending on the number of sub regions. If we have no sub_regions,
        // we add it at the center, otherwise top-left

        if node.sub_regions.len() == 0 {
            let center = node.extent / 2.0;
            let node_name = self.src_graph.node(node.src).name().to_owned();
            let text_width = Text::width_for_string_size(&node_name, config.font_size);
            let text_at = center - Point::new(text_width / 2.0, config.font_size as f32 / 2.0);
            pt_wrap.children.push(PrimTree {
                id: node.src.into(),
                prim: Prim::Text(Text {
                    at: text_at,
                    string: node_name,
                    color: Color::from_rgba(0, 0, 0, 255),
                    width: text_width,
                    size: config.font_size as f32,
                }),
                children: Vec::with_capacity(0),
            });
        } else {
            //In this case, align our text to the top left, then offset our regions
            // by config.horizontal_node_padding and config.vertical_node_padding.
            let node_name = self.src_graph.node(node.src).name().to_owned();
            let text_width = Text::width_for_string_size(&node_name, config.font_size);
            pt_wrap.children.push(PrimTree {
                id: node.src.into(),
                prim: Prim::Text(Text {
                    at: Point::splat(1.0),
                    string: node_name,
                    color: Color::from_rgba(0, 0, 0, 255),
                    width: text_width,
                    size: config.font_size as f32,
                }),
                children: Vec::with_capacity(0),
            });

            let yoff = config.vertical_node_padding as f32 + config.font_size as f32;
            let mut xoff = config.horizontal_node_padding as f32;
            for reg in node.sub_regions.iter() {
                let subreg = self.reg_to_prim_tree(reg, config);
                pt_wrap.children.push(PrimTree {
                    id: reg.src.into(),
                    prim: Prim::Offset(Point::new(xoff, yoff)),
                    children: vec![subreg],
                });
                xoff += reg.extent.x + config.horizontal_node_padding as f32;
            }
        };

        //Now add the ports to the sub regions
        for port in node.inports.iter().chain(node.outports.iter()) {
            pt_wrap.children.push(PrimTree {
                id: port.1.clone(),
                prim: Prim::Box(Rect {
                    from: port.0,
                    to: port.0 + Point::new(config.port_width as f32, config.port_height as f32),
                    color: Color::from_rgba(0, 0, 0, 255),
                }),
                children: Vec::with_capacity(0),
            });
        }

        pt_wrap
    }

    fn reg_to_prim_tree(&self, reg: &RegionLayout, config: &LayoutConfig) -> PrimTree {
        //NOTE: our region is always a empty white box as the base layer
        let bg_prim = Prim::Box(Rect {
            from: Point::ZERO,
            to: reg.extent,
            color: Color::from_rgba(255, 255, 255, 0),
        });

        let mut tree_node = PrimTree {
            id: reg.src.into(),
            prim: bg_prim,
            children: Vec::with_capacity(reg.nodes.len()),
        };

        //Append all children
        for child in reg.nodes.values() {
            tree_node
                .children
                .push(self.node_to_prim_tree(child, config));
        }

        //add ports to tree
        let port_ext = Point::new(config.port_width as f32, config.port_height as f32);
        for port in reg.arg_ports.iter().chain(reg.res_ports.iter()) {
            tree_node.children.push(PrimTree {
                id: port.1.clone(),
                prim: Prim::Box(Rect {
                    from: port.0,
                    to: port.0 + port_ext,
                    color: Color::from_rgba(0, 0, 0, 255),
                }),
                children: Vec::with_capacity(0),
            })
        }

        //Now append all paths
        for edg in reg.edges.iter() {
            let src_edge = self.src_graph.edge(edg.src);
            let color = src_edge.ty.color();
            let stroke = src_edge.ty.stroke();

            tree_node.children.push(PrimTree {
                id: edg.src.into(),
                prim: Prim::Path(edg.into_path(color, stroke)),
                children: Vec::with_capacity(0),
            });
        }

        tree_node
    }

    pub fn to_prim_tree(&self, config: &LayoutConfig) -> PrimTree {
        self.reg_to_prim_tree(&self.region_tree, config)
    }
}
