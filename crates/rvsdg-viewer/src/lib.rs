//! A simple renderer for RVSDG graphs. Exposes the [View] trait, which must be implemented by nodes and edges in order to
//! render the graph in an interactive way.
//!
//! There are only two _must have_ properties for each node/edge. A name, and a color. Everything else is optional.
//!
//!
//! To view an RVSDG annotate your `main()` with `#[macroquad::main("3D")]`
//! and call `view()`.
//!
//! ```rust ignore
//! #[macroquad::main("MyRVSDG of something")]
//! async fn main() {
//!     //... build your rvsdg
//!     view(rvsdg);
//! }
//! ```
pub mod layout;
pub mod primitives;

use layout::{Layout, LayoutConfig};
use primitives::PrimTree;
///The color of a node or edge.
pub use primitives::{Color, Point};
use rvsdg::{
    common::VSEdge,
    edge::LangEdge,
    nodes::{LangNode, NodeType},
    Rvsdg,
};
use serde::{Deserialize, Serialize};
use std::{fmt::Debug, path::Path};

#[cfg(feature = "viewer")]
mod viewer;
#[cfg(feature = "viewer")]
pub use viewer::{GraphState, GraphStateBuilder, ViewerState};

pub trait View {
    fn name(&self) -> String;
    fn color(&self) -> Color;
    fn stroke(&self) -> Stroke {
        Stroke::Line
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Stroke {
    Line,
    Dashs,
    Dots,
}

impl Stroke {
    pub fn into_svg(&self) -> String {
        match self {
            Stroke::Line => String::with_capacity(0),
            Stroke::Dots => "stroke-dasharray=\"5,5\"".to_owned(),
            Stroke::Dashs => "stroke-dasharray=\"10,10\"".to_owned(),
        }
    }
}

impl View for rvsdg::common::VSEdge {
    fn name(&self) -> String {
        match self {
            VSEdge::State { .. } => "StateEdge".to_owned(),
            VSEdge::Value { .. } => "ValueEdge".to_owned(),
        }
    }
    fn color(&self) -> Color {
        match self {
            VSEdge::State { .. } => Color::RED,
            VSEdge::Value { .. } => Color::BLACK,
        }
    }
    fn stroke(&self) -> Stroke {
        match self {
            VSEdge::State { .. } => Stroke::Dots,
            VSEdge::Value { .. } => Stroke::Line,
        }
    }
}

impl<N: View + LangNode + 'static> View for rvsdg::nodes::Node<N> {
    fn color(&self) -> Color {
        match &self.node_type {
            NodeType::Apply(_) => Color::from_rgba(255, 255, 128, 255),
            NodeType::Delta(_) => Color::from_rgba(128, 128, 0, 100),
            NodeType::Gamma(_) => Color::from_rgba(128, 255, 128, 255),
            NodeType::Lambda(_) => Color::from_rgba(128, 180, 255, 255),
            NodeType::Omega(_) => Color::from_rgba(172, 157, 147, 255),
            NodeType::Phi(_) => Color::from_rgba(255, 179, 128, 255),
            NodeType::Simple(n) => n.color(),
            NodeType::Theta(_) => Color::from_rgba(255, 128, 128, 255),
        }
    }
    fn name(&self) -> String {
        match &self.node_type {
            NodeType::Apply(_) => "Apply".to_owned(),
            NodeType::Delta(_) => "Delta".to_owned(),
            NodeType::Gamma(_) => "Gamma".to_owned(),
            NodeType::Lambda(_) => "Lambda".to_owned(),
            NodeType::Omega(_) => "Omega".to_owned(),
            NodeType::Phi(_) => "Phi".to_owned(),
            NodeType::Simple(n) => n.name().to_owned(),
            NodeType::Theta(_) => "Theta".to_owned(),
        }
    }
}

///Saves the rvsdg graph as an SVG image at `svg_path`.
pub fn into_svg<N: View + LangNode + 'static, E: View + LangEdge + 'static>(
    rvsdg: &Rvsdg<N, E>,
    svg_path: impl AsRef<Path>,
) {
    into_svg_with_config(rvsdg, svg_path, &LayoutConfig::default())
}

///Saves the rvsdg graph as an SVG image at `svg_path`.
pub fn into_svg_with_config<N: View + LangNode + 'static, E: View + LangEdge + 'static>(
    rvsdg: &Rvsdg<N, E>,
    svg_path: impl AsRef<Path>,
    config: &LayoutConfig,
) {
    println!("Building: {:?}", svg_path.as_ref());
    let layout = Layout::for_rvsdg(rvsdg, config);
    let prims = layout.into_primitive_tree();
    let svg = prims.to_svg(layout.region_tree.get_extent().y);

    std::fs::write(svg_path.as_ref(), svg).unwrap();
}

pub fn into_primitive_tree<N: View + LangNode + 'static, E: View + LangEdge + 'static>(
    rvsdg: &Rvsdg<N, E>,
    config: &LayoutConfig,
) -> PrimTree {
    let layout = Layout::for_rvsdg(rvsdg, config);
    layout.into_primitive_tree()
}
