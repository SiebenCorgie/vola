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
mod primitives;

use layout::{Layout, LayoutConfig};
pub use macroquad;
///The color of a node or edge.
pub use macroquad::color::Color;
use macroquad::prelude::{BLACK, RED};
use rvsdg::{
    common::VSEdge,
    edge::LangEdge,
    nodes::{LangNode, NodeType},
    Rvsdg,
};
use std::{fmt::Debug, path::Path};

pub trait View {
    fn name(&self) -> &str;
    fn color(&self) -> macroquad::color::Color;
    fn stroke(&self) -> Stroke {
        Stroke::Line
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
    fn name(&self) -> &str {
        match self {
            VSEdge::State { .. } => "StateEdge",
            VSEdge::Value { .. } => "ValueEdge",
        }
    }
    fn color(&self) -> macroquad::color::Color {
        match self {
            VSEdge::State { .. } => RED,
            VSEdge::Value { .. } => BLACK,
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
    fn color(&self) -> macroquad::color::Color {
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
    fn name(&self) -> &str {
        match &self.node_type {
            NodeType::Apply(_) => "Apply",
            NodeType::Delta(_) => "Delta",
            NodeType::Gamma(_) => "Gamma",
            NodeType::Lambda(_) => "Lambda",
            NodeType::Omega(_) => "Omega",
            NodeType::Phi(_) => "Phi",
            NodeType::Simple(n) => n.name(),
            NodeType::Theta(_) => "Theta",
        }
    }
}

///Saves the rvsdg graph as an SVG image at `svg_path`.
pub fn into_svg<N: View + LangNode + Debug + 'static, E: View + LangEdge + 'static>(
    rvsdg: &Rvsdg<N, E>,
    svg_path: impl AsRef<Path>,
) {
    into_svg_with_config(rvsdg, svg_path, &LayoutConfig::default())
}

///Saves the rvsdg graph as an SVG image at `svg_path`.
pub fn into_svg_with_config<N: View + LangNode + Debug + 'static, E: View + LangEdge + 'static>(
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

/*
///Uses [macroquad](macroquad) to display the graph in an interactive window.
pub async fn view<N: View + LangNode + 'static, E: View + LangEdge + 'static>(rvsdg: &Rvsdg<N, E>) {
    loop {
        clear_background(BLUE);

        set_default_camera();
        draw_text("WELCOME TO RVSDG", 10.0, 20.0, 30.0, BLACK);

        next_frame().await
    }
}
*/
