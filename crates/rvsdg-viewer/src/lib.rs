//! A simple renderer for RVSDG graphs. Exposes the [View] trait, which must be implemented by nodes and edges in order to
//! render the graph in an interactive way.
//!
//! There are only two _must have_ properties for each node/edge. A name, and a color. Everything else is optional.
//!
//!
//! To view an RVSDG annotate your `main()` with `#[macroquad::main("3D")]`
//! and call `view()`.
//!
//! ```rust
//! #[macroquad::main("MyRVSDG of something")]
//! async fn main() {
//!     //... build your rvsdg
//!     view(rvsdg);
//! }
//! ```
mod primitives;
mod printer;

use std::{fmt::Debug, path::Path};

pub use macroquad;
///The color of a node or edge.
pub use macroquad::color::Color;
use macroquad::prelude::{BLACK, RED};
use printer::Printer;
use rvsdg::{
    common::VSEdge,
    edge::LangEdge,
    nodes::{LangNode, Node},
    Rvsdg,
};

pub trait View {
    fn name(&self) -> &str;
    fn color(&self) -> macroquad::color::Color;
    fn stroke(&self) -> Stroke {
        Stroke::Line
    }
}

#[derive(Debug, Clone)]
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
        match self {
            Node::Apply(_) => Color::from_rgba(255, 255, 128, 255),
            Node::Delta(_) => Color::from_rgba(128, 128, 0, 100),
            Node::Gamma(_) => Color::from_rgba(128, 255, 128, 255),
            Node::Lambda(_) => Color::from_rgba(128, 180, 255, 255),
            Node::Omega(_) => Color::from_rgba(172, 157, 147, 255),
            Node::Phi(_) => Color::from_rgba(255, 179, 128, 255),
            Node::Simple(n) => n.color(),
            Node::Theta(_) => Color::from_rgba(255, 128, 128, 255),
        }
    }
    fn name(&self) -> &str {
        match self {
            Node::Apply(_) => "Apply",
            Node::Delta(_) => "Delta",
            Node::Gamma(_) => "Gamma",
            Node::Lambda(_) => "Lambda",
            Node::Omega(_) => "Omega",
            Node::Phi(_) => "Phi",
            Node::Simple(n) => n.name(),
            Node::Theta(_) => "Theta",
        }
    }
}

///Saves the rvsdg graph as an SVG image at `svg_path`.
pub fn into_svg<N: View + LangNode + Debug + 'static, E: View + LangEdge + 'static>(
    rvsdg: &Rvsdg<N, E>,
    svg_path: impl AsRef<Path>,
) {
    let mut printer = Printer::new(rvsdg);

    printer.layout(rvsdg);
    printer.root.flip_y();

    if svg_path.as_ref().exists() {
        std::fs::remove_file(svg_path.as_ref()).unwrap()
    }

    let buffer = printer.emit_svg();

    std::fs::write(svg_path.as_ref(), buffer).unwrap();
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
