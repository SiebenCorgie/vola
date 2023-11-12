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

///The color of a node or edge.
pub use macroquad::color::Color;
pub use macroquad;
use macroquad::{
    prelude::{BLACK, BLUE, set_default_camera, RED, GREEN, YELLOW, ORANGE},
    text::draw_text,
    window::{clear_background, next_frame},
};
use rvsdg::{nodes::{LanguageNode, Node}, Rvsdg, common::VSEdge};

pub trait View {
    fn name(&self) -> &str;
    fn color(&self) -> macroquad::color::Color;
}

impl View for rvsdg::common::VSEdge{
    fn name(&self) -> &str {
        match self{
            VSEdge::State{ .. } => "StateEdge",
            VSEdge::Value { .. } => "ValueEdge"
        }
    }
    fn color(&self) -> macroquad::color::Color {
        match self {
            VSEdge::State { .. } => RED,
            VSEdge::Value { .. } => BLACK
        }
    }
}

impl<N: View + LanguageNode + 'static> View for rvsdg::nodes::Node<N>{
    fn color(&self) -> macroquad::color::Color {
        match self{
            Node::Apply(_) => YELLOW,
            Node::Delta(_) => Color::from_rgba(20, 30, 20, 100),
            Node::Gamma(_) => GREEN,
            Node::Lambda(_) => BLUE,
            Node::Omega(_) => Color::from_rgba(50, 50, 50, 100),
            Node::Phi(_) => ORANGE,
            Node::Simple(n) => n.color(),
            Node::Theta(_) => RED
        }
    }
    fn name(&self) -> &str {
        match self{
            Node::Apply(_) => "Apply",
            Node::Delta(_) => "Delta",
            Node::Gamma(_) => "Gamma",
            Node::Lambda(_) => "Lambda",
            Node::Omega(_) => "Omega",
            Node::Phi(_) => "Phi",
            Node::Simple(n) => n.name(),
            Node::Theta(_) => "Theta"
        }
    }
}

pub async fn view<N: View + LanguageNode + 'static, E: View + 'static>(rvsdg: Rvsdg<N, E>) {
    loop {
        clear_background(BLUE);

        set_default_camera();
        draw_text("WELCOME TO RVSDG", 10.0, 20.0, 30.0, BLACK);

        next_frame().await
    }
}
