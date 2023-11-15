use rvsdg::{common::CommonRvsdg, nodes::LanguageNode, EdgeRef, region::Port};
use rvsdg_viewer::{macroquad::{main, prelude::BLUE}, view, View};
pub use rvsdg_viewer::macroquad;
///Builds the simple rvsdg structures presented in figure 2 of the RVSDG paper.

enum MyNodes {
    Load,
    Store,
    Lit(i32),
    Lt,
    Gt,
    Mul,
    Add,
}

struct LNode {
    node: MyNodes,
    inputs: Vec<Port>,
    outputs: Vec<Port>,
}

impl LanguageNode for LNode {
    fn inputs(&self) -> &[Port] {
        &self.inputs
    }
    fn outputs(&self) -> &[Port] {
        &self.outputs
    }
}

impl View for LNode{
    fn color(&self) -> macroquad::color::Color {
        BLUE
    }

    fn name(&self) -> &str {
        "LNODE"
    }
}



#[main("RVSDGSimple")]
async fn main() {
    let mut graph = CommonRvsdg::<LNode>::new();

    graph.builder();


    view(graph).await;
}
