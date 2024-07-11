use rvsdg::{
    common::VSEdge,
    nodes::LangNode,
    region::{Input, Output},
    util::cne::CneTypeEq,
    Rvsdg,
};

use rvsdg_viewer::View;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LNode {
    pub name: String,
    pub inputs: Vec<Input>,
    pub outputs: Vec<Output>,
}

impl CneTypeEq for LNode {
    fn type_equal(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl LNode {
    pub fn new() -> Self {
        LNode {
            name: String::with_capacity(0),
            inputs: Vec::with_capacity(0),
            outputs: Vec::with_capacity(0),
        }
    }

    #[allow(dead_code)]
    pub fn with_inputs(mut self, n: usize) -> Self {
        self.inputs = vec![Input::default(); n];
        self
    }

    pub fn with_outputs(mut self, n: usize) -> Self {
        self.outputs = vec![Output::default(); n];
        self
    }
    #[allow(unused)]
    pub fn with_name(mut self, name: &str) -> Self {
        self.name = name.to_owned();
        self
    }
}

impl LangNode for LNode {
    fn inputs(&self) -> &[Input] {
        &self.inputs
    }
    fn outputs(&self) -> &[Output] {
        &self.outputs
    }
    fn inputs_mut(&mut self) -> &mut [Input] {
        &mut self.inputs
    }
    fn outputs_mut(&mut self) -> &mut [Output] {
        &mut self.outputs
    }
}

impl View for LNode {
    fn name(&self) -> String {
        if self.name.is_empty() {
            "Node".to_owned()
        } else {
            self.name.clone()
        }
    }
    fn color(&self) -> rvsdg_viewer::Color {
        rvsdg_viewer::Color::from_rgba(255, 240, 50, 255)
    }
}

#[allow(unused)]
pub fn dump_graph_to_svg(graph: &Rvsdg<LNode, VSEdge>, name: &str) {
    let config = rvsdg_viewer::layout::LayoutConfig {
        ignore_dead_node: false,
        ..Default::default()
    };
    rvsdg_viewer::into_svg_with_config(graph, name, &config)
}
