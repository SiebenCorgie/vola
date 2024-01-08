use rvsdg::{
    nodes::LangNode,
    region::{Input, Output},
};

#[derive(Clone, Debug)]
pub struct LNode {
    inputs: Vec<Input>,
    outputs: Vec<Output>,
}

impl LNode {
    pub fn new() -> Self {
        LNode {
            inputs: Vec::with_capacity(0),
            outputs: Vec::with_capacity(0),
        }
    }

    pub fn with_inputs(mut self, n: usize) -> Self {
        self.inputs = vec![Input::default(); n];
        self
    }

    pub fn with_outputs(mut self, n: usize) -> Self {
        self.outputs = vec![Output::default(); n];
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
