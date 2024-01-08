use std::slice;

use crate::{
    edge::{InputType, OutputType},
    region::{Argument, Input, Output, RegResult, Region},
    SmallColl,
};

use super::StructuralNode;

pub type LoopNode = ThetaNode;
/// θ-nodes are _tail-controlled_ loops, i.e. do-while loops.
///
/// As per the paper, head controlled loops can be constructed by employing a [γ-node](GammaNode) (aka. IF-ELSE) to do the first
/// decision. Therefore only one Loop node exists, which makes writing transformation passes easier later on.
///
/// The `loop_body`'s first result is the loops predicate, all following outputs must type-match the input of the body.
///
/// At runtime, depending on the _predicate_ the non-predicate outputs of the block are either routed to the θ-node
/// output (on break / loop-end), or routed back as region arguments
#[derive(Debug, Clone)]
pub struct ThetaNode {
    pub(crate) lv_count: usize,
    pub(crate) loop_body: Region,
    pub(crate) inputs: SmallColl<Input>,
    pub(crate) outputs: SmallColl<Output>,
}

impl StructuralNode for ThetaNode {
    fn regions(&self) -> &[Region] {
        slice::from_ref(&self.loop_body)
    }
    fn regions_mut(&mut self) -> &mut [Region] {
        slice::from_mut(&mut self.loop_body)
    }
    fn inport(&self, ty: &InputType) -> Option<&Input> {
        match ty {
            InputType::Input(u) => self.inputs.get(*u),
            InputType::Result(n) => self.loop_body.results.get(*n),
            InputType::LoopVariableResult(n) => self.loop_body.results.get(n + 1),
            InputType::ThetaPredicate => Some(self.loop_predicate()),
            _ => None,
        }
    }
    fn inport_mut(&mut self, ty: &InputType) -> Option<&mut Input> {
        match ty {
            InputType::Input(u) => self.inputs.get_mut(*u),
            InputType::Result(n) => self.loop_body.results.get_mut(*n),
            InputType::LoopVariableResult(n) => self.loop_body.results.get_mut(n + 1),
            InputType::ThetaPredicate => Some(self.loop_predicate_mut()),
            _ => None,
        }
    }

    fn outport(&self, ty: &OutputType) -> Option<&Output> {
        match ty {
            OutputType::Output(n) => self.outputs.get(*n),
            OutputType::Argument(n) => self.loop_body.arguments.get(*n),
            _ => None,
        }
    }
    fn outport_mut(&mut self, ty: &OutputType) -> Option<&mut Output> {
        match ty {
            OutputType::Output(n) => self.outputs.get_mut(*n),
            OutputType::Argument(n) => self.loop_body.arguments.get_mut(*n),
            _ => None,
        }
    }

    fn inputs(&self) -> &[Input] {
        &self.inputs
    }
    fn inputs_mut(&mut self) -> &mut [Input] {
        &mut self.inputs
    }
    fn outputs(&self) -> &[Output] {
        &self.outputs
    }
    fn outputs_mut(&mut self) -> &mut [Output] {
        &mut self.outputs
    }
}

impl ThetaNode {
    pub fn new() -> Self {
        let mut loop_body = Region::new();
        //add the loop criteria
        loop_body.results.insert(0, RegResult::default());
        ThetaNode {
            lv_count: 0,
            loop_body,
            inputs: SmallColl::default(),
            outputs: SmallColl::default(),
        }
    }

    pub fn add_loop_variable(&mut self) -> usize {
        let lvidx = self.lv_count;
        self.lv_count += 1;
        self.inputs.insert(lvidx, Input::default());
        self.outputs.insert(lvidx, Output::default());
        self.loop_body.arguments.insert(lvidx, Argument::default());
        //offset by 1, since the first is the criteria.
        self.loop_body
            .results
            .insert(lvidx + 1, RegResult::default());

        lvidx
    }

    ///Returns the loop predicate port. This is always a result, since ThetaNodes are tail-controlled.
    pub fn loop_predicate(&self) -> &RegResult {
        &self.loop_body.results[0]
    }

    ///Returns the loop predicate port. This is always a result, since ThetaNodes are tail-controlled.
    pub fn loop_predicate_mut(&mut self) -> &mut RegResult {
        &mut self.loop_body.results[0]
    }

    ///Returns the `n`-th loop variable input to this node.
    pub fn lv_input(&self, n: usize) -> Option<&Input> {
        self.inputs.get(n)
    }
    ///Returns the `n`-th loop variable input to this node.
    pub fn lv_input_mut(&mut self, n: usize) -> Option<&mut Input> {
        self.inputs.get_mut(n)
    }

    ///Returns the `n`-th loop variable argument to this loop's body.
    pub fn lv_argument(&self, n: usize) -> Option<&Argument> {
        self.loop_body.arguments.get(n)
    }
    ///Returns the `n`-th loop variable argument to this loop's body.
    pub fn lv_argument_mut(&mut self, n: usize) -> Option<&mut Argument> {
        self.loop_body.arguments.get_mut(n)
    }

    ///Returns the `n`-th loop variable result to this loop's body.
    pub fn lv_result(&self, n: usize) -> Option<&RegResult> {
        self.loop_body.results.get(n + 1)
    }
    ///Returns the `n`-th loop variable result to this loop's body.
    pub fn lv_result_mut(&mut self, n: usize) -> Option<&mut RegResult> {
        self.loop_body.results.get_mut(n + 1)
    }

    ///Returns the `n`-th loop variable output to this node.
    pub fn lv_output(&self, n: usize) -> Option<&Output> {
        self.outputs.get(n)
    }
    ///Returns the `n`-th loop variable output to this node.
    pub fn lv_output_mut(&mut self, n: usize) -> Option<&mut Output> {
        self.outputs.get_mut(n)
    }
}
