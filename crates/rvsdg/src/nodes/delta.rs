use std::slice;

use smallvec::{smallvec, SmallVec};

use crate::{
    edge::{InputType, OutputType},
    region::{Argument, Input, Output, RegResult, Region},
    SmallColl,
};

use super::StructuralNode;

/// δ-Nodes represents a global variable. It can have a set of inputs needed to define the variable. Similarly to [λ-Node](LambdaNode)s
/// context-variables can import inner, dependent state, like λ-Nodes or other arguments.
///
/// A δ-node must always provide a single output.
#[derive(Debug, Clone)]
pub struct DeltaNode {
    pub(crate) cv_count: usize,
    ///All inputs of a delta node are context variables.
    pub(crate) inputs: SmallColl<Input>,
    pub(crate) body: Region,
    pub(crate) output: Output,
}

impl StructuralNode for DeltaNode {
    fn regions(&self) -> &[Region] {
        slice::from_ref(&self.body)
    }
    fn regions_mut(&mut self) -> &mut [Region] {
        slice::from_mut(&mut self.body)
    }
    fn inport(&self, ty: &InputType) -> Option<&Input> {
        match ty {
            InputType::Input(i) => self.inputs.get(*i),
            InputType::Result(n) => self.body.results.get(*n),
            InputType::ContextVariableInput(d) => self.cv_input(*d),
            _ => None,
        }
    }
    fn inport_mut(&mut self, ty: &InputType) -> Option<&mut Input> {
        match ty {
            InputType::Input(i) => self.inputs.get_mut(*i),
            InputType::Result(n) => self.body.results.get_mut(*n),
            InputType::ContextVariableInput(d) => self.cv_input_mut(*d),
            _ => None,
        }
    }
    fn outport(&self, ty: &OutputType) -> Option<&Output> {
        match ty {
            OutputType::Output(n) => {
                if *n == 0 {
                    Some(&self.output)
                } else {
                    None
                }
            }
            OutputType::DeltaDeclaration => Some(&self.output),
            OutputType::ContextVariableArgument(n) => self.cv_argument(*n),
            _ => None,
        }
    }
    fn outport_mut(&mut self, ty: &OutputType) -> Option<&mut Output> {
        match ty {
            OutputType::Output(n) => {
                if *n == 0 {
                    Some(&mut self.output)
                } else {
                    None
                }
            }
            OutputType::DeltaDeclaration => Some(&mut self.output),
            OutputType::ContextVariableArgument(n) => self.cv_argument_mut(*n),
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
        slice::from_ref(&self.output)
    }
    fn outputs_mut(&mut self) -> &mut [Output] {
        slice::from_mut(&mut self.output)
    }

    fn output_types(&self) -> SmallVec<[OutputType; 3]> {
        smallvec![OutputType::DeltaDeclaration]
    }
    fn input_types(&self) -> SmallVec<[InputType; 3]> {
        let mut inputs = SmallVec::default();

        for i in 0..self.inputs.len() {
            if i < self.cv_count {
                inputs.push(InputType::ContextVariableInput(i));
            } else {
                panic!("There shouldn't be anything besides cvargs");
            }
        }

        inputs
    }
    fn argument_types(&self, region_index: usize) -> SmallVec<[OutputType; 3]> {
        //Does not exist!
        if region_index != 0 {
            return SmallVec::new();
        }

        let mut args = SmallVec::new();
        for i in 0..self.body.arguments.len() {
            if i < self.cv_count {
                args.push(OutputType::ContextVariableArgument(i))
            } else {
                args.push(OutputType::Argument(i - self.cv_count))
            }
        }
        args
    }
    fn result_types(&self, region_index: usize) -> SmallVec<[InputType; 3]> {
        //Does not exist!
        if region_index != 0 {
            return SmallVec::new();
        }

        let mut res = SmallVec::new();
        for i in 0..self.body.results.len() {
            res.push(InputType::Result(i));
        }

        res
    }
}

impl DeltaNode {
    pub fn new() -> Self {
        let mut region = Region::new();
        //Add the default region output for the default output of the node
        region.results.push(RegResult::default());
        DeltaNode {
            cv_count: 0,
            inputs: SmallColl::default(),
            body: region,
            output: Output::default(),
        }
    }
    pub fn add_context_variable(&mut self) -> usize {
        let idx = self.cv_count;
        self.cv_count += 1;
        self.inputs.insert(idx, Input::default());
        self.body.arguments.insert(idx, Argument::default());

        idx
    }

    ///Reuturns the output port representing this global value.
    pub fn decleration(&self) -> &Output {
        &self.output
    }

    pub fn decleration_mut(&mut self) -> &mut Output {
        &mut self.output
    }

    pub fn cv_input(&self, n: usize) -> Option<&Input> {
        if n >= self.cv_count {
            return None;
        }
        self.inputs.get(n)
    }

    pub fn cv_input_mut(&mut self, n: usize) -> Option<&mut Input> {
        if n >= self.cv_count {
            return None;
        }
        self.inputs.get_mut(n)
    }

    pub fn cv_argument(&self, n: usize) -> Option<&Argument> {
        if n >= self.cv_count {
            return None;
        }
        self.body.arguments.get(n)
    }

    pub fn cv_argument_mut(&mut self, n: usize) -> Option<&mut Argument> {
        if n >= self.cv_count {
            return None;
        }
        self.body.arguments.get_mut(n)
    }
}

pub type GlobalVariable = DeltaNode;
