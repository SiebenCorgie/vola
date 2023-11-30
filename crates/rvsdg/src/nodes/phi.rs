use std::slice;

use tinyvec::TinyVec;

use crate::{
    edge::{InputType, OutputType},
    region::{Argument, Input, Output, RegResult, Region},
};

use super::StructuralNode;

pub type RecursionNode = PhiNode;

/// ϕ-Nodes represent a environment for mutual recursion, i.e `fn f(){... return f();}`.
///
/// In essence this is similar to the idea of a [λ-Node](LambdaNode). However, it allows you to not only declare the function (what a LambdaNode does via its single output),
/// but also use that function in that deceleration. This is done by not only setting an output (again, like the LambdaNode), but also having an argument, that
/// is the deceleration defined by the output.
///
///
/// An added concept, next to the already known context_variables are recursion_variables. Their concept is similar to loop_variables. They are
/// the variables needed by the recursive [ApplyNode] to represent a variable that is taken, and returned at each recursion level.
/// Have a look at the [ϕ-Builder](crate::builder::PhiBuilder::add_recursion_variable) for an example.
///
///
/// By definition the PhiNode has only an internal [λ-Node](LambdaNode), where the first context variable is an import of itself. This models the recursion.
/// It follows all other context-variables (imported from outside the ϕ-Node), the recursion-variable (modelling recursive data flow), and finally all non recursive
/// arguments.
///
///
/// # Disclaimer
///
/// The PhiNode is currently not completely sound. You can set it up _the right way_, but it is currently way to easy to do it _the wrong way_.
/// > I still have to find a nicer way to model this API-wise. Currently unsolved is:
/// >  - Do we have to allow non recursion/context variables?
/// >  - Should we always implicitly model the internal semi-hidden λ-Node, or should we let the user have access to it?
#[derive(Debug, Clone)]
pub struct PhiNode {
    pub(crate) cv_count: usize,
    pub(crate) rv_count: usize,
    pub(crate) body: Region,
    pub(crate) outputs: TinyVec<[Output; 3]>,
    pub(crate) inputs: TinyVec<[Input; 3]>,
}

impl StructuralNode for PhiNode {
    fn regions(&self) -> &[Region] {
        slice::from_ref(&self.body)
    }
    fn regions_mut(&mut self) -> &mut [Region] {
        slice::from_mut(&mut self.body)
    }
    fn inport(&self, ty: &InputType) -> Option<&Input> {
        match ty {
            InputType::Input(u) => self.inputs.get(*u),
            InputType::Result(r) => self.body.results.get(*r + self.rv_count),
            InputType::ContextVariableInput(n) => self.cv_input(*n),
            InputType::RecursionVariableResult(r) => self.rv_result(*r),
            _ => None,
        }
    }
    fn inport_mut(&mut self, ty: &InputType) -> Option<&mut Input> {
        match ty {
            InputType::Input(u) => self.inputs.get_mut(*u),
            InputType::Result(r) => self.body.results.get_mut(*r + self.rv_count),
            InputType::ContextVariableInput(n) => self.cv_input_mut(*n),
            InputType::RecursionVariableResult(r) => self.rv_result_mut(*r),
            _ => None,
        }
    }
    fn outport(&self, ty: &OutputType) -> Option<&Output> {
        match ty {
            OutputType::Output(n) => self.outputs.get(*n),
            OutputType::Argument(n) => self.body.arguments.get(*n + self.cv_count + self.rv_count),
            OutputType::RecursionVariableOutput(n) => self.rv_output(*n),
            OutputType::RecursionVariableArgument(n) => self.rv_argument(*n),
            OutputType::ContextVariableArgument(n) => self.cv_argument(*n),
            _ => None,
        }
    }
    fn outport_mut(&mut self, ty: &OutputType) -> Option<&mut Output> {
        match ty {
            OutputType::Output(n) => self.outputs.get_mut(*n),
            OutputType::Argument(n) => self
                .body
                .arguments
                .get_mut(*n + self.cv_count + self.rv_count),
            OutputType::RecursionVariableOutput(n) => self.rv_output_mut(*n),
            OutputType::RecursionVariableArgument(n) => self.rv_argument_mut(*n),
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
        &self.outputs
    }
    fn outputs_mut(&mut self) -> &mut [Output] {
        &mut self.outputs
    }
    fn context_variable_count(&self) -> usize {
        self.cv_count
    }
    fn recursion_variable_count(&self) -> usize {
        self.rv_count
    }
}

impl PhiNode {
    pub fn new() -> Self {
        PhiNode {
            cv_count: 0,
            rv_count: 0,
            inputs: TinyVec::default(),
            body: Region::new(),
            outputs: TinyVec::default(),
        }
    }

    pub fn add_context_variable(&mut self) -> usize {
        let idx = self.cv_count;
        self.cv_count += 1;
        self.inputs.insert(idx, Input::default());
        self.body.arguments.insert(idx, Argument::default());

        idx
    }

    ///Adds new recursion port. Returns the port index for that recursion variable.
    pub fn add_recursion_variable(&mut self) -> usize {
        let arg_idx = self.cv_count + self.rv_count;
        let res_idx = self.rv_count;
        self.rv_count += 1;
        self.body.arguments.insert(arg_idx, Argument::default());
        self.body.results.insert(res_idx, RegResult::default());
        self.outputs.insert(res_idx, Output::default());

        res_idx
    }

    pub fn add_input(&mut self) -> usize {
        self.inputs.push(Input::default());
        let pushed_to = self.inputs.len();
        self.body.arguments.push(Argument::default());
        assert!(
            self.body.arguments.len() == pushed_to,
            "Detected invalid LambdaNode state, input and argument-count don't match"
        );

        pushed_to - self.cv_count - 1
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

    ///Returns the `n`-th recursion variable argument for the phi node
    pub fn rv_argument(&self, n: usize) -> Option<&Argument> {
        if n >= self.rv_count {
            return None;
        }

        self.body.arguments.get(self.cv_count + n)
    }
    ///Returns the `n`-th recursion variable argument for the phi node
    pub fn rv_argument_mut(&mut self, n: usize) -> Option<&mut Argument> {
        if n >= self.rv_count {
            return None;
        }

        self.body.arguments.get_mut(self.cv_count + n)
    }

    ///Returns the `n`-th recursion variable result for the phi node
    pub fn rv_result(&self, n: usize) -> Option<&RegResult> {
        if n >= self.rv_count {
            return None;
        }

        self.body.results.get(n)
    }
    ///Returns the `n`-th recursion variable result for the phi node
    pub fn rv_result_mut(&mut self, n: usize) -> Option<&mut RegResult> {
        if n >= self.rv_count {
            return None;
        }

        self.body.results.get_mut(n)
    }

    ///Returns the `n`-th recursion variable output for the phi node
    pub fn rv_output(&self, n: usize) -> Option<&Output> {
        if n >= self.rv_count {
            return None;
        }

        self.outputs.get(n)
    }
    ///Returns the `n`-th recursion variable output for the phi node
    pub fn rv_output_mut(&mut self, n: usize) -> Option<&mut Output> {
        if n >= self.rv_count {
            return None;
        }

        self.outputs.get_mut(n)
    }
}
