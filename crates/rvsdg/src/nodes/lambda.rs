/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use smallvec::SmallVec;

use crate::{
    edge::{InputType, OutputType},
    region::{Argument, Input, Output, RegResult, Region},
    SmallColl,
};
use std::slice;

use super::StructuralNode;

///Related to the [λ-Node](LambdaNode). Represents a call of some function. The first port is defined as the `caller`, which must be connected to a
/// [LambdaNode].
#[derive(Debug, Clone)]
pub struct ApplyNode {
    ///Function being called, must be a edge to a lambdaNode
    pub(crate) inputs: SmallColl<Input>,
    pub(crate) outputs: SmallColl<Output>,
}

impl ApplyNode {
    pub fn new() -> Self {
        let mut inputs = SmallColl::default();
        //The function input to apply
        inputs.insert(0, Input::default());
        ApplyNode {
            inputs,
            outputs: SmallColl::default(),
        }
    }

    ///Creates a call that has the signature needed for the given lambda node
    pub fn new_for_lambda(node: &LambdaNode) -> Self {
        let node_body = &node.body;
        //NOTE: We need a call-arg and the amount of arguments for the apply-node's
        //      Inputs.
        let inputs = (0..(node.argument_count() + 1))
            .map(|_| Input::default())
            .collect();
        ApplyNode {
            inputs,
            outputs: node_body
                .results
                .iter()
                .map(|_p| Output::default())
                .collect(),
        }
    }

    pub fn add_input(&mut self) -> usize {
        self.inputs.push(Input::default());
        self.inputs.len() - 1
    }

    pub fn add_output(&mut self) -> usize {
        self.outputs.push(Output::default());
        self.outputs.len() - 1
    }

    ///Returns the input for the callable (λ-Node or ϕ-Node) declaration this apply node will execute
    pub fn get_callabel_decl(&self) -> &Input {
        &self.inputs[0]
    }

    ///Returns the input for the callabel (λ-Node or ϕ-Node) declaration this apply node will execute
    pub fn callable_decl_mut(&mut self) -> &mut Input {
        &mut self.inputs[0]
    }

    ///Returns the input port for the `n`-th argument to the called function.
    pub fn argument_input(&self, n: usize) -> Option<&Input> {
        self.inputs.get(n + 1)
    }

    ///Returns the input port for the `n`-th argument to the called function.
    pub fn argument_input_mut(&mut self, n: usize) -> Option<&mut Input> {
        self.inputs.get_mut(n + 1)
    }

    ///Returns the port for the `n`-th return value the function emits.
    pub fn return_value(&self, n: usize) -> Option<&Output> {
        self.outputs.get(n)
    }

    ///Returns the port for the `n`-th return value the function emits.
    pub fn return_value_mut(&mut self, n: usize) -> Option<&mut Output> {
        self.outputs.get_mut(n)
    }

    ///Returns how many callargs the apply node expects. This is by definition `input_count - 1`. As
    /// the first input is reserved for the callable that is called.
    pub fn get_call_arg_count(&self) -> usize {
        self.inputs.len() - 1
    }
}

pub type FunctionNode = LambdaNode;
///λ-Node represents a function, characterised by an internal [Region](crate::region::Region) representing the function's body.
/// The node has `n` inputs, and a single output. The single output represents the function **not** the function's output.
///
/// The output is mapped at call time (represented by the [ApplyNode]) to the output of the calling node.
///
/// A function is called via an [ApplyNode], where the function being called (callee), is an argument to the [ApplyNode] (referred to as Caller).
#[derive(Debug, Clone)]
pub struct LambdaNode {
    pub(crate) cv_count: usize,
    pub(crate) inputs: SmallColl<Input>,
    pub(crate) output: Output,
    pub(crate) body: Region,
}

impl StructuralNode for LambdaNode {
    fn regions(&self) -> &[Region] {
        slice::from_ref(&self.body)
    }
    fn regions_mut(&mut self) -> &mut [Region] {
        slice::from_mut(&mut self.body)
    }
    fn inport(&self, ty: &InputType) -> Option<&Input> {
        match ty {
            InputType::Input(n) => self.inputs.get(*n),
            InputType::Result(n) => self.body.results.get(*n),
            InputType::ContextVariableInput(n) => self.cv_input(*n),
            _ => None,
        }
    }
    fn inport_mut(&mut self, ty: &InputType) -> Option<&mut Input> {
        match ty {
            InputType::Input(n) => self.inputs.get_mut(*n),
            InputType::Result(n) => self.body.results.get_mut(*n),
            InputType::ContextVariableInput(n) => self.cv_input_mut(*n),
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
            OutputType::Argument(n) => self.body.arguments.get(*n + self.cv_count),
            OutputType::LambdaDeclaration => Some(&self.output),
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
            OutputType::Argument(n) => self.body.arguments.get_mut(*n + self.cv_count),
            OutputType::LambdaDeclaration => Some(&mut self.output),
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
    fn context_variable_count(&self) -> usize {
        self.cv_count
    }

    fn outport_types(&self) -> SmallVec<[OutputType; 3]> {
        let mut outs = SmallVec::new();
        outs.push(OutputType::LambdaDeclaration);
        outs
    }
    fn input_types(&self) -> SmallVec<[InputType; 3]> {
        let mut inputs = SmallVec::default();
        //now append all the args
        for i in 0..self.inputs.len() {
            if i < self.cv_count {
                inputs.push(InputType::ContextVariableInput(i));
            } else {
                panic!("λ-Node can't have other inputs than CV-Inputs.")
            }
        }

        inputs
    }
    fn argument_types(&self, region_index: usize) -> SmallVec<[OutputType; 3]> {
        if region_index > 0 {
            return SmallVec::new();
        }

        let mut args = SmallVec::new();
        for i in 0..self.body.arguments.len() {
            if i < self.cv_count {
                args.push(OutputType::ContextVariableArgument(i));
            } else {
                args.push(OutputType::Argument(i - self.cv_count));
            }
        }
        args
    }
    fn result_types(&self, region_index: usize) -> SmallVec<[InputType; 3]> {
        //Does not exist!
        if region_index > 0 {
            return SmallVec::new();
        }

        let mut res = SmallVec::new();
        for i in 0..self.body.results.len() {
            res.push(InputType::Result(i));
        }

        res
    }
}

impl LambdaNode {
    pub fn new() -> Self {
        LambdaNode {
            cv_count: 0,
            inputs: SmallColl::default(),
            output: Output::default(),
            body: Region::new(),
        }
    }

    pub fn add_context_variable(&mut self) -> usize {
        let idx = self.cv_count;
        self.cv_count += 1;
        self.inputs.insert(idx, Input::default());
        self.body.arguments.insert(idx, Argument::default());

        idx
    }

    ///Adds an argument to the lambda's body. Returns the argument's index.
    pub fn add_argument(&mut self) -> usize {
        let cv_count = self.cv_count;
        self.body.arguments.push(Argument::default());
        let this_args_idx = self.body.arguments.len() - 1;

        this_args_idx - cv_count
    }

    ///Adds a result to the function. Note that this does NOT mean the output of this lambda function, but adding
    /// a result to the later evaluated procedure of the body.
    pub fn add_result(&mut self) -> usize {
        self.body.results.push(RegResult::default());
        self.body.results.len() - 1
    }

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

    ///Returns the port to this function's `n`-th argument
    pub fn argument(&self, n: usize) -> Option<&Argument> {
        self.body.arguments.get(n + self.cv_count)
    }

    ///Returns the port to this function's `n`-th argument
    pub fn argument_mut(&mut self, n: usize) -> Option<&mut Argument> {
        self.body.arguments.get_mut(n + self.cv_count)
    }

    ///Returns the port to this function's `n`-th result
    pub fn result(&self, n: usize) -> Option<&RegResult> {
        self.body.results.get(n)
    }

    ///Returns the port to this function's `n`-th argument
    pub fn result_mut(&mut self, n: usize) -> Option<&mut RegResult> {
        self.body.results.get_mut(n)
    }

    ///The amount of arguments, that are no context-variables to this λ-Node's region.
    pub fn argument_count(&self) -> usize {
        self.body.arguments.len() - self.cv_count
    }

    pub fn result_count(&self) -> usize {
        self.body.results.len()
    }
}

#[cfg(test)]
mod phitests {
    use smallvec::{smallvec, SmallVec};

    use crate::{
        edge::{InputType, OutputType},
        nodes::{LambdaNode, StructuralNode},
    };

    //TODO write those tests for the others as well!
    #[test]
    fn sig_inputs_test() {
        let mut lmd = LambdaNode::new();

        assert!(lmd.add_context_variable() == 0);
        assert!(lmd.add_argument() == 0);
        assert!(lmd.add_context_variable() == 1);
        assert!(lmd.add_argument() == 1);
        assert!(lmd.add_result() == 0);
        assert!(lmd.add_result() == 1);

        let insig = lmd.input_types();
        let expected_in_sig: SmallVec<[InputType; 3]> = smallvec![
            InputType::ContextVariableInput(0),
            InputType::ContextVariableInput(1),
        ];
        assert!(
            insig == expected_in_sig,
            "{:?} != {:?}",
            insig,
            expected_in_sig
        );

        let argsig = lmd.argument_types(0);
        let expected_arg_sig: SmallVec<[OutputType; 3]> = smallvec![
            OutputType::ContextVariableArgument(0),
            OutputType::ContextVariableArgument(1),
            OutputType::Argument(0),
            OutputType::Argument(1),
        ];
        assert!(argsig == expected_arg_sig);

        let ressig = lmd.result_types(0);
        let expected_ressig: SmallVec<[InputType; 3]> =
            smallvec![InputType::Result(0), InputType::Result(1),];
        assert!(ressig == expected_ressig);

        let outsig = lmd.outport_types();
        let expected_outsig: SmallVec<[OutputType; 3]> = smallvec![OutputType::LambdaDeclaration,];
        assert!(outsig == expected_outsig);
    }
}
