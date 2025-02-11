/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::slice;

use smallvec::SmallVec;

use crate::{
    edge::{InputType, OutputType},
    region::{Argument, Input, Output, RegResult, Region},
    SmallColl,
};

use super::StructuralNode;

pub type RecursionNode = PhiNode;

/// ϕ-Nodes represent a environment for mutual recursion, i.e `fn f(){... return f();}`.
///
/// In essence this is similar to the idea of a [λ-Node](crate::nodes::LambdaNode). However, it allows you to not only declare the function (what a LambdaNode does via its single output),
/// but also use that function in that deceleration. This is done by not only setting an output (again, like the LambdaNode), but also having an argument, that
/// is the deceleration defined by the output.
///
///
/// An added concept, next to the already known context_variables are recursion_variables. Their concept is similar to loop_variables. They are
/// the variables needed by the recursive [ApplyNode](crate::nodes::ApplyNode) to represent a variable that is taken, and returned at each recursion level.
/// Have a look at the [ϕ-Builder](crate::builder::PhiBuilder::add_recursion_variable) for an example.
///
///
/// By definition the PhiNode has only an internal [λ-Node](crate::nodes::LambdaNode), where the first context variable is an import of itself. This models the recursion.
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
    pub(crate) outputs: SmallColl<Output>,
    pub(crate) inputs: SmallColl<Input>,
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

    fn outport_types(&self) -> SmallVec<[OutputType; 3]> {
        let mut outs = SmallVec::new();
        for i in 0..self.rv_count {
            outs.push(OutputType::RecursionVariableOutput(i));
        }
        outs
    }
    fn input_types(&self) -> SmallVec<[InputType; 3]> {
        let mut inputs = SmallVec::default();
        //now append all the args
        for i in 0..self.inputs.len() {
            if i < self.cv_count {
                inputs.push(InputType::ContextVariableInput(i));
            } else {
                panic!("ϕ-Node can't have other inputs than CV-inputs.")
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
                if i < (self.cv_count + self.rv_count) {
                    args.push(OutputType::RecursionVariableArgument(i - self.cv_count));
                } else {
                    args.push(OutputType::Argument(i - self.cv_count - self.rv_count));
                }
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
            if i < self.rv_count {
                res.push(InputType::RecursionVariableResult(i));
            } else {
                res.push(InputType::Result(i - self.rv_count));
            }
        }

        res
    }
}

impl PhiNode {
    pub fn new() -> Self {
        PhiNode {
            cv_count: 0,
            rv_count: 0,
            inputs: SmallColl::default(),
            body: Region::new(),
            outputs: SmallColl::default(),
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

    ///Adds an argument to the phi's body. Returns the argument's index.
    pub fn add_argument(&mut self) -> usize {
        let offset = self.body.arguments.len() - self.rv_count - self.cv_count;
        self.body.arguments.push(Argument::default());

        offset
    }

    ///Adds a result to the phi's body. Note that this does NOT mean the output of this phi, but adding
    /// a result to the later evaluated procedure of the body.
    pub fn add_result(&mut self) -> usize {
        self.body.results.push(RegResult::default());
        self.body.results.len() - self.rv_count - 1
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

    ///The amount of arguments, that are no context-variables to this λ-Node's region.
    pub fn argument_count(&self) -> usize {
        self.body.arguments.len() - self.cv_count - self.rv_count
    }

    pub fn result_count(&self) -> usize {
        self.body.results.len() - self.rv_count
    }
}

#[cfg(test)]
mod phitests {
    use smallvec::{smallvec, SmallVec};

    use crate::{
        edge::{InputType, OutputType},
        nodes::{PhiNode, StructuralNode},
    };

    //TODO write those tests for the others as well!
    #[test]
    fn sig_inputs_test() {
        let mut phi = PhiNode::new();

        assert!(phi.add_context_variable() == 0);
        assert!(phi.add_context_variable() == 1);
        assert!(phi.add_recursion_variable() == 0);
        assert!(phi.add_recursion_variable() == 1);
        assert!(phi.add_argument() == 0);
        assert!(phi.add_argument() == 1);
        assert!(phi.add_result() == 0);
        assert!(phi.add_result() == 1);

        let insig = phi.input_types();
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

        let argsig = phi.argument_types(0);
        let expected_arg_sig: SmallVec<[OutputType; 3]> = smallvec![
            OutputType::ContextVariableArgument(0),
            OutputType::ContextVariableArgument(1),
            OutputType::RecursionVariableArgument(0),
            OutputType::RecursionVariableArgument(1),
            OutputType::Argument(0),
            OutputType::Argument(1),
        ];
        assert!(argsig == expected_arg_sig);

        let ressig = phi.result_types(0);
        let expected_ressig: SmallVec<[InputType; 3]> = smallvec![
            InputType::RecursionVariableResult(0),
            InputType::RecursionVariableResult(1),
            InputType::Result(0),
            InputType::Result(1),
        ];
        assert!(ressig == expected_ressig);

        let outsig = phi.outport_types();
        let expected_outsig: SmallVec<[OutputType; 3]> = smallvec![
            OutputType::RecursionVariableOutput(0),
            OutputType::RecursionVariableOutput(1),
        ];
        assert!(outsig == expected_outsig);
    }
}
