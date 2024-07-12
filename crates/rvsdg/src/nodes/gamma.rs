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

use super::StructuralNode;

///The γ-node is a n-way decision node. This maps to the concept of C's if-then-else or switch-case.
/// It has at least a predicate edge that maps to 0..n, as well as n regions.
///
/// All regions must have the same signature as this γ-node (excluding the predicate).
#[derive(Debug, Clone)]
pub struct GammaNode {
    pub(crate) entry_var_count: usize,
    pub(crate) exit_var_count: usize,
    pub(crate) regions: SmallColl<Region>,
    pub(crate) inputs: SmallColl<Input>,
    pub(crate) outputs: SmallColl<Output>,
}

impl StructuralNode for GammaNode {
    fn regions(&self) -> &[Region] {
        &self.regions
    }
    fn regions_mut(&mut self) -> &mut [Region] {
        &mut self.regions
    }
    fn inport(&self, ty: &InputType) -> Option<&Input> {
        match ty {
            InputType::Input(n) => self.inputs.get(*n),
            InputType::GammaPredicate => Some(self.predicate()),
            InputType::EntryVariableInput(i) => self.entry_var_input(*i),
            InputType::ExitVariableResult {
                branch,
                exit_variable,
            } => self.exit_var_result(*exit_variable, *branch),
            _ => None,
        }
    }
    fn inport_mut(&mut self, ty: &InputType) -> Option<&mut Input> {
        match ty {
            InputType::Input(n) => self.inputs.get_mut(*n),
            InputType::GammaPredicate => Some(self.predicate_mut()),
            InputType::EntryVariableInput(i) => self.entry_var_input_mut(*i),
            InputType::ExitVariableResult {
                branch,
                exit_variable,
            } => self.exit_var_result_mut(*exit_variable, *branch),
            _ => None,
        }
    }
    fn outport(&self, ty: &OutputType) -> Option<&Output> {
        match ty {
            OutputType::Output(n) => self.outputs.get(*n),
            OutputType::EntryVariableArgument {
                branch,
                entry_variable,
            } => {
                if let Some(reg) = self.regions.get(*branch) {
                    reg.arguments.get(*entry_variable)
                } else {
                    None
                }
            }
            OutputType::ExitVariableOutput(n) => self.exit_var_output(*n),
            _ => None,
        }
    }
    fn outport_mut(&mut self, ty: &OutputType) -> Option<&mut Output> {
        match ty {
            OutputType::Output(n) => self.outputs.get_mut(*n),
            OutputType::EntryVariableArgument {
                branch,
                entry_variable,
            } => {
                if let Some(reg) = self.regions.get_mut(*branch) {
                    reg.arguments.get_mut(*entry_variable)
                } else {
                    None
                }
            }
            OutputType::ExitVariableOutput(n) => self.exit_var_output_mut(*n),
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

    fn outport_types(&self) -> SmallVec<[OutputType; 3]> {
        let mut outs = SmallVec::new();
        for outidx in 0..self.exit_var_count {
            outs.push(OutputType::ExitVariableOutput(outidx));
        }
        outs
    }
    fn input_types(&self) -> SmallVec<[InputType; 3]> {
        let mut inputs = SmallVec::default();
        //prepend the gamma-predicate
        inputs.push(InputType::GammaPredicate);
        //now append all the args
        for i in 0..self.entry_var_count {
            inputs.push(InputType::EntryVariableInput(i))
        }

        inputs
    }
    fn argument_types(&self, region_index: usize) -> SmallVec<[OutputType; 3]> {
        if region_index >= self.regions.len() {
            return SmallVec::new();
        }

        let mut args = SmallVec::new();
        for i in 0..self.entry_var_count {
            args.push(OutputType::EntryVariableArgument {
                branch: region_index,
                entry_variable: i,
            })
        }
        args
    }
    fn result_types(&self, region_index: usize) -> SmallVec<[InputType; 3]> {
        //Does not exist!
        if region_index >= self.regions.len() {
            return SmallVec::new();
        }

        let mut res = SmallVec::new();
        for i in 0..self.exit_var_count {
            res.push(InputType::ExitVariableResult {
                branch: region_index,
                exit_variable: i,
            })
        }

        res
    }
}

impl GammaNode {
    pub fn new() -> Self {
        let mut inputs = SmallColl::default();
        inputs.push(Input::default());
        GammaNode {
            entry_var_count: 0,
            exit_var_count: 0,
            regions: SmallColl::default(),
            inputs,
            outputs: SmallColl::default(),
        }
    }

    ///Adds a new port for an entry variable.
    pub fn add_entry_var(&mut self) -> usize {
        let idx = self.entry_var_count;
        self.entry_var_count += 1;
        //NOTE: offset by one, since the first is the criteria
        self.inputs.insert(idx + 1, Input::default());

        //now add at same location to arguments of inner blocks
        for r in &mut self.regions {
            r.arguments.insert(idx, Argument::default());
        }

        idx
    }

    ///Adds a new port for an exit variable.
    pub fn add_exit_var(&mut self) -> usize {
        let idx = self.exit_var_count;
        self.exit_var_count += 1;
        self.outputs.insert(idx, Output::default());

        //now add at same location to arguments of inner blocks
        for r in &mut self.regions {
            r.results.insert(idx, RegResult::default());
        }

        idx
    }

    ///Adds a new decision branch / region.
    pub fn add_region(&mut self) -> usize {
        let mut r = Region::new();

        //Setup region with same input/output count thats already valid
        for _ in 0..self.entry_var_count {
            r.arguments.push(Argument::default());
        }
        for _ in 0..self.exit_var_count {
            r.results.push(RegResult::default());
        }

        self.regions.push(r);
        self.regions.len() - 1
    }

    ///Returns the `n`-th entry_variable argument to the `r`-th region.
    pub fn entry_var_argument(&self, n: usize, r: usize) -> Option<&Argument> {
        if let Some(reg) = self.regions.get(r) {
            reg.arguments.get(n)
        } else {
            None
        }
    }

    ///Returns the `n`-th entry_variable argument to the `r`-th region.
    pub fn entry_var_argument_mut(&mut self, n: usize, r: usize) -> Option<&mut Argument> {
        if let Some(reg) = self.regions.get_mut(r) {
            reg.arguments.get_mut(n)
        } else {
            None
        }
    }

    ///Returns the [Input] that is mapped to the `n`-th entry_variable of this gamma-node.
    pub fn entry_var_input(&self, n: usize) -> Option<&Input> {
        self.inputs.get(n + 1)
    }

    ///Returns the [Input] that is mapped to the `n`-th entry_variable of this gamma-node.
    pub fn entry_var_input_mut(&mut self, n: usize) -> Option<&mut Input> {
        self.inputs.get_mut(n + 1)
    }

    ///Returns the [Output] that is mapped to the `n`-th exit_variable of this gamma-node.
    pub fn exit_var_output(&self, n: usize) -> Option<&Output> {
        self.outputs.get(n)
    }

    ///Returns the [Output] that is mapped to the `n`-th exit_variable of this gamma-node.
    pub fn exit_var_output_mut(&mut self, n: usize) -> Option<&mut Output> {
        self.outputs.get_mut(n)
    }

    ///Returns the `n`-th exit_variable result to the `r`-th region.
    pub fn exit_var_result(&self, n: usize, r: usize) -> Option<&RegResult> {
        if let Some(reg) = self.regions.get(r) {
            reg.results.get(n)
        } else {
            None
        }
    }
    ///Returns the `n`-th exit_variable result to the `r`-th region.
    pub fn exit_var_result_mut(&mut self, n: usize, r: usize) -> Option<&mut RegResult> {
        if let Some(reg) = self.regions.get_mut(r) {
            reg.results.get_mut(n)
        } else {
            None
        }
    }

    ///Returns the Gamma's predicate, which is always the first input to the gamma node.
    pub fn predicate(&self) -> &Input {
        &self.inputs[0]
    }

    ///Returns the Gamma's predicate, which is always the first input to the gamma node.
    pub fn predicate_mut(&mut self) -> &mut Input {
        &mut self.inputs[0]
    }

    pub fn entry_var_count(&self) -> usize {
        self.entry_var_count
    }
    pub fn exit_var_count(&self) -> usize {
        self.exit_var_count
    }
}

pub type DecisionNode = GammaNode;

#[cfg(test)]
mod gammatests {
    use smallvec::{smallvec, SmallVec};

    use crate::{
        edge::{InputType, OutputType},
        nodes::StructuralNode,
    };

    use super::GammaNode;

    //TODO write those tests for the others as well!
    #[test]
    fn sig_inputs_test() {
        let mut gamma = GammaNode::new();
        let a = gamma.add_entry_var();
        let b = gamma.add_entry_var();
        let c = gamma.add_exit_var();
        assert!(gamma.add_region() == 0);
        assert!(a == 0);
        assert!(b == 1);
        assert!(c == 0);

        let insig = gamma.input_types();
        let expected_in_sig: SmallVec<[InputType; 3]> = smallvec![
            InputType::GammaPredicate,
            InputType::EntryVariableInput(a),
            InputType::EntryVariableInput(b)
        ];
        assert!(
            insig == expected_in_sig,
            "{:?} != {:?}",
            insig,
            expected_in_sig
        );

        let argsig = gamma.argument_types(0);
        let expected_arg_sig: SmallVec<[OutputType; 3]> = smallvec![
            OutputType::EntryVariableArgument {
                branch: 0,
                entry_variable: 0
            },
            OutputType::EntryVariableArgument {
                branch: 0,
                entry_variable: 1
            }
        ];
        assert!(argsig == expected_arg_sig);

        let ressig = gamma.result_types(0);
        let expected_ressig: SmallVec<[InputType; 3]> = smallvec![InputType::ExitVariableResult {
            branch: 0,
            exit_variable: 0
        }];
        assert!(ressig == expected_ressig);

        let outsig = gamma.outport_types();
        let expected_outsig: SmallVec<[OutputType; 3]> =
            smallvec![OutputType::ExitVariableOutput(0)];
        assert!(outsig == expected_outsig);
    }
}
