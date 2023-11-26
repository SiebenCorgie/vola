use tinyvec::TinyVec;

use crate::{
    edge::{InputType, OutputType},
    region::{Argument, Input, Output, RegResult, Region},
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
    pub(crate) regions: TinyVec<[Region; 3]>,
    pub(crate) inputs: TinyVec<[Input; 3]>,
    pub(crate) outputs: TinyVec<[Output; 3]>,
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
}

impl GammaNode {
    pub fn new() -> Self {
        let mut inputs = TinyVec::default();
        inputs.push(Input::default());
        GammaNode {
            entry_var_count: 0,
            exit_var_count: 0,
            regions: TinyVec::default(),
            inputs,
            outputs: TinyVec::default(),
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
        let arg_count = self.inputs.len();
        let res_count = self.outputs.len();

        //Setup region with same input/output count thats already valid
        for _ in 0..arg_count {
            r.arguments.push(Argument::default());
        }
        for _ in 0..res_count {
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
}

pub type DecisionNode = GammaNode;
