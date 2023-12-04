use std::slice;

use crate::{
    edge::{InputType, OutputType},
    region::{Argument, Input, Output, RegResult, Region},
};

use super::StructuralNode;

pub type TranslationUnit = OmegaNode;

///ω-node models a translation unit. It therefore has no inputs or outputs. It contains exactly one region, which in/outputs model
/// external dependencies to the translation unit.
#[derive(Debug, Clone)]
pub struct OmegaNode {
    pub(crate) body: Region,
}

impl StructuralNode for OmegaNode {
    fn regions(&self) -> &[Region] {
        slice::from_ref(&self.body)
    }
    fn regions_mut(&mut self) -> &mut [Region] {
        slice::from_mut(&mut self.body)
    }
    fn inport(&self, ty: &InputType) -> Option<&Input> {
        match ty {
            InputType::Result(n) => self.body.results.get(*n),
            _ => None,
        }
    }
    fn inport_mut(&mut self, ty: &InputType) -> Option<&mut Input> {
        match ty {
            InputType::Result(n) => self.body.results.get_mut(*n),
            _ => None,
        }
    }
    fn outport(&self, ty: &OutputType) -> Option<&Output> {
        match ty {
            OutputType::Argument(n) => self.body.arguments.get(*n),
            _ => None,
        }
    }
    fn outport_mut(&mut self, ty: &OutputType) -> Option<&mut Output> {
        match ty {
            OutputType::Argument(n) => self.body.arguments.get_mut(*n),
            _ => None,
        }
    }

    fn inputs(&self) -> &[Input] {
        &[]
    }
    fn inputs_mut(&mut self) -> &mut [Input] {
        &mut []
    }
    fn outputs(&self) -> &[Output] {
        &[]
    }
    fn outputs_mut(&mut self) -> &mut [Output] {
        &mut []
    }
}

impl OmegaNode {
    pub fn new() -> Self {
        OmegaNode {
            body: Region::new(),
        }
    }

    pub fn add_import(&mut self) -> usize {
        let pushed_to = self.body.arguments.len();
        self.body.arguments.push(Argument::default());
        pushed_to
    }

    pub fn add_export(&mut self) -> usize {
        let pushed_to = self.body.results.len();
        self.body.results.push(RegResult::default());
        pushed_to
    }

    ///Returns the `n`-th imported variable
    pub fn get_import(&self, n: usize) -> Option<&Argument> {
        self.body.arguments.get(n)
    }
    ///Returns the `n`-th imported variable
    pub fn get_import_mut(&mut self, n: usize) -> Option<&mut Argument> {
        self.body.arguments.get_mut(n)
    }

    ///Returns the `n`-th exported variable
    pub fn get_export(&self, n: usize) -> Option<&RegResult> {
        self.body.results.get(n)
    }
    ///Returns the `n`-th exported variable
    pub fn get_export_mut(&mut self, n: usize) -> Option<&mut RegResult> {
        self.body.results.get_mut(n)
    }
}
