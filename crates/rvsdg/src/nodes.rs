//! # RVSDG-Nodes
//!
//! The represents data, and control-flow in a single graph. The graph is acyclic. It therfore introduces different nodes to represent
//! cyclic dependencies like loops and mutual recursion.
//!
//! State and value dependencies are modeled via edges between nodes, where each edge either represents a value, or state dependency.
//!
//! The crate uses the node names of the source paper, however, we also export _common_ names. The mapping is as follows
//!
//!
//! - [GammaNode] ~ [DecisionNode] : n-way switch-case
//! - [ThetaNode] ~ [LoopNode] : tail-controlled loop
//! - [LambdaNode] ~ [FunctionNode] : defines a function, outputs its own definition. Can be called via an [ApplyNode].
//! - [DeltaNode] ~ [GlobalVariable] : As the name implies, global data. Can have external input, so this is not necessarily an constant value.
//! - [PhiNode] ~ [RecursionNode] : Models mutual recursion. Similarly to function nodes in uses _context-variables_ to import state that is used internally. _Recursion-Variables_ are used to represent in-and output exclusive to the recursion border.
//! - [OmegaNode] ~ [TranslationUnit] : Represents the whole translation unit. Based on the internal region we can identify imported and exported state.

use tinyvec::TinyVec;
pub mod relations;

use crate::{
    edge::{InputType, OutputType},
    err::LegalizationError,
    region::{Argument, Inport, Input, Outport, Output, RegResult, Region},
};

///simple node of a language. The trait lets us embed such a node in our overall RVSDG graph.
///
/// For the RVSDG the in and out edges are the most interesting aspects.
pub trait LangNode {
    fn inputs(&self) -> &[Input];
    fn inputs_mut(&mut self) -> &mut [Input];
    fn outputs(&self) -> &[Output];
    fn outputs_mut(&mut self) -> &mut [Output];
}
///Different node types as outlined in section 4. of the RVSDG paper. The
/// _Simple_ node represent your IR's instruction. All other nodes are RVSDG
/// specific architectural nodes.
#[derive(Debug, Clone)]
pub enum Node<N: LangNode + 'static> {
    ///Simple input/output sequential node
    Simple(N),
    ///Decision point node of matching signatures
    Gamma(GammaNode),
    ///Tail controlled loop
    Theta(ThetaNode),
    ///Models a function, meaning its signature (multiple inputs), body-region and a single output.
    Lambda(LambdaNode),
    ///Function call node
    Apply(ApplyNode),
    ///Models a global variable. Contains a region which represents the constant value.
    Delta(DeltaNode),
    ///Models a environment with mutual recursive functions. This allows us to model the case _f() calls f()_ in the
    /// the otherwise acyclic RVSDG.
    Phi(PhiNode),
    ///Represents the top-node of the RVSDG. This allows us to clearly define import and exported values.
    Omega(OmegaNode),
    ///Represents an invalid node. Mostly used to either allocate node keys, or represent invalid graph state
    Invalid,
}

impl<N: LangNode + 'static> Node<N> {
    pub fn inputs(&self) -> &[Input] {
        match &self {
            Node::Simple(node) => node.inputs(),
            Node::Gamma(g) => &g.inputs,
            Node::Theta(g) => &g.inputs,
            Node::Lambda(g) => &g.inputs,
            Node::Apply(g) => &g.inputs,
            Node::Delta(g) => &g.inputs,
            Node::Phi(g) => &g.inputs,
            Node::Omega(_g) => &[],
            Node::Invalid => &[],
        }
    }

    pub fn inputs_mut(&mut self) -> &mut [Input] {
        match self {
            Node::Simple(node) => node.inputs_mut(),
            Node::Gamma(g) => &mut g.inputs,
            Node::Theta(g) => &mut g.inputs,
            Node::Lambda(g) => &mut g.inputs,
            Node::Apply(g) => &mut g.inputs,
            Node::Delta(g) => &mut g.inputs,
            Node::Phi(g) => &mut g.inputs,
            Node::Omega(_g) => &mut [],
            Node::Invalid => &mut [],
        }
    }
    pub fn outputs(&self) -> &[Output] {
        match &self {
            Node::Simple(node) => node.outputs(),
            Node::Gamma(g) => &g.outputs,
            Node::Theta(g) => &g.outputs,
            Node::Lambda(g) => core::slice::from_ref(&g.output),
            Node::Apply(g) => &g.outputs,
            Node::Delta(g) => core::slice::from_ref(&g.output),
            Node::Phi(g) => &g.outputs,
            Node::Omega(_g) => &[],
            Node::Invalid => &[],
        }
    }

    pub fn outputs_mut(&mut self) -> &mut [Output] {
        match self {
            Node::Simple(node) => node.outputs_mut(),
            Node::Gamma(g) => &mut g.outputs,
            Node::Theta(g) => &mut g.outputs,
            Node::Lambda(g) => core::slice::from_mut(&mut g.output),
            Node::Apply(g) => &mut g.outputs,
            Node::Delta(g) => core::slice::from_mut(&mut g.output),
            Node::Phi(g) => &mut g.outputs,
            Node::Omega(_g) => &mut [],
            Node::Invalid => &mut [],
        }
    }

    ///Reference to all internal regions. This will mostly have length 0/1. Only gamma nodes have >1 regions.
    pub fn regions(&self) -> &[Region] {
        match &self {
            Node::Simple(_node) => &[],
            Node::Gamma(g) => &g.regions,
            Node::Theta(g) => core::slice::from_ref(&g.loop_body),
            Node::Lambda(g) => core::slice::from_ref(&g.body),
            Node::Apply(_g) => &[],
            Node::Delta(g) => core::slice::from_ref(&g.body),
            Node::Phi(g) => core::slice::from_ref(&g.body),
            Node::Omega(g) => core::slice::from_ref(&g.body),
            Node::Invalid => &[],
        }
    }
    ///Reference to all internal regions. This will mostly have length 0/1. Only gamma nodes have >1 regions.
    pub fn regions_mut(&mut self) -> &mut [Region] {
        match self {
            Node::Simple(_node) => &mut [],
            Node::Gamma(g) => &mut g.regions,
            Node::Theta(g) => core::slice::from_mut(&mut g.loop_body),
            Node::Lambda(g) => core::slice::from_mut(&mut g.body),
            Node::Apply(_g) => &mut [],
            Node::Delta(g) => core::slice::from_mut(&mut g.body),
            Node::Phi(g) => core::slice::from_mut(&mut g.body),
            Node::Omega(g) => core::slice::from_mut(&mut g.body),
            Node::Invalid => &mut [],
        }
    }

    pub fn legalize(&mut self) -> Result<(), LegalizationError> {
        todo!("Implement legalization")
    }

    ///Tries to translate the `port` into a valid port.
    pub fn outport(&self, port_ty: &OutputType) -> Option<&Outport> {
        match port_ty {
            OutputType::Output(n) => self.outputs().get(*n),
            OutputType::Argument(n) => match self {
                Node::Simple(_node) => None,
                Node::Gamma(_g) => None,
                Node::Theta(g) => g.loop_body.arguments.get(*n),
                Node::Lambda(g) => g.body.arguments.get(*n + g.cv_count),
                Node::Apply(_g) => None,
                Node::Delta(_g) => None,
                Node::Phi(g) => g.body.arguments.get(*n + g.cv_count + g.rv_count),
                Node::Omega(g) => g.body.arguments.get(*n),
                Node::Invalid => None,
            },
            OutputType::LambdaDecleration => {
                if let Node::Lambda(l) = self {
                    Some(&l.output)
                } else {
                    None
                }
            }
            OutputType::DeltaDecleration => {
                if let Node::Delta(d) = self {
                    Some(&d.output)
                } else {
                    None
                }
            }
            OutputType::RecursionVariableOutput(n) => {
                if let Node::Phi(p) = self {
                    p.rv_output(*n)
                } else {
                    None
                }
            }
            OutputType::RecursionVariableArgument(n) => {
                if let Node::Phi(p) = self {
                    p.rv_argument(*n)
                } else {
                    None
                }
            }
            OutputType::EntryVariableArgument {
                branch,
                entry_variable,
            } => {
                if let Node::Gamma(g) = self {
                    if let Some(reg) = g.regions.get(*branch) {
                        reg.arguments.get(*entry_variable)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            OutputType::ContextVariableArgument(n) => match self {
                Node::Lambda(l) => l.cv_argument(*n),
                Node::Phi(p) => p.cv_argument(*n),
                Node::Delta(d) => d.cv_argument(*n),
                _ => None,
            },
            OutputType::ExitVariableOutput(n) => {
                if let Node::Gamma(g) = self {
                    g.exit_var_output(*n)
                } else {
                    None
                }
            }
        }
    }

    ///Tries to translate the `port` into a valid port.
    pub fn outport_mut(&mut self, port_ty: &OutputType) -> Option<&mut Outport> {
        match port_ty {
            OutputType::Output(n) => self.outputs_mut().get_mut(*n),
            OutputType::Argument(n) => match self {
                Node::Simple(_node) => None,
                Node::Gamma(_g) => None,
                Node::Theta(g) => g.loop_body.arguments.get_mut(*n),
                Node::Lambda(g) => g.body.arguments.get_mut(*n + g.cv_count),
                Node::Apply(_g) => None,
                Node::Delta(_g) => None,
                Node::Phi(g) => g.body.arguments.get_mut(*n + g.cv_count + g.rv_count),
                Node::Omega(g) => g.body.arguments.get_mut(*n),
                Node::Invalid => None,
            },
            OutputType::LambdaDecleration => {
                if let Node::Lambda(l) = self {
                    Some(&mut l.output)
                } else {
                    None
                }
            }
            OutputType::DeltaDecleration => {
                if let Node::Delta(d) = self {
                    Some(&mut d.output)
                } else {
                    None
                }
            }
            OutputType::RecursionVariableOutput(n) => {
                if let Node::Phi(p) = self {
                    p.rv_output_mut(*n)
                } else {
                    None
                }
            }
            OutputType::RecursionVariableArgument(n) => {
                if let Node::Phi(p) = self {
                    p.rv_argument_mut(*n)
                } else {
                    None
                }
            }
            OutputType::EntryVariableArgument {
                branch,
                entry_variable,
            } => {
                if let Node::Gamma(g) = self {
                    if let Some(reg) = g.regions.get_mut(*branch) {
                        reg.arguments.get_mut(*entry_variable)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            OutputType::ContextVariableArgument(n) => match self {
                Node::Lambda(l) => l.cv_argument_mut(*n),
                Node::Phi(p) => p.cv_argument_mut(*n),
                Node::Delta(d) => d.cv_argument_mut(*n),
                _ => None,
            },
            OutputType::ExitVariableOutput(n) => {
                if let Node::Gamma(g) = self {
                    g.exit_var_output_mut(*n)
                } else {
                    None
                }
            }
        }
    }

    pub fn inport(&self, port_ty: &InputType) -> Option<&Inport> {
        match port_ty {
            InputType::Input(u) => self.inputs().get(*u),
            InputType::Result(n) => match self {
                Node::Simple(_node) => None,
                Node::Gamma(_g) => None,
                Node::Theta(g) => g.loop_body.results.get(*n + 1),
                Node::Lambda(g) => g.body.results.get(*n),
                Node::Apply(_g) => None,
                Node::Delta(g) => g.body.results.get(*n),
                Node::Phi(g) => g.body.results.get(*n + g.rv_count),
                Node::Omega(g) => g.body.results.get(*n),
                Node::Invalid => None,
            },
            InputType::GammaPredicate => {
                if let Node::Gamma(g) = self {
                    Some(g.predicate())
                } else {
                    None
                }
            }
            InputType::ThetaPredicate => {
                if let Node::Theta(t) = self {
                    Some(t.loop_predicate())
                } else {
                    None
                }
            }
            InputType::ExitVariableResult {
                branch,
                exit_variable,
            } => {
                if let Node::Gamma(g) = self {
                    g.exit_var_result(*exit_variable, *branch)
                } else {
                    None
                }
            }
            InputType::EntryVariableInput(i) => {
                if let Node::Gamma(g) = self {
                    g.entry_var_input(*i)
                } else {
                    None
                }
            }
            InputType::RecursionVariableResult(r) => {
                if let Node::Phi(p) = self {
                    p.rv_result(*r)
                } else {
                    None
                }
            }
            InputType::ContextVariableInput(n) => match self {
                Node::Lambda(l) => l.cv_input(*n),
                Node::Phi(p) => p.cv_input(*n),
                Node::Delta(d) => d.cv_input(*n),
                _ => None,
            },
        }
    }

    pub fn inport_mut(&mut self, port_ty: &InputType) -> Option<&mut Inport> {
        match port_ty {
            InputType::Input(u) => self.inputs_mut().get_mut(*u),
            InputType::Result(n) => match self {
                Node::Simple(_node) => None,
                Node::Gamma(_g) => None,
                Node::Theta(g) => g.loop_body.results.get_mut(*n + 1),
                Node::Lambda(g) => g.body.results.get_mut(*n),
                Node::Apply(_g) => None,
                Node::Delta(g) => g.body.results.get_mut(*n),
                Node::Phi(g) => g.body.results.get_mut(*n + g.rv_count),
                Node::Omega(g) => g.body.results.get_mut(*n),
                Node::Invalid => None,
            },
            InputType::GammaPredicate => {
                if let Node::Gamma(g) = self {
                    Some(g.predicate_mut())
                } else {
                    None
                }
            }
            InputType::ThetaPredicate => {
                if let Node::Theta(t) = self {
                    Some(t.loop_predicate_mut())
                } else {
                    None
                }
            }
            InputType::ExitVariableResult {
                branch,
                exit_variable,
            } => {
                if let Node::Gamma(g) = self {
                    g.exit_var_result_mut(*exit_variable, *branch)
                } else {
                    None
                }
            }
            InputType::EntryVariableInput(i) => {
                if let Node::Gamma(g) = self {
                    g.entry_var_input_mut(*i)
                } else {
                    None
                }
            }
            InputType::RecursionVariableResult(r) => {
                if let Node::Phi(p) = self {
                    p.rv_result_mut(*r)
                } else {
                    None
                }
            }
            InputType::ContextVariableInput(n) => match self {
                Node::Lambda(l) => l.cv_input_mut(*n),
                Node::Phi(p) => p.cv_input_mut(*n),
                Node::Delta(d) => d.cv_input_mut(*n),
                _ => None,
            },
        }
    }
}

pub type DecisionNode = GammaNode;

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
    pub(crate) inputs: TinyVec<[Input; 3]>,
    pub(crate) outputs: TinyVec<[Output; 3]>,
}

impl ThetaNode {
    pub fn new() -> Self {
        let mut loop_body = Region::new();
        //add the loop criteria
        loop_body.results.insert(0, RegResult::default());
        ThetaNode {
            lv_count: 0,
            loop_body,
            inputs: TinyVec::default(),
            outputs: TinyVec::default(),
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

///Related to the [λ-Node](LambdaNode). Represents a call of some function. The first port is defined as the `caller`, which must be connected to a
/// [LambdaNode].
#[derive(Debug, Clone)]
pub struct ApplyNode {
    ///Function being called, must be a edge to a lambdaNode
    pub(crate) inputs: TinyVec<[Input; 3]>,
    pub(crate) outputs: TinyVec<[Output; 3]>,
}

impl ApplyNode {
    pub fn new() -> Self {
        let mut inputs = TinyVec::default();
        //The function input to apply
        inputs.insert(0, Input::default());
        ApplyNode {
            inputs,
            outputs: TinyVec::default(),
        }
    }

    ///Creates a call that has the signature needed for the given lambda node
    pub fn new_for_lambda(node: &LambdaNode) -> Self {
        let node_body = &node.body;
        ApplyNode {
            inputs: node_body
                .arguments
                .iter()
                .map(|_p| Input::default())
                .collect(),
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
    pub(crate) inputs: TinyVec<[Input; 3]>,
    pub(crate) output: Output,
    pub(crate) body: Region,
}

impl LambdaNode {
    pub fn new() -> Self {
        LambdaNode {
            cv_count: 0,
            inputs: TinyVec::default(),
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
        let args = self.body.arguments.len();

        args - cv_count
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
}

pub type GlobalVariable = DeltaNode;

/// δ-Nodes represents a global variable. It can have a set of inputs needed to define the variable. Similarly to [λ-Node](LambdaNode)s
/// context-variables can import inner, dependent state, like λ-Nodes or other arguments.
///
/// A δ-node must always provide a single output.
#[derive(Debug, Clone)]
pub struct DeltaNode {
    pub(crate) cv_count: usize,
    ///All inputs of a delta node are context variables.
    pub(crate) inputs: TinyVec<[Input; 3]>,
    pub(crate) body: Region,
    pub(crate) output: Output,
}

impl DeltaNode {
    pub fn new() -> Self {
        DeltaNode {
            cv_count: 0,
            inputs: TinyVec::default(),
            body: Region::new(),
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

pub type TranslationUnit = OmegaNode;

///ω-node models a translation unit. It therefore has no inputs or outputs. It contains exactly one region, which in/outputs model
/// external dependencies to the translation unit.
#[derive(Debug, Clone)]
pub struct OmegaNode {
    pub(crate) body: Region,
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
