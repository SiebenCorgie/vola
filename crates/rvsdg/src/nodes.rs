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

mod delta;
pub use delta::{DeltaNode, GlobalVariable};
mod gamma;
pub use gamma::{DecisionNode, GammaNode};
mod lambda;
pub use lambda::{ApplyNode, FunctionNode, LambdaNode};
mod omega;
pub use omega::{OmegaNode, TranslationUnit};
mod phi;
pub use phi::{PhiNode, RecursionNode};
pub mod relations;
mod theta;
pub use theta::{LoopNode, ThetaNode};

use crate::{
    edge::{InputType, OutputType},
    err::LegalizationError,
    region::{Inport, Input, Outport, Output, Region},
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

///Interface that is shared for all structural nodes. A structural node, as the name implies is a node that is used
/// to structure the graph. In our case this are all node types except for the [ApplyNode] and the `SimpleNode` in [Node].
pub trait StructuralNode {
    fn regions(&self) -> &[Region];
    fn regions_mut(&mut self) -> &mut [Region];
    fn outport(&self, ty: &OutputType) -> Option<&Output>;
    fn outport_mut(&mut self, ty: &OutputType) -> Option<&mut Output>;
    fn inport(&self, ty: &InputType) -> Option<&Input>;
    fn inport_mut(&mut self, ty: &InputType) -> Option<&mut Input>;
}
