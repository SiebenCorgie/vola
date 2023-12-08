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
use std::fmt::{Debug, Display};

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
            Node::Gamma(g) => g.inputs(),
            Node::Theta(g) => &g.inputs(),
            Node::Lambda(g) => &g.inputs(),
            Node::Apply(g) => &g.inputs,
            Node::Delta(g) => &g.inputs(),
            Node::Phi(g) => &g.inputs(),
            Node::Omega(g) => &g.inputs(),
        }
    }

    pub fn inputs_mut(&mut self) -> &mut [Input] {
        match self {
            Node::Simple(node) => node.inputs_mut(),
            Node::Gamma(g) => g.inputs_mut(),
            Node::Theta(g) => g.inputs_mut(),
            Node::Lambda(g) => g.inputs_mut(),
            Node::Apply(g) => &mut g.inputs,
            Node::Delta(g) => g.inputs_mut(),
            Node::Phi(g) => g.inputs_mut(),
            Node::Omega(_g) => &mut [],
        }
    }
    pub fn outputs(&self) -> &[Output] {
        match &self {
            Node::Simple(node) => node.outputs(),
            Node::Gamma(g) => g.outputs(),
            Node::Theta(g) => g.outputs(),
            Node::Lambda(g) => g.outputs(),
            Node::Apply(g) => &g.outputs,
            Node::Delta(g) => g.outputs(),
            Node::Phi(g) => g.outputs(),
            Node::Omega(_g) => &[],
        }
    }

    pub fn outputs_mut(&mut self) -> &mut [Output] {
        match self {
            Node::Simple(node) => node.outputs_mut(),
            Node::Gamma(g) => g.outputs_mut(),
            Node::Theta(g) => g.outputs_mut(),
            Node::Lambda(g) => g.outputs_mut(),
            Node::Apply(g) => &mut g.outputs,
            Node::Delta(g) => g.outputs_mut(),
            Node::Phi(g) => g.outputs_mut(),
            Node::Omega(_g) => &mut [],
        }
    }

    ///Reference to all internal regions. This will mostly have length 0/1. Only gamma nodes have >1 regions.
    pub fn regions(&self) -> &[Region] {
        match &self {
            Node::Simple(_node) => &[],
            Node::Gamma(g) => g.regions(),
            Node::Theta(g) => g.regions(),
            Node::Lambda(g) => g.regions(),
            Node::Apply(_g) => &[],
            Node::Delta(g) => g.regions(),
            Node::Phi(g) => g.regions(),
            Node::Omega(g) => g.regions(),
        }
    }
    ///Reference to all internal regions. This will mostly have length 0/1. Only gamma nodes have >1 regions.
    pub fn regions_mut(&mut self) -> &mut [Region] {
        match self {
            Node::Simple(_node) => &mut [],
            Node::Gamma(g) => g.regions_mut(),
            Node::Theta(g) => g.regions_mut(),
            Node::Lambda(g) => g.regions_mut(),
            Node::Apply(_g) => &mut [],
            Node::Delta(g) => g.regions_mut(),
            Node::Phi(g) => g.regions_mut(),
            Node::Omega(g) => g.regions_mut(),
        }
    }

    pub fn legalize(&mut self) -> Result<(), LegalizationError> {
        todo!("Implement legalization")
    }

    ///Tries to translate the `port` into a valid port.
    pub fn outport(&self, port_ty: &OutputType) -> Option<&Outport> {
        match port_ty {
            OutputType::Output(p) => self.outputs().get(*p),
            _ => match self {
                Node::Simple(_) | Node::Apply(_) => None,
                Node::Theta(g) => g.outport(port_ty),
                Node::Lambda(g) => g.outport(port_ty),
                Node::Delta(g) => g.outport(port_ty),
                Node::Phi(g) => g.outport(port_ty),
                Node::Omega(g) => g.outport(port_ty),
                Node::Gamma(g) => g.outport(port_ty),
            },
        }
    }

    ///Tries to translate the `port` into a valid port.
    pub fn outport_mut(&mut self, port_ty: &OutputType) -> Option<&mut Outport> {
        match port_ty {
            OutputType::Output(p) => self.outputs_mut().get_mut(*p),
            _ => match self {
                Node::Simple(_) | Node::Apply(_) => None,
                Node::Theta(g) => g.outport_mut(port_ty),
                Node::Lambda(g) => g.outport_mut(port_ty),
                Node::Delta(g) => g.outport_mut(port_ty),
                Node::Phi(g) => g.outport_mut(port_ty),
                Node::Omega(g) => g.outport_mut(port_ty),
                Node::Gamma(g) => g.outport_mut(port_ty),
            },
        }
    }

    pub fn inport(&self, port_ty: &InputType) -> Option<&Inport> {
        match port_ty {
            InputType::Input(i) => self.inputs().get(*i),
            _ => match self {
                Node::Simple(_) | Node::Apply(_) => None,
                Node::Theta(g) => g.inport(port_ty),
                Node::Lambda(g) => g.inport(port_ty),
                Node::Delta(g) => g.inport(port_ty),
                Node::Phi(g) => g.inport(port_ty),
                Node::Omega(g) => g.inport(port_ty),
                Node::Gamma(g) => g.inport(port_ty),
            },
        }
    }

    pub fn inport_mut(&mut self, port_ty: &InputType) -> Option<&mut Inport> {
        match port_ty {
            InputType::Input(i) => self.inputs_mut().get_mut(*i),
            _ => match self {
                Node::Simple(_) | Node::Apply(_) => None,
                Node::Theta(g) => g.inport_mut(port_ty),
                Node::Lambda(g) => g.inport_mut(port_ty),
                Node::Delta(g) => g.inport_mut(port_ty),
                Node::Phi(g) => g.inport_mut(port_ty),
                Node::Omega(g) => g.inport_mut(port_ty),
                Node::Gamma(g) => g.inport_mut(port_ty),
            },
        }
    }

    ///Returns true if this is either a lambda, or phi node
    pub fn is_callable(&self) -> bool {
        if let Node::Lambda(_) | Node::Phi(_) = self {
            true
        } else {
            false
        }
    }

    ///Returns true, if any input or output port has a connection
    pub fn is_connected(&self) -> bool {
        for input in self.inputs().iter() {
            if input.edge.is_some() {
                return true;
            }
        }
        for output in self.outputs().iter() {
            if !output.edges.is_empty() {
                return true;
            }
        }

        false
    }
}

impl<N: LangNode + Debug + 'static> Display for Node<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Apply(_a) => write!(f, "Apply"),
            Node::Delta(d) => write!(f, "Delta({} cv, {} nodes)", d.cv_count, d.body.nodes.len()),
            Node::Gamma(g) => write!(
                f,
                "Gamma({} branches, {} entry-vars, {} exit-vars)",
                g.regions.len(),
                g.entry_var_count,
                g.exit_var_count
            ),
            Node::Lambda(l) => write!(
                f,
                "Lambda({} arguments, {} results)",
                l.body.arguments.len(),
                l.body.results.len()
            ),
            Node::Omega(o) => write!(
                f,
                "Omega({} imports, {} exports, {} top-level nodes)",
                o.body.arguments.len(),
                o.body.results.len(),
                o.body.nodes.len()
            ),
            Node::Phi(p) => write!(f, "Phi({} rv, {} cv)", p.rv_count, p.cv_count),
            Node::Simple(s) => write!(f, "Simple({:?})", s),
            Node::Theta(t) => write!(f, "Theta({} lv)", t.lv_count),
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
    ///Returns all inputs to a node
    fn inputs(&self) -> &[Input];
    ///Returns all inputs to a node
    fn inputs_mut(&mut self) -> &mut [Input];
    ///Returns all outputs to a node
    fn outputs(&self) -> &[Output];
    ///Returns all outputs to a node
    fn outputs_mut(&mut self) -> &mut [Output];
    fn context_variable_count(&self) -> usize {
        0
    }
    fn recursion_variable_count(&self) -> usize {
        0
    }
    ///Returns how many inputs this node has, that are not context- or recursion-variables.
    fn std_input_count(&self) -> usize {
        self.inputs().len() - self.context_variable_count() - self.recursion_variable_count()
    }

    ///Returns how many outputs this node has, that are not recursion-variables.
    fn std_output_count(&self) -> usize {
        self.outputs().len() - self.recursion_variable_count()
    }
}
