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
    region::{Inport, Input, Outport, Output, Region, RegionLocation},
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
pub enum NodeType<N: LangNode + 'static> {
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

///A single node container in the RVSDG. Contains the most important inner `node_type`, as well as context information.
///
/// The most interesting here is the `parent` field, which lets you efficiently traverse child->parent relationships.
#[derive(Debug, Clone)]
pub struct Node<N: LangNode + 'static> {
    pub node_type: NodeType<N>,
    ///Declares in which nodes's region `self` is located.
    pub parent: Option<RegionLocation>,
}

impl<N: LangNode + 'static> Node<N> {
    pub fn inputs(&self) -> &[Input] {
        match &self.node_type {
            NodeType::Simple(node) => node.inputs(),
            NodeType::Gamma(g) => g.inputs(),
            NodeType::Theta(g) => &g.inputs(),
            NodeType::Lambda(g) => &g.inputs(),
            NodeType::Apply(g) => &g.inputs,
            NodeType::Delta(g) => &g.inputs(),
            NodeType::Phi(g) => &g.inputs(),
            NodeType::Omega(g) => &g.inputs(),
        }
    }

    pub fn inputs_mut(&mut self) -> &mut [Input] {
        match &mut self.node_type {
            NodeType::Simple(node) => node.inputs_mut(),
            NodeType::Gamma(g) => g.inputs_mut(),
            NodeType::Theta(g) => g.inputs_mut(),
            NodeType::Lambda(g) => g.inputs_mut(),
            NodeType::Apply(g) => &mut g.inputs,
            NodeType::Delta(g) => g.inputs_mut(),
            NodeType::Phi(g) => g.inputs_mut(),
            NodeType::Omega(_g) => &mut [],
        }
    }
    pub fn outputs(&self) -> &[Output] {
        match &self.node_type {
            NodeType::Simple(node) => node.outputs(),
            NodeType::Gamma(g) => g.outputs(),
            NodeType::Theta(g) => g.outputs(),
            NodeType::Lambda(g) => g.outputs(),
            NodeType::Apply(g) => &g.outputs,
            NodeType::Delta(g) => g.outputs(),
            NodeType::Phi(g) => g.outputs(),
            NodeType::Omega(_g) => &[],
        }
    }

    pub fn outputs_mut(&mut self) -> &mut [Output] {
        match &mut self.node_type {
            NodeType::Simple(node) => node.outputs_mut(),
            NodeType::Gamma(g) => g.outputs_mut(),
            NodeType::Theta(g) => g.outputs_mut(),
            NodeType::Lambda(g) => g.outputs_mut(),
            NodeType::Apply(g) => &mut g.outputs,
            NodeType::Delta(g) => g.outputs_mut(),
            NodeType::Phi(g) => g.outputs_mut(),
            NodeType::Omega(_g) => &mut [],
        }
    }

    ///Reference to all internal regions. This will mostly have length 0/1. Only gamma nodes have >1 regions.
    pub fn regions(&self) -> &[Region] {
        match &self.node_type {
            NodeType::Simple(_node) => &[],
            NodeType::Gamma(g) => g.regions(),
            NodeType::Theta(g) => g.regions(),
            NodeType::Lambda(g) => g.regions(),
            NodeType::Apply(_g) => &[],
            NodeType::Delta(g) => g.regions(),
            NodeType::Phi(g) => g.regions(),
            NodeType::Omega(g) => g.regions(),
        }
    }
    ///Reference to all internal regions. This will mostly have length 0/1. Only gamma nodes have >1 regions.
    pub fn regions_mut(&mut self) -> &mut [Region] {
        match &mut self.node_type {
            NodeType::Simple(_node) => &mut [],
            NodeType::Gamma(g) => g.regions_mut(),
            NodeType::Theta(g) => g.regions_mut(),
            NodeType::Lambda(g) => g.regions_mut(),
            NodeType::Apply(_g) => &mut [],
            NodeType::Delta(g) => g.regions_mut(),
            NodeType::Phi(g) => g.regions_mut(),
            NodeType::Omega(g) => g.regions_mut(),
        }
    }

    pub fn legalize(&mut self) -> Result<(), LegalizationError> {
        todo!("Implement legalization")
    }

    ///Tries to translate the `port` into a valid port.
    pub fn outport(&self, port_ty: &OutputType) -> Option<&Outport> {
        match port_ty {
            OutputType::Output(p) => self.outputs().get(*p),
            _ => match &self.node_type {
                NodeType::Simple(_) | NodeType::Apply(_) => None,
                NodeType::Theta(g) => g.outport(port_ty),
                NodeType::Lambda(g) => g.outport(port_ty),
                NodeType::Delta(g) => g.outport(port_ty),
                NodeType::Phi(g) => g.outport(port_ty),
                NodeType::Omega(g) => g.outport(port_ty),
                NodeType::Gamma(g) => g.outport(port_ty),
            },
        }
    }

    ///Tries to translate the `port` into a valid port.
    pub fn outport_mut(&mut self, port_ty: &OutputType) -> Option<&mut Outport> {
        match port_ty {
            OutputType::Output(p) => self.outputs_mut().get_mut(*p),
            _ => match &mut self.node_type {
                NodeType::Simple(_) | NodeType::Apply(_) => None,
                NodeType::Theta(g) => g.outport_mut(port_ty),
                NodeType::Lambda(g) => g.outport_mut(port_ty),
                NodeType::Delta(g) => g.outport_mut(port_ty),
                NodeType::Phi(g) => g.outport_mut(port_ty),
                NodeType::Omega(g) => g.outport_mut(port_ty),
                NodeType::Gamma(g) => g.outport_mut(port_ty),
            },
        }
    }

    pub fn inport(&self, port_ty: &InputType) -> Option<&Inport> {
        match port_ty {
            InputType::Input(i) => self.inputs().get(*i),
            _ => match &self.node_type {
                NodeType::Simple(_) | NodeType::Apply(_) => None,
                NodeType::Theta(g) => g.inport(port_ty),
                NodeType::Lambda(g) => g.inport(port_ty),
                NodeType::Delta(g) => g.inport(port_ty),
                NodeType::Phi(g) => g.inport(port_ty),
                NodeType::Omega(g) => g.inport(port_ty),
                NodeType::Gamma(g) => g.inport(port_ty),
            },
        }
    }

    pub fn inport_mut(&mut self, port_ty: &InputType) -> Option<&mut Inport> {
        match port_ty {
            InputType::Input(i) => self.inputs_mut().get_mut(*i),
            _ => match &mut self.node_type {
                NodeType::Simple(_) | NodeType::Apply(_) => None,
                NodeType::Theta(g) => g.inport_mut(port_ty),
                NodeType::Lambda(g) => g.inport_mut(port_ty),
                NodeType::Delta(g) => g.inport_mut(port_ty),
                NodeType::Phi(g) => g.inport_mut(port_ty),
                NodeType::Omega(g) => g.inport_mut(port_ty),
                NodeType::Gamma(g) => g.inport_mut(port_ty),
            },
        }
    }

    ///Returns true if this is either a lambda, or phi node
    pub fn is_callable(&self) -> bool {
        if let NodeType::Lambda(_) | NodeType::Phi(_) = self.node_type {
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
        match &self.node_type {
            NodeType::Apply(_a) => write!(f, "Apply"),
            NodeType::Delta(d) => {
                write!(f, "Delta({} cv, {} nodes)", d.cv_count, d.body.nodes.len())
            }
            NodeType::Gamma(g) => write!(
                f,
                "Gamma({} branches, {} entry-vars, {} exit-vars)",
                g.regions.len(),
                g.entry_var_count,
                g.exit_var_count
            ),
            NodeType::Lambda(l) => write!(
                f,
                "Lambda({} arguments, {} results)",
                l.body.arguments.len(),
                l.body.results.len()
            ),
            NodeType::Omega(o) => write!(
                f,
                "Omega({} imports, {} exports, {} top-level nodes)",
                o.body.arguments.len(),
                o.body.results.len(),
                o.body.nodes.len()
            ),
            NodeType::Phi(p) => write!(f, "Phi({} rv, {} cv)", p.rv_count, p.cv_count),
            NodeType::Simple(s) => write!(f, "Simple({:?})", s),
            NodeType::Theta(t) => write!(f, "Theta({} lv)", t.lv_count),
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
