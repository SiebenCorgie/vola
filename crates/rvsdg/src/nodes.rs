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
use std::ops::Index;

use tinyvec::ArrayVec;

use crate::{
    edge::PortIndex,
    err::LegalizationError,
    region::{Port, TypedPort},
    RegionRef,
};

///simple node of a language. The trait lets us embed such a node in our overall RVSDG graph.
///
/// For the RVSDG the in and out edges are the most interesting aspects.
pub trait LangNode {
    fn inputs(&self) -> &[Port];
    fn inputs_mut(&mut self) -> &mut [Port];
    fn outputs(&self) -> &[Port];
    fn outputs_mut(&mut self) -> &mut [Port];
}
///Different node types as outlined in section 4. of the RVSDG paper. The
/// _Simple_ node represent your IR's instruction. All other nodes are RVSDG
/// specific architectural nodes.
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
    ///All registered inputs of a node. This means `input` in the original sense, which excludes context_variables and recursion_variables.
    pub fn inputs(&self) -> &[Port] {
        match &self {
            Node::Simple(node) => node.inputs(),
            Node::Gamma(g) => &g.inputs,
            Node::Theta(g) => &g.inputs,
            Node::Lambda(g) => &g.inputs,
            Node::Apply(g) => &g.outputs,
            Node::Delta(g) => &g.inputs,
            Node::Phi(g) => &g.inputs,
            Node::Omega(g) => &g.inputs,
            Node::Invalid => &[],
        }
    }

    ///All registered inputs of a node. This means `input` in the original sense, which excludes context_variables and recursion_variables.
    pub fn inputs_mut(&mut self) -> &mut [Port] {
        match self {
            Node::Simple(node) => node.inputs_mut(),
            Node::Gamma(g) => &mut g.inputs,
            Node::Theta(g) => &mut g.inputs,
            Node::Lambda(g) => &mut g.inputs,
            Node::Apply(g) => &mut g.outputs,
            Node::Delta(g) => &mut g.inputs,
            Node::Phi(g) => &mut g.inputs,
            Node::Omega(g) => &mut g.inputs,
            Node::Invalid => &mut [],
        }
    }

    ///All registered outputs of a node. This means `outputs` in the original sense, which excludes recursion_variables.
    pub fn outputs(&self) -> &[Port] {
        match &self {
            Node::Simple(node) => node.outputs(),
            Node::Gamma(g) => &g.outputs,
            Node::Theta(g) => &g.outputs,
            Node::Lambda(g) => core::slice::from_ref(&g.output),
            Node::Apply(g) => &g.outputs,
            Node::Delta(g) => core::slice::from_ref(&g.output),
            Node::Phi(g) => core::slice::from_ref(&g.output),
            Node::Omega(g) => &g.outputs,
            Node::Invalid => &[],
        }
    }

    ///All registered outputs of a node. This means `outputs` in the original sense, which excludes recursion_variables.
    pub fn outputs_mut(&mut self) -> &mut [Port] {
        match self {
            Node::Simple(node) => node.outputs_mut(),
            Node::Gamma(g) => &mut g.outputs,
            Node::Theta(g) => &mut g.outputs,
            Node::Lambda(g) => core::slice::from_mut(&mut g.output),
            Node::Apply(g) => &mut g.outputs,
            Node::Delta(g) => core::slice::from_mut(&mut g.output),
            Node::Phi(g) => core::slice::from_mut(&mut g.output),
            Node::Omega(g) => &mut g.outputs,
            Node::Invalid => &mut [],
        }
    }

    ///Reference to all internal regions. This will mostly have length 0/1. Only gamma nodes have >1 regions.
    pub fn regions(&self) -> &[RegionRef] {
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
    pub fn regions_mut(&mut self) -> &mut [RegionRef] {
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
}

pub type DecisionNode = GammaNode;

///The γ-node is a n-way decision node. This maps to the concept of C's if-then-else or switch-case.
/// It has at least a predicate edge that maps to 0..n, as well as n regions.
///
/// All regions must have the same signature as this γ-node (excluding the predicate).
pub struct GammaNode {
    ///Consumer of the switch predicate.
    pub predicate: Port,
    pub regions: ArrayVec<[RegionRef; 3]>,
    pub inputs: ArrayVec<[Port; 3]>,
    pub outputs: ArrayVec<[Port; 3]>,
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
pub struct ThetaNode {
    pub loop_body: RegionRef,
    pub inputs: ArrayVec<[Port; 3]>,
    pub outputs: ArrayVec<[Port; 3]>,
}

///Related to the [λ-Node](LambdaNode). Represents a call of some function.
pub struct ApplyNode {
    ///Function being called, must be a edge to a lambdaNode
    pub callee: TypedPort<LambdaNode>,
    pub inputs: ArrayVec<[Port; 3]>,
    pub outputs: ArrayVec<[Port; 3]>,
}

pub type FunctionNode = LambdaNode;
///λ-Node represents a function, characterised by an internal [Region](crate::region::Region) representing the function's body.
/// The node has `n` inputs, and a single output. The single output represents the function **not** the function's output.
///
/// The output is mapped at call time (represented by the [ApplyNode]) to the output of the calling node.
///
/// A function is called via an [ApplyNode], where the function being called (callee), is an argument to the [ApplyNode] (referred to as Caller).
pub struct LambdaNode {
    pub inputs: ArrayVec<[Port; 3]>,
    pub output: Port,
    pub body: RegionRef,
    ///ContextVariables basically represent functions and arguments that are used _internally_ in this λ-node/function.
    /// For instance consider this code:
    /// ```rust
    /// fn max(a, b) -> int{...}
    ///
    /// fn f(a, b) -> int {
    ///     println("max");
    ///     return max(a, b);
    /// }
    /// ```
    ///
    /// In this case we need to _know_ that we import `max` from our _translation unit_, but we need to import `println`
    /// from an external source. We also need to known that we depend on the global value `"max"`.
    ///
    /// For a better/more visual explanation, see Figure 3.a in the source paper.
    pub context_variables: ArrayVec<[Port; 3]>,
}

pub type GlobalVariable = DeltaNode;

/// δ-Nodes represents a global variable. It can have a set of inputs needed to define the variable. Similarly to [λ-Node](LambdaNode)s
/// context-variables can import inner, dependent state, like λ-Nodes or other arguments.
///
/// A δ-node must always provide a single output.
pub struct DeltaNode {
    pub inputs: ArrayVec<[Port; 3]>,
    /// See [LambdaNode]'s `context_variable` documentation, as well as Figure 3.a in the source paper.
    pub context_variables: ArrayVec<[Port; 3]>,
    pub body: RegionRef,
    pub output: Port,
}

pub type RecursionNode = PhiNode;

///ϕ-Nodes represent a environment for mutual recursion, i.e `fn f(){... return f();}`.
///
///
pub struct PhiNode {
    pub body: RegionRef,
    ///Context variables for λ-nodes in the φ-Node. Basically pass through of pre-defined functions, that are called from within the recursion.
    pub context_variable: ArrayVec<[Port; 3]>,
    ///Represent the argument stream to a recursion, as well as the output of the recursion.
    ///
    /// Consider the following:
    /// ```rust
    /// fn f(a: int) -> int{
    ///     if a <= 1{ return; }
    ///     return f(a - 1) + f(a - 2);
    /// }
    /// ```
    ///
    /// Then the recursion variable is made out of `a` as well as the result of `f()`.
    pub recursion_variables: ArrayVec<[(Port, Port); 2]>,

    pub output: Port,
    pub inputs: ArrayVec<[Port; 3]>,
}

pub type TranslationUnit = OmegaNode;

///ω-node models a translation unit. It therefore has no inputs or outputs. It contains exactly one region, which in/outputs model
/// external dependencies to the translation unit.
pub struct OmegaNode {
    pub body: RegionRef,
    pub inputs: ArrayVec<[Port; 3]>,
    pub outputs: ArrayVec<[Port; 3]>,
}
