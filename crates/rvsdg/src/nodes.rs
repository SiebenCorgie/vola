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

use tinyvec::ArrayVec;

use crate::{
    edge::{LangEdge, PortIndex},
    err::LegalizationError,
    region::Port,
    RegionRef, Rvsdg,
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
    pub fn outputs(&self) -> &[Port] {
        match &self {
            Node::Simple(node) => node.outputs(),
            Node::Gamma(g) => &g.outputs,
            Node::Theta(g) => &g.outputs,
            Node::Lambda(g) => core::slice::from_ref(&g.output),
            Node::Apply(g) => &g.outputs,
            Node::Delta(g) => core::slice::from_ref(&g.output),
            Node::Phi(g) => &g.outputs,
            Node::Omega(g) => &g.outputs,
            Node::Invalid => &[],
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
    pub(crate) entry_var_count: usize,
    pub(crate) exit_var_count: usize,
    pub(crate) regions: ArrayVec<[RegionRef; 3]>,
    pub(crate) inputs: ArrayVec<[Port; 3]>,
    pub(crate) outputs: ArrayVec<[Port; 3]>,
}

impl GammaNode {
    pub fn new() -> Self {
        GammaNode {
            entry_var_count: 0,
            exit_var_count: 0,
            regions: ArrayVec::default(),
            inputs: ArrayVec::default(),
            outputs: ArrayVec::default(),
        }
    }

    ///Adds a new port for an entry variable.
    pub fn add_entry_var<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        let idx = self.entry_var_count;
        self.entry_var_count += 1;
        //NOTE: offset by one, since the first is the criteria
        self.inputs.insert(idx + 1, Port::default());

        //now add at same location to arguments of inner blocks
        for b in self.regions {
            ctx.on_region_mut(b, |r| r.arguments.insert(idx, Port::default()));
        }

        idx
    }

    ///Adds a new port for an exit variable.
    pub fn add_exit_var<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        let idx = self.exit_var_count;
        self.exit_var_count += 1;
        self.outputs.insert(idx, Port::default());

        //now add at same location to arguments of inner blocks
        for b in self.regions {
            ctx.on_region_mut(b, |r| r.results.insert(idx, Port::default()));
        }

        idx
    }

    ///Adds a new decision branch / region.
    pub fn add_region<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        let new_region = ctx.new_region();
        let arg_count = self.inputs.len();
        let res_count = self.outputs.len();

        //Setup region with same input/output count thats already valid
        ctx.on_region_mut(new_region, |r| {
            for _ in 0..arg_count {
                r.arguments.push(Port::default());
            }
            for _ in 0..res_count {
                r.results.push(Port::default());
            }
        });

        self.regions.push(new_region);
        self.regions.len() - 1
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
pub struct ThetaNode {
    pub(crate) lv_count: usize,
    pub(crate) loop_body: RegionRef,
    pub(crate) inputs: ArrayVec<[Port; 3]>,
    pub(crate) outputs: ArrayVec<[Port; 3]>,
}

impl ThetaNode {
    pub fn new<N: LangNode + 'static, E: LangEdge + 'static>(ctx: &mut Rvsdg<N, E>) -> Self {
        let loop_body = ctx.new_region();
        //add the loop criteria
        ctx.on_region_mut(loop_body, |b| b.results.insert(0, Port::default()));
        ThetaNode {
            lv_count: 0,
            loop_body,
            inputs: ArrayVec::default(),
            outputs: ArrayVec::default(),
        }
    }

    pub fn add_loop_variable<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        let lvidx = self.lv_count;
        self.lv_count += 1;
        self.inputs.insert(lvidx, Port::default());
        self.outputs.insert(lvidx, Port::default());
        ctx.on_region_mut(self.loop_body, |r| {
            r.arguments.insert(lvidx, Port::default());
            //offset by 1, since the first is the criteria.
            r.results.insert(lvidx + 1, Port::default());
        });

        lvidx
    }

    //TODO do we want to allow additional inputs/outputs?
}

///Related to the [λ-Node](LambdaNode). Represents a call of some function. The first port is defined as the `caller`, which must be connected to a
/// [LambdaNode].
pub struct ApplyNode {
    ///Function being called, must be a edge to a lambdaNode
    pub(crate) inputs: ArrayVec<[Port; 3]>,
    pub(crate) outputs: ArrayVec<[Port; 3]>,
}

impl ApplyNode {
    pub fn new() -> Self {
        let mut inputs = ArrayVec::default();
        //The function input to apply
        inputs.insert(0, Port::default());
        ApplyNode {
            inputs,
            outputs: ArrayVec::default(),
        }
    }

    pub fn add_input(&mut self) -> usize {
        self.inputs.push(Port::default());
        self.inputs.len() - 1
    }

    pub fn add_output(&mut self) -> usize {
        self.outputs.push(Port::default());
        self.outputs.len() - 1
    }
}
pub type FunctionNode = LambdaNode;
///λ-Node represents a function, characterised by an internal [Region](crate::region::Region) representing the function's body.
/// The node has `n` inputs, and a single output. The single output represents the function **not** the function's output.
///
/// The output is mapped at call time (represented by the [ApplyNode]) to the output of the calling node.
///
/// A function is called via an [ApplyNode], where the function being called (callee), is an argument to the [ApplyNode] (referred to as Caller).
pub struct LambdaNode {
    pub(crate) cv_count: usize,
    pub(crate) inputs: ArrayVec<[Port; 3]>,
    pub(crate) output: Port,
    pub(crate) body: RegionRef,
}

impl LambdaNode {
    pub fn new<N: LangNode + 'static, E: LangEdge + 'static>(ctx: &mut Rvsdg<N, E>) -> Self {
        LambdaNode {
            cv_count: 0,
            inputs: ArrayVec::default(),
            output: Port::default(),
            body: ctx.new_region(),
        }
    }

    pub fn add_context_variable<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        let idx = self.cv_count;
        self.cv_count += 1;
        self.inputs.insert(idx, Port::default());
        ctx.on_region_mut(self.body, |r| r.arguments.insert(idx, Port::default()));

        idx
    }

    pub fn add_input<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        self.inputs.push(Port::default());
        let pushed_to = self.inputs.len();
        ctx.on_region_mut(self.body, |r| {
            r.arguments.push(Port::default());
            assert!(
                r.arguments.len() == pushed_to,
                "Detected invalid LambdaNode state, input and argument-count don't match"
            );
        });

        pushed_to - self.cv_count - 1
    }

    ///Adds a result to the function. Note that this does NOT mean the output of this lambda function, but adding
    /// a result to the later evaluated procedure of the body.
    pub fn add_result<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        ctx.on_region_mut(self.body, |r| {
            r.results.push(Port::default());
            r.results.len() - 1
        })
    }
}

pub type GlobalVariable = DeltaNode;

/// δ-Nodes represents a global variable. It can have a set of inputs needed to define the variable. Similarly to [λ-Node](LambdaNode)s
/// context-variables can import inner, dependent state, like λ-Nodes or other arguments.
///
/// A δ-node must always provide a single output.
pub struct DeltaNode {
    pub(crate) cv_count: usize,
    pub(crate) inputs: ArrayVec<[Port; 3]>,
    pub(crate) body: RegionRef,
    pub(crate) output: Port,
}

impl DeltaNode {
    pub fn new<N: LangNode + 'static, E: LangEdge + 'static>(ctx: &mut Rvsdg<N, E>) -> Self {
        DeltaNode {
            cv_count: 0,
            inputs: ArrayVec::default(),
            body: ctx.new_region(),
            output: Port::default(),
        }
    }
    pub fn add_context_variable<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        let idx = self.cv_count;
        self.cv_count += 1;
        self.inputs.insert(idx, Port::default());
        ctx.on_region_mut(self.body, |r| r.arguments.insert(idx, Port::default()));

        idx
    }

    pub fn add_input<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        self.inputs.push(Port::default());
        let pushed_to = self.inputs.len();
        ctx.on_region_mut(self.body, |r| {
            r.arguments.push(Port::default());
            assert!(
                r.arguments.len() == pushed_to,
                "Detected invalid LambdaNode state, input and argument-count don't match"
            );
        });

        pushed_to - self.cv_count - 1
    }
}

pub type RecursionNode = PhiNode;

///ϕ-Nodes represent a environment for mutual recursion, i.e `fn f(){... return f();}`.
///
///
pub struct PhiNode {
    pub(crate) cv_count: usize,
    pub(crate) rv_count: usize,
    pub(crate) body: RegionRef,
    pub(crate) outputs: ArrayVec<[Port; 3]>,
    pub(crate) inputs: ArrayVec<[Port; 3]>,
}

impl PhiNode {
    pub fn new<N: LangNode + 'static, E: LangEdge + 'static>(ctx: &mut Rvsdg<N, E>) -> Self {
        PhiNode {
            cv_count: 0,
            rv_count: 0,
            inputs: ArrayVec::default(),
            body: ctx.new_region(),
            outputs: ArrayVec::default(),
        }
    }
    pub fn add_context_variable<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        let idx = self.cv_count;
        self.cv_count += 1;
        self.inputs.insert(idx, Port::default());
        ctx.on_region_mut(self.body, |r| r.arguments.insert(idx, Port::default()));

        idx
    }

    ///Adds new recursion port. Returns the port index for that recursion variable.
    pub fn add_recursion_variable<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        let arg_idx = self.cv_count + self.rv_count;
        let res_idx = self.rv_count;
        self.rv_count += 1;
        ctx.on_region_mut(self.body, |r| {
            r.arguments.insert(arg_idx, Port::default());
            r.results.insert(res_idx, Port::default());
        });
        self.outputs.insert(res_idx, Port::default());

        res_idx
    }

    pub fn add_input<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        self.inputs.push(Port::default());
        let pushed_to = self.inputs.len();
        ctx.on_region_mut(self.body, |r| {
            r.arguments.push(Port::default());
            assert!(
                r.arguments.len() == pushed_to,
                "Detected invalid LambdaNode state, input and argument-count don't match"
            );
        });

        pushed_to - self.cv_count - 1
    }
}

pub type TranslationUnit = OmegaNode;

///ω-node models a translation unit. It therefore has no inputs or outputs. It contains exactly one region, which in/outputs model
/// external dependencies to the translation unit.
pub struct OmegaNode {
    pub(crate) body: RegionRef,
    pub(crate) inputs: ArrayVec<[Port; 3]>,
    pub(crate) outputs: ArrayVec<[Port; 3]>,
}
impl OmegaNode {
    pub fn new<N: LangNode + 'static, E: LangEdge + 'static>(ctx: &mut Rvsdg<N, E>) -> Self {
        OmegaNode {
            inputs: ArrayVec::default(),
            body: ctx.new_region(),
            outputs: ArrayVec::default(),
        }
    }

    pub fn add_input<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        self.inputs.push(Port::default());
        let pushed_to = self.inputs.len();
        ctx.on_region_mut(self.body, |r| {
            r.arguments.push(Port::default());
            assert!(
                r.arguments.len() == pushed_to,
                "Detected invalid OmegaNode state, input and argument-count don't match"
            );
        });

        pushed_to - 1
    }

    pub fn add_output<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        ctx: &mut Rvsdg<N, E>,
    ) -> usize {
        self.outputs.push(Port::default());
        let pushed_to = self.outputs.len();
        ctx.on_region_mut(self.body, |r| {
            r.results.push(Port::default());
            assert!(
                r.results.len() == pushed_to,
                "Detected invalid Omega state, output and result-count don't match"
            );
        });

        pushed_to - 1
    }
}
