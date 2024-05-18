use slotmap::{new_key_type, SlotMap};

use crate::{edge::OutportLocation, NodeRef};

///CFG recovery via the _naive_ StructuredControlFlowRecovery described in section 3.4 _Perfect Reconstructability of Control Flow from Demand Dependence Graphs_
/// [here](https://dl.acm.org/doi/10.1145/2693261)
pub mod scfr;

pub enum CfgNode {
    CondBranch {
        ///The src gamma node this branch is based on
        src_node: NodeRef,
        ///The CfgNode the condition comes from
        condition: OutportLocation,
        ///Branch to _true_
        true_branch: CfgRef,
        ///Branch to _false_
        false_branch: CfgRef,
    },
    Branch {
        ///The γ or θ node this branch is based on.
        src_node: NodeRef,
        dst: CfgRef,
    },
    Null,
    Root(CfgRef),
    BasicBlock(BasicBlock),
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    ///Nodes this basic block is targeted by
    pub incoming_nodes: Vec<CfgRef>,
    ///The nodes, in (needed) order of execution.
    pub nodes: Vec<NodeRef>,
    ///The node the BB branches to after reaching its end.
    pub exit_node: CfgRef,
}

impl BasicBlock {
    ///Creates a new BB where the exit is hooked up to the error node of the cfg
    /// and no incoming nodes are set
    pub fn new(cfg: &mut Cfg) -> Self {
        BasicBlock {
            incoming_nodes: Vec::new(),
            nodes: Vec::new(),
            exit_node: cfg.error,
        }
    }
}

new_key_type! {
    ///Reference to some CfgNode in a Cfg
    pub struct CfgRef;
}

///Describes a generic SCFG over nodes [CfgNode]s that can be derived from any
///RVSDG region that only contains theta, gamma, apply and simple nodes of type `N`.
///
///The graph follows the formalization of [Formalizing Structured Control Flow Graphs](https://link.springer.com/chapter/10.1007/978-3-319-52709-3_13).
pub struct Cfg {
    pub nodes: SlotMap<CfgRef, CfgNode>,
    ///Error node of the cfg
    pub error: CfgRef,
    ///The root node of the graph, points to the first basic block
    pub root: CfgRef,
}

impl Cfg {
    pub fn new() -> Self {
        let mut nodes = SlotMap::with_key();
        let error = nodes.insert(CfgNode::Null);
        let root = nodes.insert(CfgNode::Root(error));
        Cfg { nodes, error, root }
    }
}
