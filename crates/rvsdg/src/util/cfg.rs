use slotmap::SlotMap;

use crate::nodes::LangNode;

///CFG recovery via the _naive_ StructuredControlFlowRecovery described in section 3.4 _Perfect Reconstructability of Control Flow from Demand Dependence Graphs_
/// [here](https://dl.acm.org/doi/10.1145/2693261)
pub mod scfr;

pub enum CfgNode<N> {
    Branch {
        condition: CfgRef,
        true_branch: CfgRef,
        false_branch: CfgRef,
    },
    BasicBlock(),
}

///Reference to some CfgNode in a Cfg
new_key_type! {pub struct CfgRef;}

///Describes a generic CFG over nodes `N` that can be derived from any
///RVSDG of type `Rvsdg<N, _>`
pub struct Cfg<N: LangNode> {
    nodes: SlotMap,
}
