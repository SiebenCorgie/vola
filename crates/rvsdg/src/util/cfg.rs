/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! This module implements SCFG structurizer for the Rvsdg.
//! Each node in the resulting SCFG is describe by a [CfgNode] in a [Cfg] graph.
//!
//! At the moment only the [scfr](crate::Rvsdg::region_to_cfg_scfr) algorithm is implemented.

use ahash::AHashSet;
use scfr::ScfrError;
use slotmap::{new_key_type, Key, SlotMap};
use smallvec::{smallvec, SmallVec};

use crate::{edge::OutportLocation, NodeRef};

///CFG recovery via the _naive_ StructuredControlFlowRecovery described in section 3.4 _Perfect Reconstructability of Control Flow from Demand Dependence Graphs_
/// [here](https://dl.acm.org/doi/10.1145/2693261)
pub mod scfr;

#[derive(Debug)]
pub enum CfgNode {
    //Header _into_ a Theta nodes body.
    LoopHeader {
        src_node: NodeRef,
        //the last bb before that loop
        pre_loop_bb: CfgRef,
        //first bb of the loop
        loop_entry_bb: CfgRef,
        //The tail of the loop, so the back-edge source
        ctrl_tail: CfgRef,
    },
    ///The control tail of the loop.
    LoopCtrlTail {
        ///The last bb of the loop's body
        last_bb: CfgRef,
        ///The loop_entry bb
        loop_entry_bb: CfgRef,
        ///The loop exit bb, aka. the _after the loop_ merge
        post_loop_bb: CfgRef,
        ///Condition's output port, so the thing based on which we need to
        /// setup the conditional jump / branch.
        condition_src: OutportLocation,
        ///The theta node this ctrl-tail belongs to.
        src_node: NodeRef,
        ///id of the loop header
        header: CfgRef,
    },

    BranchHeader {
        ///The gamma node this header belongs to
        src_node: NodeRef,
        condition_src: OutportLocation,
        last_bb: CfgRef,
        ///Branch to _true_
        true_branch: CfgRef,
        ///Branch to _false_
        false_branch: CfgRef,
        ///The BranchMerge node at which true and false meet
        merge: CfgRef,
        ///The basic block that follows the merge
        post_merge_block: CfgRef,
    },
    BranchMerge {
        src_node: NodeRef,
        //last bb of the true branch
        src_true: CfgRef,
        //last bb of the false branch
        src_false: CfgRef,
        ///next node after merging the if-then-else
        next: CfgRef,
    },
    Null,
    Root(CfgRef),
    BasicBlock(BasicBlock),
}

impl CfgNode {
    ///Returns all successors. Does not include the backedge from a LoopCtrlTail to the
    /// LoopHead.
    pub fn successors(&self) -> SmallVec<[CfgRef; 2]> {
        match self {
            Self::BasicBlock(BasicBlock { exit_node, .. }) => smallvec![*exit_node],
            Self::BranchHeader {
                true_branch,
                false_branch,
                ..
            } => smallvec![*true_branch, *false_branch],
            Self::BranchMerge { next, .. } => smallvec![*next],
            Self::LoopHeader { loop_entry_bb, .. } => smallvec![*loop_entry_bb],
            Self::LoopCtrlTail { post_loop_bb, .. } => smallvec![*post_loop_bb],
            Self::Null => smallvec![],
            Self::Root(next) => smallvec![*next],
        }
    }

    pub fn predecessors(&self) -> SmallVec<[CfgRef; 2]> {
        match self {
            Self::BasicBlock(bb) => smallvec![bb.predecessor],
            Self::BranchHeader { last_bb, .. } => smallvec![*last_bb],
            Self::BranchMerge {
                src_true,
                src_false,
                ..
            } => smallvec![*src_true, *src_false],
            Self::LoopCtrlTail { last_bb, .. } => smallvec![*last_bb],
            Self::LoopHeader { pre_loop_bb, .. } => smallvec![*pre_loop_bb],
            Self::Null => smallvec![],
            Self::Root(_) => smallvec![],
        }
    }
}

#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub predecessor: CfgRef,
    ///The nodes, in (needed) order of execution.
    pub nodes: Vec<NodeRef>,
    ///The node the BB branches to after reaching its end.
    pub exit_node: CfgRef,
}

impl BasicBlock {
    ///Creates a new BB where the exit is hooked up to the error node of the cfg
    /// and no incoming nodes are set
    pub fn new(cfg: &mut Cfg, predecessor: CfgRef) -> Self {
        BasicBlock {
            predecessor,
            nodes: Vec::new(),
            exit_node: cfg.error,
        }
    }
}

new_key_type! {
    ///Reference to some CfgNode in a Cfg
    pub struct CfgRef;
}

impl CfgRef {
    pub fn ffi(&self) -> u64 {
        self.data().as_ffi()
    }
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

    ///Builds a topological order of CfgRefs over the Cfg that
    /// touches all nodes in a way, that any dominator A of B is seen before
    /// B.
    pub fn topological_order(&self) -> Result<Vec<CfgRef>, ScfrError> {
        //NOTE: this is basically Khan's algorithm on the
        //      cfg graph.

        let mut seen = AHashSet::with_capacity(self.nodes.len());
        let mut l = Vec::with_capacity(self.nodes.len());
        let mut s = Vec::new();
        //NOTE: Khan's algorithm starts with _all nodes with no
        //      incoming edges_. However, we always only
        //      have the root node.
        s.push(self.root);

        while let Some(next) = s.pop() {
            l.push(next);
            seen.insert(next);

            for succ in self.nodes[next].successors() {
                let succpreds = self.nodes[succ].predecessors();
                let mut seen_all = true;
                for sp in succpreds {
                    if !seen.contains(&sp) {
                        seen_all = false;
                        break;
                    }
                }

                if seen_all {
                    s.push(succ);
                }
            }
        }

        if seen.len() != self.nodes.len() {
            return Err(ScfrError::TopoOrdNotAllNodesTraversed(l));
        } else {
            Ok(l)
        }
    }
}
