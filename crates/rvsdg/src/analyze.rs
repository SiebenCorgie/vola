//! RVSDG analyzis tools
//!
//! Provides several tools to analyse an RVSDG. Including walker utilities
//! that make gathering information easy, and finding dependencies or definitions of nodes.

use crate::{
    edge::{LangEdge, OutportLocation},
    nodes::LangNode,
    NodeRef, Rvsdg,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Tries to find a callable definition at the end of this (src, port) combination. The helper lets you trace over
    /// inter-procedural node boundaries easily. It stops whenever it reaches a node-output that can be called by an apply node,
    /// returning the reference to the node, if that node is an lambda or phi node.
    ///
    /// If you get `Some(node)`, you'd be safe to unwrap into a lambda node.
    pub fn find_callabel_def(&self, src: OutportLocation) -> Option<NodeRef> {
        todo!("Reimplement calle find")
    }
}
