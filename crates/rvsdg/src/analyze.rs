//! RVSDG analyzis tools
//!
//! Provides several tools to analyse an RVSDG. Including walker utilities
//! that make gathering information easy, and finding dependencies or definitions of nodes.

use crate::{
    edge::{LangEdge, PortIndex, PortLocation},
    nodes::{LangNode, Node},
    NodeRef, Rvsdg,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Tries to find a callable definition at the end of this (src, port) combination. The helper lets you trace over
    /// inter-procedural node boundaries easily. It stops whenever it reaches a node-output that can be called by an apply node,
    /// returning the reference to the node, if that node is an lambda or phi node.
    ///
    /// If you get `Some(node)`, you'd be safe to unwrap into a lambda node.
    pub fn find_callabel_def(&self, src: NodeRef, port: PortIndex) -> Option<NodeRef> {
        let mut node = src;
        let mut port = port;
        loop {
            //Get the resolved location
            let location = if let Some(loc) = port.into_location(self.node(node)) {
                loc
            } else {
                //Happens at invalid port mapping.
                return None;
            };
            //If this is a output port, make sure that its the first, and this node is a lambda node
            if let PortLocation::Outputs(_o) = location {
                match (location, self.node(node)) {
                    (PortLocation::Outputs(0), Node::Lambda(_)) => return Some(node),
                    (PortLocation::Outputs(0), Node::Phi(_)) => return Some(node),
                    //Is an output, but not callable
                    _ => return None,
                }
            } else {
                //Port is not an output, check if its an context variable argument. In that case we can map to an context-variable
                // input and advance the 'search loop in the parent.
                if let PortIndex::ContextVar {
                    var_index,
                    tuple_index: 1,
                } = port
                {
                    if let Some(cv_input) = self.port(
                        node,
                        PortIndex::ContextVar {
                            var_index,
                            tuple_index: 0,
                        },
                    ) {
                        //If the Context variable has an valid input edge, update node and port, otherwise, end
                        assert!(cv_input.edges.len() <= 1, "InputPort cannot have >1 inputs");
                        if let Some(edge) = cv_input.edges.get(0) {
                            node = self.edge(*edge).src;
                            port = self.edge(*edge).src_index.clone();
                        } else {
                            return None;
                        }
                    }
                } else {
                    //Is not a context var, therefore not valid.
                    return None;
                }
            }
        }
    }
}
