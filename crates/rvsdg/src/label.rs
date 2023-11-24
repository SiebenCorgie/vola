use crate::{edge::PortIndex, EdgeRef, NodeRef};

///Defines all possible locations for a label.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum LabelLoc {
    Port { node: NodeRef, port: PortIndex },
    Node(NodeRef),
    Edge(EdgeRef),
    Custom(String),
}
