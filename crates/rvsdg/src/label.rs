use crate::{edge::PortIndex, EdgeRef, NodeRef, RegionRef};

///Defines all possible locations for a label.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum LabelLoc {
    Port { node: NodeRef, port: PortIndex },
    Node(NodeRef),
    Edge(EdgeRef),
    Region(RegionRef),
    Custom(String),
}
