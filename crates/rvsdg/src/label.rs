use crate::{
    edge::{InportLocation, OutportLocation},
    EdgeRef, NodeRef,
};

///Defines all possible locations for a label.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum LabelLoc {
    InPort(InportLocation),
    OutPort(OutportLocation),
    Node(NodeRef),
    Edge(EdgeRef),
    Custom(String),
}
