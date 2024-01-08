use thiserror::Error;

use crate::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    EdgeRef, NodeRef,
};

#[derive(Error, Debug, Eq, PartialEq)]
pub enum LegalizationError {
    #[error("Detected a cycle")]
    CycleDetected,
}

///Errors that are returned when using the builder. Those are mostly recoverabel.
#[derive(Error, Debug, Clone)]
pub enum BuilderError {
    #[error("The Node {0} is not part of the region it is used in. consider importing it as argument or context variable.")]
    NodeNotInRegion(NodeRef),
    #[error("Overwrote connection {0} for input, this might not be desired.")]
    EdgeOverwrite(EdgeRef),
    #[error("No input {0:?} on node")]
    ExpectedInport(InportLocation),
    #[error("No output {0:?} on node")]
    ExpectedOutport(OutportLocation),
    #[error("{0}")]
    Other(String),
}

///Errors that happen when operating on the graph directly. For instance, when trying to delete an
/// invalid edge, accessing an non-existent node etc.
#[derive(Error, Debug, Clone)]
pub enum GraphError {
    #[error("The edge {0} is invalid. It might have been deleted since its creation.")]
    InvalidEdge(EdgeRef),
    #[error("The node {0} is invalid. It might have been deleted since its creation.")]
    InvalidNode(NodeRef),
    #[error("The region location {0:?} is invalid in this graph")]
    InvalidRegion(RegionLocation),

    #[error("Unexpected node type")]
    UnexpectedNodeType,
    #[error("Node {0} was not declared in a parent to region {1:?} or {1:?} itself")]
    NodeNotInParentRegion(NodeRef, RegionLocation),

    #[error("Node {0} has no parent region")]
    NoParentRegion(NodeRef),

    #[error("The definition of {0} is not a callable node.")]
    NotCallable(NodeRef),

    #[error("{0} is not connected to any region's output, or input")]
    NotConnectedInRegion(NodeRef),
    #[error("Source and destination port are not in the same region {src:?} != {dst:?}")]
    NodesNotInSameRegion {
        src: RegionLocation,
        dst: RegionLocation,
    },
    #[error("Edge {0} was not in any region")]
    EdgeNotInRegion(EdgeRef),

    #[error("{0:?} is already in use")]
    InportInUse(InportLocation),
    #[error("{0:?} does not exist (on node)")]
    InvalidInport(InportLocation),
    #[error("{0:?} does not exist (on node)")]
    InvalidOutport(OutportLocation),
    #[error("Expected result-like input type, got {0:?}")]
    ExpectedResult(InputType),
    #[error("Expected argument-like output type, got {0:?}")]
    ExpectedArgument(OutputType),
}
