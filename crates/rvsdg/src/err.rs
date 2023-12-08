use thiserror::Error;

use crate::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    EdgeRef, NodeRef,
};

#[derive(Error, Debug)]
pub enum LegalizationError {}

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

    #[error("Unexpected node type")]
    UnexpectedNodeType,
    #[error("Node {0} was not declared in a parent region to {1} or {1} itself")]
    NodeNotInParentRegion(NodeRef, NodeRef),

    #[error("The definition of {0} is not a callable node.")]
    NotCallable(NodeRef),

    #[error("{0} is not connected to any region's output, or input")]
    NotConnectedInRegion(NodeRef),
    #[error("Source and destination port are not in the same region {src}.region({src_reg_idx}) != {dst}.region({dst_reg_idx})")]
    NodesNotInSameRegion {
        src: NodeRef,
        src_reg_idx: usize,
        dst: NodeRef,
        dst_reg_idx: usize,
    },

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
