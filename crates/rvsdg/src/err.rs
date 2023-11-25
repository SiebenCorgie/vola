use thiserror::Error;

use crate::{EdgeRef, NodeRef};

#[derive(Error, Debug)]
pub enum LegalizationError {}

///Errors that are returned when using the builder. Those are mostly recoverabel.
#[derive(Error, Debug)]
pub enum BuilderError {
    #[error("The Node {0} is not part of the region it is used in. consider importing it as argument or context variable.")]
    NodeNotInRegion(NodeRef),
    #[error("Overwrote connection {0} for input, this might not be desired.")]
    EdgeOverwrite(EdgeRef),
}

///Errors that happen when operating on the graph directly. For instance, when trying to delete an
/// invalid edge, accessing an non-existent node etc.
#[derive(Error, Debug)]
pub enum GraphError {
    #[error("The edge {0} is invalid. It might have been deleted since its creation.")]
    InvalidEdge(EdgeRef),
    #[error("The node {0} is invalid. It might have been deleted since its creation.")]
    InvalidNode(NodeRef),

    #[error("The definition of {0} is not a callable node.")]
    NotCallable(NodeRef),
}
