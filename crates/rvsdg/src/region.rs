use std::marker::PhantomData;

use ahash::AHashSet;
use tinyvec::ArrayVec;

use crate::{EdgeRef, NodeRef};

pub struct Port(pub Option<EdgeRef>);

impl Default for Port {
    fn default() -> Self {
        Port(None)
    }
}

///A port where the type of the EdgeRef must be `E`.
pub struct TypedPort<E> {
    edge_type: PhantomData<E>,
    pub edge: Option<EdgeRef>,
}

impl<E> Default for TypedPort<E> {
    fn default() -> Self {
        TypedPort {
            edge_type: PhantomData,
            edge: None,
        }
    }
}

impl<E> From<TypedPort<E>> for Port {
    fn from(value: TypedPort<E>) -> Self {
        Port(value.edge)
    }
}

///Region as defined in 4.0 in the source paper.
///
/// A region R = (A, N, E, R) is characterized through a set of arguments A, its internal nodes N and edges E, and a result tuple R.
pub struct Region {
    arguments: ArrayVec<[Port; 3]>,
    results: ArrayVec<[Port; 3]>,
    nodes: AHashSet<NodeRef>,
    edges: AHashSet<EdgeRef>,
}

impl Region {
    pub fn new() -> Self {
        Region {
            nodes: AHashSet::default(),
            edges: AHashSet::default(),
            arguments: ArrayVec::default(),
            results: ArrayVec::default(),
        }
    }
}
