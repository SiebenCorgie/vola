use std::marker::PhantomData;

use ahash::AHashSet;
use tinyvec::ArrayVec;

use crate::{EdgeRef, NodeRef};

///A port allows us to partially define a edge. They are used to represent partially setup
/// interprocedural nodes (All nodes except _simple_ nodes).
pub struct Port {
    ///The edge reference for this node
    pub edge: Option<EdgeRef>,
}

impl Default for Port {
    fn default() -> Self {
        Port { edge: None }
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
        Port { edge: value.edge }
    }
}

///Region as defined in 4.0 in the source paper.
///
/// A region R = (A, N, E, R) is characterized through a set of arguments A, its internal nodes N and edges E, and a result tuple R.
pub struct Region {
    pub arguments: ArrayVec<[Port; 3]>,
    pub results: ArrayVec<[Port; 3]>,
    pub nodes: AHashSet<NodeRef>,
    pub edges: AHashSet<EdgeRef>,
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
