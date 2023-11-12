use std::marker::PhantomData;

use tinyvec::ArrayVec;

use crate::EdgeRef;

pub struct Port(pub Option<EdgeRef>);

///A port where the type of the EdgeRef must be `E`.
pub struct TypedPort<E> {
    edge_type: PhantomData<E>,
    pub edge: Option<EdgeRef>,
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
}
