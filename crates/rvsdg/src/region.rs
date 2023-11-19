use std::marker::PhantomData;

use ahash::AHashSet;
use tinyvec::ArrayVec;

use crate::{EdgeRef, NodeRef};

///A port allows us to partially define a edge. They are used to represent partially setup
/// interprocedural nodes (All nodes except _simple_ nodes).
#[derive(Debug, Clone)]
pub struct Port {
    pub edges: ArrayVec<[EdgeRef; 2]>,
}

impl Default for Port {
    fn default() -> Self {
        Port {
            edges: ArrayVec::default(),
        }
    }
}

///Region as defined in 4.0 in the source paper.
///
/// A region R = (A, N, E, R) is characterised through a set of arguments A, its internal nodes N and edges E, and a result tuple R.
#[derive(Debug, Clone)]
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
