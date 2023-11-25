use ahash::AHashSet;
use tinyvec::ArrayVec;

use crate::{
    builder::RegionBuilder,
    edge::{InportLocation, LangEdge, OutportLocation},
    err::BuilderError,
    nodes::LangNode,
    EdgeRef, NodeRef, Rvsdg,
};

///A Outport allows us to define multiple destination edges. It is the base type for [Output] and [Argument].
#[derive(Debug, Clone)]
pub struct Outport {
    pub edges: ArrayVec<[EdgeRef; 2]>,
}

impl Default for Outport {
    fn default() -> Self {
        Outport {
            edges: ArrayVec::default(),
        }
    }
}

pub type Output = Outport;
pub type Argument = Outport;

///A in port allows us to define at most a single src edge. It is the base type for [Input] and [RegResult].
#[derive(Debug, Clone)]
pub struct Inport {
    pub edge: Option<EdgeRef>,
}

impl Default for Inport {
    fn default() -> Self {
        Inport { edge: None }
    }
}
pub type Input = Inport;
pub type RegResult = Inport;

///Region as defined in 4.0 in the source paper.
///
/// A region R = (A, N, E, R) is characterised through a set of arguments A, its internal nodes N and edges E, and a result tuple R.
#[derive(Debug, Clone)]
pub struct Region {
    pub arguments: ArrayVec<[Argument; 3]>,
    pub results: ArrayVec<[RegResult; 3]>,
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

    ///Tries to connect `src` to `dst` within this context.
    pub fn connect<N: LangNode + 'static, E: LangEdge + 'static>(
        &mut self,
        parent: NodeRef,
        ctx: &mut Rvsdg<N, E>,
        src: OutportLocation,
        dst: InportLocation,
        edge_ty: E,
    ) -> Result<EdgeRef, BuilderError> {
        RegionBuilder::new(ctx, self, parent).connect(src, dst, edge_ty)
    }
}

impl Default for Region {
    fn default() -> Self {
        Region::new()
    }
}
