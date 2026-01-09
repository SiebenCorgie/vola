mod topo_greedy;
use rvsdg::{Rvsdg, edge::LangEdge, nodes::LangNode};
pub use topo_greedy::TopoGreedyRewriter;
mod canonicalizer;
pub use canonicalizer::Canonicalizer;

///Describes how recursion into sub-regions of a node should be handeled.
#[derive(Debug, PartialEq)]
pub enum DriverRecursion {
    ///First process the parent, before processing any remaining child-regions.
    TopDown,
    ///First process child regions befor processing the parent.
    BottomUp,
    ///Do not recurse at all
    None,
}

///Signals how often a driver can restart execution, if a region was changed.
#[derive(Debug, Clone)]
pub enum DriverRestart {
    ///Can restart as often as needed until nothing changes anymore.
    ///
    /// *Keep in mind that this can possibly lead to unlimited runtime*.
    ///
    /// However, on paper this produces the best results.
    Unbound,
    ///Can restart up to the given amount of times.
    Bound(usize),
    ///Runs once in every region.
    SingeShot,
}

impl DriverRestart {
    pub fn should_restart(&mut self) -> bool {
        match self {
            Self::Unbound => true,
            Self::Bound(bound) => {
                if let Some(new_count) = bound.checked_sub(1) {
                    //Update the bound to one-less
                    *bound = new_count;
                    true
                } else {
                    false
                }
            }
            Self::SingeShot => false,
        }
    }
}

///Allows you to use _anything_ that contains a graph
/// to drive pattern rewrites.
///
/// This is useful if you need/use context in pattern matching.
///
/// Note that any _standard_ RVSDG is by definition rewriteable.
pub trait RewriteableGraph {
    type Node: LangNode;
    type Edge: LangEdge;

    fn graph(&self) -> &Rvsdg<Self::Node, Self::Edge>;
    fn graph_mut(&mut self) -> &mut Rvsdg<Self::Node, Self::Edge>;
}

impl<N: LangNode, E: LangEdge> RewriteableGraph for Rvsdg<N, E> {
    type Edge = E;
    type Node = N;
    fn graph(&self) -> &Rvsdg<Self::Node, Self::Edge> {
        self
    }
    fn graph_mut(&mut self) -> &mut Rvsdg<Self::Node, Self::Edge> {
        self
    }
}
