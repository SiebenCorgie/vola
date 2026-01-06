use rvsdg::{NodeRef, Rvsdg, edge::LangEdge, nodes::LangNode};

use crate::benefit::Benefit;

///Describes a pattern-rewrite to a driver.
pub trait PatternRewrite<N: LangNode, E: LangEdge, B: Benefit> {
    ///Returns true if this pattern could be applied to `node`.
    fn matches(&self, graph: &Rvsdg<N, E>, node: NodeRef) -> bool;

    fn benefit(&self) -> &B;

    ///Can assume that `node` fulfills the [matches](Self::matches) criterion. `graph` is shared to this application of a rewrite _exclusively_.
    /// Generally, if `matches` returns true, this is expected to succeed. Otherwise its free to panic, which indicates a pattern bug.
    fn apply(&self, graph: &mut Rvsdg<N, E>, node: NodeRef);
}
