use rvsdg::{NodeRef, Rvsdg, edge::LangEdge, nodes::LangNode};

use crate::benefit::Benefit;

///Describes a pattern-rewrite to a driver.
pub trait PatternRewrite<N: LangNode, E: LangEdge, B: Benefit, Err> {
    ///Returns true if this pattern could be applied to `node`.
    fn matches(&self, node: NodeRef) -> bool;

    fn benefit(&self) -> &B;

    ///When applied, the
    fn apply(&self, graph: &mut Rvsdg<N, E>, node: NodeRef) -> Result<(), Err>;
}
