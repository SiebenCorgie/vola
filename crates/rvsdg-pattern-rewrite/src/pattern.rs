use rvsdg::{NodeRef, edge::LangEdge, nodes::LangNode};

use crate::{benefit::Benefit, driver::RewriteableGraph};

///Describes a pattern-rewrite to a driver.
pub trait PatternRewrite<
    N: LangNode + 'static,
    E: LangEdge + 'static,
    Ctx: RewriteableGraph<Node = N, Edge = E>,
    B: Benefit,
>
{
    ///Returns true if this pattern could be applied to `node`.
    fn matches(&self, ctx: &Ctx, node: NodeRef) -> bool;

    fn benefit(&self) -> &B;

    ///Can assume that `node` fulfills the [matches](Self::matches) criterion. `ctx` is shared to this application of a rewrite _exclusively_.
    /// Generally, if `matches` returns true, this is expected to succeed. Otherwise its free to panic, which indicates a pattern bug.
    ///
    /// [apply](Self::apply) usually replace `node` fully (i.e. usually ending with [replace_node_uses](rvsdg::Rvsdg::replace_node_uses) or similar) once applied.
    /// It should **not** invalidate formerly valid nodes.
    fn apply(&self, ctx: &mut Ctx, node: NodeRef);
}
