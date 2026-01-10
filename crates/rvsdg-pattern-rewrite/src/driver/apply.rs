use rvsdg::{NodeRef, edge::LangEdge, nodes::LangNode};

use crate::{Benefit, PatternRewrite, driver::RewriteableGraph};

///Driver that directly applies a matching pattern on a
/// node.
pub struct Apply<
    N: LangNode + 'static,
    E: LangEdge + 'static,
    Ctx: RewriteableGraph<Node = N, Edge = E>,
    B: Benefit,
> {
    rewriter: Vec<Box<dyn PatternRewrite<N, E, Ctx, B>>>,
    rules_changed: bool,
}

impl<Node, Edge, Ctx, B> Default for Apply<Node, Edge, Ctx, B>
where
    Node: LangNode + 'static,
    Edge: LangEdge + 'static,
    Ctx: RewriteableGraph<Node = Node, Edge = Edge>,
    B: Benefit,
{
    fn default() -> Self {
        Self {
            rewriter: Vec::with_capacity(0),
            rules_changed: true,
        }
    }
}

impl<Node, Edge, Ctx, B> Apply<Node, Edge, Ctx, B>
where
    Node: LangNode + 'static,
    Edge: LangEdge + 'static,
    Ctx: RewriteableGraph<Node = Node, Edge = Edge>,
    B: Benefit,
{
    ///Registers a rewrite pattern.
    pub fn register(&mut self, rewrite: impl PatternRewrite<Node, Edge, Ctx, B> + 'static) {
        self.rewriter.push(Box::new(rewrite));
        self.rules_changed = true;
    }

    ///Sorts the rules by benefit
    fn prepare_rules(&mut self) {
        self.rewriter.sort_by(|a, b| a.benefit().cmp(b.benefit()));
        self.rules_changed = false;
    }

    ///Runs the first (i.e. best) matching rewrite on `node`.
    ///
    /// Returns true if any pattern matched, and its rewrite was applied. Otherwise false
    pub fn apply_on(&mut self, ctx: &mut Ctx, node: NodeRef) -> bool {
        if self.rules_changed {
            self.prepare_rules();
        }

        for pattern in &self.rewriter {
            if pattern.matches(ctx, node) {
                log::info!("Applying direct pattern {}", pattern.name());
                pattern.apply(ctx, node);
                return true;
            }
        }

        false
    }
}
