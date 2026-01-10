use rvsdg::{NodeRef, SmallColl, SmallSet, edge::LangEdge, nodes::LangNode};

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
    /// Returns Ok if any pattern matched, carrying the replaced root node(s) (i.e. the nodes that replaced `node`).
    /// This might be many, if a node that produced multiple values was replaced with multiple
    /// nodes that produce a single value for instance.
    /// Otherwise returns Err.
    pub fn apply_on(&mut self, ctx: &mut Ctx, node: NodeRef) -> Result<SmallColl<NodeRef>, ()> {
        if self.rules_changed {
            self.prepare_rules();
        }

        for pattern in &self.rewriter {
            if pattern.matches(ctx, node) {
                //save the first user
                let unique_users = ctx.graph().unique_dst_ports(node);
                log::info!("Applying direct pattern {}", pattern.name());
                pattern.apply(ctx, node);

                //fold the producers of the unique users into a single array of producers
                let producer = unique_users
                    .into_iter()
                    .map(|user| ctx.graph().inport_src(user))
                    .flatten()
                    .map(|user| user.node)
                    .collect::<SmallSet<NodeRef>>();

                return Ok(producer.into());
            }
        }

        Err(())
    }
}
