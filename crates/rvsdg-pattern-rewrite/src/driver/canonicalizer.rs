use std::collections::VecDeque;

use ahash::AHashSet;
use rvsdg::{
    NodeRef,
    edge::{InputType, LangEdge, OutportLocation, OutputType},
    nodes::LangNode,
    region::RegionLocation,
};

use crate::{PatternRewrite, benefit::Benefit, driver::RewriteableGraph};

///The actual canonicalization runner that reuses already allocated
/// helpers from the base pass.
struct CanonRunner<
    'a,
    N: LangNode + 'static,
    E: LangEdge + 'static,
    Ctx: RewriteableGraph<Node = N, Edge = E>,
    B: Benefit,
> {
    queue: &'a mut VecDeque<OutportLocation>,
    seen: &'a mut AHashSet<OutportLocation>,
    handeled: &'a mut AHashSet<NodeRef>,
    rewriter: &'a Vec<Box<dyn PatternRewrite<N, E, Ctx, B>>>,
    follow_calls: bool,
    follow_context: bool,
}

impl<
    'a,
    N: LangNode + 'static,
    E: LangEdge + 'static,
    Ctx: RewriteableGraph<Node = N, Edge = E>,
    B: Benefit,
> CanonRunner<'a, N, E, Ctx, B>
{
    pub fn run(&mut self, graph: &mut Ctx) {
        //TODO: implement the _recursive_ predecesor following in a iterative fashion. On each node call the rewriter
        //      Then reimplement the AD canonicalizer on top of this API as a simple set of rules

        // Iterate the queue and handle each node according to the pattern rewrite.
        // Depending on _follow_calls_ we might enque additional nodes
        // when encountering apply nodes

        while let Some(next) = self.queue.pop_front() {
            //ignore if alread seen
            if self.seen.contains(&next) {
                continue;
            } else {
                //tag as seen
                self.seen.insert(next);
            }

            log::trace!(
                "Check {next} / {:?}",
                graph.graph()[next.node].into_abstract()
            );

            if !next.output.is_argument() {
                if let OutputType::ContextVariableArgument(_cvidx) = next.output {
                    //if this is a context variable output, check whether we actually want to follow that
                    if self.follow_context {
                        if let Some(outside) = next.output.map_out_of_region() {
                            if let Some(src) = graph
                                .graph()
                                .inport_src(next.node.as_inport_location(outside))
                            {
                                //valid context connected, enque
                                self.queue.push_back(src);
                            }
                        }
                    }
                } else if let OutputType::RecursionVariableArgument(_rvarg) = next.output {
                    //we dont handle those yet
                    log::error!("Recursion variable argument not handeled!");
                } else if let OutputType::DeltaDeclaration | OutputType::LambdaDeclaration =
                    next.output
                {
                    //for declerations context_following was activated, therefore enque all results of the context
                    for region in graph.graph().iter_regions(next.node) {
                        for result in graph.graph().result_ports(region) {
                            if let Some(src) = graph.graph().inport_src(result) {
                                self.queue.push_back(src);
                            }
                        }
                    }
                } else {
                    //all other outputs (which are no arguments) with regions are just mapped into the region and followed.
                    // In practice this is manly loop-variable-outputs and and exit-variables
                    for region in graph.graph().iter_regions(next.node) {
                        let Some(inside) = next.output.map_to_in_region(region.region_index) else {
                            continue;
                        };

                        let Some(src) = graph
                            .graph()
                            .inport_src(next.node.as_inport_location(inside))
                        else {
                            continue;
                        };

                        //has a valid src, enque it
                        self.queue.push_back(src);
                    }
                }
            } else {
                //if this is argument-like (i.e. loop-var / entry-var) handle the mapping _out_
                // of the node, and in case of the loop-var, also enque the loop-var-result. The
                // latter is needed to catch cases where (for intance) the result of lv0 is fed by lv1,
                // i.e. shifting data in loops across invocations.

                //for both, map out of region, and check whether there is a value connected
                if let Some(outside) = next.output.map_out_of_region() {
                    if let Some(src) = graph
                        .graph()
                        .inport_src(next.node.as_inport_location(outside))
                    {
                        self.queue.push_back(src);
                    }
                }

                //only for theta, enque the respective loop-result
                if graph.graph()[next.node].node_type.is_theta() {
                    let OutputType::Argument(index) = next.output else {
                        log::error!(
                            "Handling argument-like theta port, but it wasn't an argument, ignoring full port since its invalid ..."
                        );
                        continue;
                    };

                    let result_port = next.node.as_inport_location(InputType::Result(index));
                    if let Some(src) = graph.graph().inport_src(result_port) {
                        self.queue.push_back(src);
                    }
                }
            }

            //if this is a apply node, and we should follow those, find the connected lambda/phi, and enque the respective result
            if graph.graph()[next.node].node_type.is_apply() && self.follow_calls {
                if let OutputType::Output(index) = next.output {
                    if let Some(calldef) = graph.graph().find_called(next.node) {
                        //check whether the called node's resul is connected
                        if let Some(result_src) = graph
                            .graph()
                            .inport_src(calldef.node.as_inport_location(InputType::Result(index)))
                        {
                            //has a result, enqueue it
                            self.queue.push_back(result_src);
                        }
                    }
                } else {
                    log::error!(
                        "On apply node {}, but not using invalid result {}",
                        next.node,
                        next.output
                    );
                }
            }

            //for _simple_ and apply nodes, enque all input-connected nodes
            if graph.graph()[next.node].node_type.is_apply()
                || graph.graph()[next.node].node_type.is_simple()
            {
                for input in graph.graph().inports(next.node) {
                    if let Some(src) = graph.graph().inport_src(input) {
                        self.queue.push_back(src);
                    }
                }
            }

            //for gamma-nodes, also enque the predicate produces
            if graph.graph()[next.node].node_type.is_gamma() {
                if let Some(src) = graph
                    .graph()
                    .inport_src(next.node.as_inport_location(InputType::GammaPredicate))
                {
                    self.queue.push_back(src);
                }
            }

            // finally handle the node itself, if it wasn't handeled
            // yet.
            // If it was handeled, just bail in order to not emit canon-nodes multiple time.
            // this can happen if a value is used multiple times from different predecessor paths.
            if !self.handeled.contains(&next.node) {
                self.handeled.insert(next.node);

                //find the first matching rule and apply it.
                for rule in self.rewriter {
                    if rule.matches(graph, next.node) {
                        log::info!("Apply rewrite to {} via {next}", next.node);

                        //Safe who uses the value and re-insert it after applying the rule. This
                        // basically allows the canonicalizer to
                        // re-visit just inserted experssions.
                        // This in turn allows rules to emit code, that is
                        // further lowered by other rules
                        let mut users = graph.graph().unique_dst_ports(next.node);

                        rule.apply(graph, next.node);

                        //If there is a user (which we could technically assume).
                        // enque the source, which effectively enques
                        // the just canonicalized node
                        if let Some(user) = users.pop() {
                            if let Some(src) = graph.graph().inport_src(user) {
                                //NOTE: add to the front in order to evaluate them immediately.
                                self.queue.push_front(src);
                            }
                        }

                        break;
                    }
                }
            }
        }
    }
}

///Traverses the dependencies of a value in topological order always applying the best (according to benefits) matching rewrite pattern.
/// ```rust, ignore
/// let driver = Canonicalize::new(false)
/// .with_rule(MyRule)
/// .with_rule(MyOtherRule);
///
/// driver.run_on(&mut my_graph, my_value).unwrap();
/// ```
pub struct Canonicalizer<
    N: LangNode + 'static,
    E: LangEdge + 'static,
    Ctx: RewriteableGraph<Node = N, Edge = E>,
    B: Benefit,
> {
    ///All registered rewrites
    rewriter: Vec<Box<dyn PatternRewrite<N, E, Ctx, B>>>,
    queue: VecDeque<OutportLocation>,
    seen: AHashSet<OutportLocation>,
    handeled: AHashSet<NodeRef>,
    rules_changed: bool,
    follow_calls: bool,
    follow_context: bool,
}

impl<Node, Edge, Ctx, B> Default for Canonicalizer<Node, Edge, Ctx, B>
where
    Node: LangNode + 'static,
    Edge: LangEdge + 'static,
    Ctx: RewriteableGraph<Node = Node, Edge = Edge>,
    B: Benefit,
{
    fn default() -> Self {
        Self {
            rewriter: Vec::with_capacity(0),
            queue: VecDeque::default(),
            seen: AHashSet::default(),
            handeled: AHashSet::default(),
            rules_changed: true,
            follow_calls: false,
            follow_context: false,
        }
    }
}

impl<Node, Edge, Ctx, B> Canonicalizer<Node, Edge, Ctx, B>
where
    Node: LangNode + 'static,
    Edge: LangEdge + 'static,
    Ctx: RewriteableGraph<Node = Node, Edge = Edge>,
    B: Benefit,
{
    ///If set, follows apply nodes into the called node and canonicalizes the respective result-producer.
    ///
    /// Default: Off
    pub fn follow_calls(mut self, is_following: bool) -> Self {
        self.follow_calls = is_following;
        self
    }

    ///If set, follows context variables and canonicalizes it as well.
    ///
    /// Default: Off
    pub fn follow_context(mut self, is_following: bool) -> Self {
        self.follow_context = is_following;
        self
    }

    ///Registers a canonicalization pattern.
    pub fn register(&mut self, rewrite: impl PatternRewrite<Node, Edge, Ctx, B> + 'static) {
        self.rewriter.push(Box::new(rewrite));
        self.rules_changed = true;
    }

    ///Sorts the rules by benefit
    fn prepare_rules(&mut self) {
        self.rewriter.sort_by(|a, b| a.benefit().cmp(b.benefit()));
        self.rules_changed = false;
    }

    ///Runs the on `value` in `graph`. Applies the patterns to all values that contribute to `value`, i.e. all dependencies.
    ///
    /// Depending on whether [follow_calls](Self::follow_calls) and [follow_context](Self::follow_context) are set, might canonicalize called function bodies and used context as well.
    pub fn canonicalize_value(&mut self, ctx: &mut Ctx, value: OutportLocation) {
        log::info!("Initializing canonicalization-rewrite on {value}");
        //NOTE: mini wrapper that catches if rules changed. Afterwards we can consider the driver immutable.
        if self.rules_changed {
            self.prepare_rules();
        }

        self.on_value(ctx, value)
    }

    ///Runs the driver on all live nodes of `region` (i.e. nodes connected to the results of the region) and, depending on
    /// whether [follow_calls](Self::follow_calls) / [follow_context](Self::follow_context) was set, possibly related regions as well.
    pub fn canonicalize_region(&mut self, ctx: &mut Ctx, region: RegionLocation) {
        log::info!("Initialize canonicalization-rewrite for {region}");

        if self.rules_changed {
            self.prepare_rules();
        }

        self.on_region(ctx, region);
    }

    ///Runs the driver on `value` and its dependencies.
    fn on_value(&mut self, ctx: &mut Ctx, value: OutportLocation) {
        //setup the runner for this run and defer to it
        self.queue.clear();
        self.seen.clear();

        //enque the value itself
        self.queue.push_front(value);

        //now build the actual runner, and let it run to completition
        let mut runner = CanonRunner {
            rewriter: &self.rewriter,
            queue: &mut self.queue,
            seen: &mut self.seen,
            handeled: &mut self.handeled,
            follow_calls: self.follow_calls,
            follow_context: self.follow_context,
        };

        runner.run(ctx);
    }

    fn on_region(&mut self, ctx: &mut Ctx, region: RegionLocation) {
        self.queue.clear();
        self.seen.clear();

        //for the region case, we enque all results and then run

        for result in ctx.graph().result_ports(region) {
            if let Some(src) = ctx.graph().inport_src(result) {
                self.queue.push_back(src);
            }
        }

        //now build the actual runner, and let it run to completition
        let mut runner = CanonRunner {
            rewriter: &self.rewriter,
            queue: &mut self.queue,
            seen: &mut self.seen,
            handeled: &mut self.handeled,
            follow_calls: self.follow_calls,
            follow_context: self.follow_context,
        };

        runner.run(ctx);
    }
}
