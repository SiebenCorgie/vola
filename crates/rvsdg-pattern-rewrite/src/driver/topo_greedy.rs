use rvsdg::{Rvsdg, edge::LangEdge, nodes::LangNode, region::RegionLocation};

use crate::{DriverRecursion, PatternRewrite, benefit::Benefit, driver::DriverRestart};

///Traverses a graph in topological order, always applying the best (according to the benefits) rewrite-pattern to a node.
///A typical initialization would be:
/// ```rust, ignore
/// let driver = TopoGreedyRewriter::default()
/// .with_recursion(DriverRecursion::TopDown)
/// .with_restart(DriverRestart::Bound(10))
/// .with_rule(MyRule)
/// .with_rule(MyOtherRule);
///
/// driver.run_on(&mut myGraph, myRegion).unwrap();
/// ```
pub struct TopoGreedyRewriter<Node: LangNode, Edge: LangEdge, B: Benefit> {
    ///All registered rewrites
    rewriter: Vec<Box<dyn PatternRewrite<Node, Edge, B>>>,
    rules_changed: bool,
    pub recursion: DriverRecursion,
    pub restart: DriverRestart,
}

impl<Node, Edge, B> Default for TopoGreedyRewriter<Node, Edge, B>
where
    Node: LangNode,
    Edge: LangEdge,
    B: Benefit,
{
    fn default() -> Self {
        Self {
            rewriter: Vec::with_capacity(0),
            rules_changed: true,
            recursion: DriverRecursion::None,
            restart: DriverRestart::SingeShot,
        }
    }
}

impl<Node, Edge, B> TopoGreedyRewriter<Node, Edge, B>
where
    Node: LangNode,
    Edge: LangEdge,
    B: Benefit,
{
    pub fn with_recursion(mut self, recursion: DriverRecursion) -> Self {
        self.recursion = recursion;
        self
    }
    pub fn with_restart(mut self, restart: DriverRestart) -> Self {
        self.restart = restart;
        self
    }

    pub fn register(&mut self, rewrite: impl PatternRewrite<Node, Edge, B> + 'static) {
        self.rewriter.push(Box::new(rewrite));
        self.rules_changed = true;
    }

    ///Sorts the rules by benefit
    fn prepare_rules(&mut self) {
        self.rewriter.sort_by(|a, b| a.benefit().cmp(b.benefit()));
        self.rules_changed = false;
    }

    ///Runs the on `entry` in `graph`. If `recursive` is true, it'll consider sub-region (i.e. loop-bodies, etc.).
    pub fn run(&mut self, graph: &mut Rvsdg<Node, Edge>, entry: RegionLocation) {
        log::info!("Initializing patter-rewrite on {entry}");
        //NOTE: mini wrapper that catches if rules changed. Afterwards we can consider the driver immutable.
        if self.rules_changed {
            self.prepare_rules();
        }

        self.run_on(graph, entry)
    }
    ///Runst the driver on `entry`. Returns the amount of rewrites that was applied, if successful.
    fn run_on(&self, graph: &mut Rvsdg<Node, Edge>, entry: RegionLocation) {
        let mut order =
            graph.topological_order_nodes(graph.live_nodes_in_region(entry).into_iter());
        //First recurse
        if self.recursion == DriverRecursion::BottomUp {
            //NOTE: the just-build order is known to only contain existing nodes, so we really can just iter it.
            for node in &order {
                let region_count = graph[*node].regions().len();
                if region_count == 0 {
                    continue;
                }
                for region_index in 0..region_count {
                    self.run_on(
                        graph,
                        RegionLocation {
                            node: *node,
                            region_index,
                        },
                    );
                }
            }

            //This might have disturbed the order of nodes, therfore regenerate it
            order = graph.topological_order_nodes(graph.live_nodes_in_region(entry).into_iter());
        }

        //Tracks restart state
        let mut restart = self.restart.clone();

        //Outer loop takes care of restarting, if possible
        'restart: loop {
            let mut changed_any = false;

            //now, run on each node, applying the first fitting pattern.
            'note_iter: for node in &order {
                for pattern in &self.rewriter {
                    //Apply..
                    if pattern.matches(&graph, *node) {
                        changed_any = true;
                        pattern.apply(graph, *node);
                        //..and continue with next node
                        continue 'note_iter;
                    }
                }
            }

            //Always break if fixpoint was reached
            if !changed_any {
                log::info!("Reached fixpoint in {entry}");
                break;
            } else {
                //check if there are any rerstarts left
                if restart.should_restart() {
                    //if so, update the order before restarting
                    order = graph
                        .topological_order_nodes(graph.live_nodes_in_region(entry).into_iter());
                    continue 'restart;
                } else {
                    log::info!("Reached restart bound in {entry}");
                    break;
                }
            }
        }

        //Take care of top-down recursion.
        if self.recursion == DriverRecursion::TopDown {
            //NOTE: we make no gurantee in which order recursion happens, so we just recurse any existing node here.
            for node in &order {
                //Bail on none existing nodes and nodes without a subregion
                if let Some(node) = graph.try_node(*node) {
                    if node.regions().is_empty() {
                        continue;
                    }
                } else {
                    continue;
                }

                //Exists and has a subregion, recurse
                for region_index in 0..graph[*node].regions().len() {
                    self.run_on(
                        graph,
                        RegionLocation {
                            node: *node,
                            region_index,
                        },
                    )
                }
            }
        }
    }
}
