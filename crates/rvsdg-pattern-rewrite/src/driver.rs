use rvsdg::{Rvsdg, edge::LangEdge, nodes::LangNode, region::RegionLocation};

use crate::{PatternRewrite, benefit::Benefit};

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

///Traverses a graph in topological order, always applying the best (according to the benefits) rewrite-pattern to a node.
///A typical initialization would be:
/// ```rust, ignore
/// let driver = TopoGreedyRewriter{
///     recursion: DriverRecursion::TopDown,
///     ..Default::default()
/// }
/// .with_rule(MyRule)
/// .with_rule(MyOtherRule);
///
/// driver.run_on(&mut myGraph, myRegion).unwrap();
/// ```
pub struct TopoGreedyRewriter<Node: LangNode, Edge: LangEdge, B: Benefit, Err> {
    ///All registered rewrites
    rewriter: Vec<Box<dyn PatternRewrite<Node, Edge, B, Err>>>,
    rules_changed: bool,
    pub recursion: DriverRecursion,
}

impl<Node, Edge, B, Err> Default for TopoGreedyRewriter<Node, Edge, B, Err>
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
        }
    }
}

impl<Node, Edge, B, Err> TopoGreedyRewriter<Node, Edge, B, Err>
where
    Node: LangNode,
    Edge: LangEdge,
    B: Benefit,
{
    pub fn register(&mut self, rewrite: impl PatternRewrite<Node, Edge, B, Err> + 'static) {
        self.rewriter.push(Box::new(rewrite));
        self.rules_changed = true;
    }

    ///Sorts the rules by benefit
    fn prepare_rules(&mut self) {
        self.rewriter.sort_by(|a, b| a.benefit().cmp(b.benefit()));
        self.rules_changed = false;
    }

    ///Runs the on `entry` in `graph`. If `recursive` is true, it'll consider sub-region (i.e. loop-bodies, etc.).
    pub fn run(&mut self, graph: &mut Rvsdg<Node, Edge>, entry: RegionLocation) -> Result<(), Err> {
        //NOTE: mini wrapper that catches if rules changed. Afterwards we can consider the driver immutable.
        if self.rules_changed {
            self.prepare_rules();
        }

        self.run_on(graph, entry)
    }
    fn run_on(&self, graph: &mut Rvsdg<Node, Edge>, entry: RegionLocation) -> Result<(), Err> {
        let mut order = graph.topological_order_region(entry);
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
                    )?
                }
            }

            //This might have disturbed the order of nodes, therfore regenerate it
            order = graph.topological_order_region(entry)
        }

        //now, run on each node, applying the first fitting pattern.
        'note_iter: for node in &order {
            for pattern in &self.rewriter {
                //Apply..
                if pattern.matches(*node) {
                    pattern.apply(graph, *node)?;
                    //..and continue with next node
                    continue 'note_iter;
                }
            }
        }

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
                    )?
                }
            }
        }

        Ok(())
    }
}
