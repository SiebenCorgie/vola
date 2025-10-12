/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use std::collections::VecDeque;

use ahash::{AHashMap, AHashSet};
use rvsdg::{
    edge::{InportLocation, LangEdge, OutportLocation},
    util::abstract_node_type::AbstractNodeType,
    NodeRef,
};
use vola_common::{Span, VolaError};

use crate::{
    interval::{IntervalError, IntervalExtension},
    passes::activity::{Activity, ActivityAnalysis},
    typelevel::IntervalConstruct,
    OptEdge, OptError, OptNode, Optimizer,
};

/// Pass that handles the [interval-extension](https://en.wikipedia.org/wiki/Interval_arithmetic#Interval_extensions_of_general_functions) for any call to
/// `bound()` in the source program.
pub struct IntervalExtensionPass<'opt> {
    optimizer: &'opt mut Optimizer,
    //caches the vec-deque so we don't have to allocate all the time.
    rewriter_queue: VecDeque<OutportLocation>,
    output_interval_mapping: AHashMap<OutportLocation, OutportLocation>,
    input_interval_mapping: AHashMap<InportLocation, InportLocation>,
    seen: AHashSet<OutportLocation>,
}

impl<'opt> IntervalExtensionPass<'opt> {
    pub fn setup(optimizer: &'opt mut Optimizer) -> Self {
        IntervalExtensionPass {
            optimizer,
            rewriter_queue: VecDeque::with_capacity(0),
            output_interval_mapping: AHashMap::default(),
            input_interval_mapping: AHashMap::default(),
            seen: AHashSet::default(),
        }
    }

    pub fn extend_all(mut self) -> Result<(), VolaError<OptError>> {
        let all_entry_points = self.collect_entry_points()?;

        #[cfg(feature = "log")]
        log::info!("Interval extension for {}", all_entry_points.len());

        for ep in all_entry_points {
            let ep_span = self.optimizer.find_span(ep.into()).unwrap_or(Span::empty());

            //Extend the expression to an interval expression
            match self.expand_entry(ep) {
                Err(e) => {
                    return Err(
                        e.with_label(ep_span, "While extending this to an interval expression")
                    );
                }
                Ok(new_src) => {
                    //tick the type system for this
                    let t = self.optimizer.get_or_derive_type(new_src, true);
                    assert!(t.is_interval(), "{new_src} {} should be interval", t);
                }
            }
        }

        Ok(())
    }

    ///Collects all entry-points and verifies that they obey all rules. Returns the bottom-up list of entry-points to the extension.
    fn collect_entry_points(&mut self) -> Result<Vec<NodeRef>, VolaError<OptError>> {
        let mut all_nodes = Vec::with_capacity(0);

        for live_node in self.optimizer.graph.walk_reachable() {
            if self.optimizer.is_node_type::<IntervalExtension>(live_node) {
                all_nodes.push(live_node);
            }
        }

        //Since the walk_reachable is already BFS ordered, we just
        // have to reverse the order and are finished.
        all_nodes.reverse();
        Ok(all_nodes)
    }

    ///Expands the `entry_point` and replaces all users with the actual
    /// interval calculation.
    fn expand_entry(
        &mut self,
        entry_point: NodeRef,
    ) -> Result<OutportLocation, VolaError<OptError>> {
        //This basically transforms the entry-expression from a shape<T> expression to a interval<shape<t>> expression. We just copy the whole thing,
        // replace any dynamic value `p` with the interval of `p`
        // then we type-erase the whole thing and call the type resolved to do its thing.
        //
        // If everything is _well-defined_ we should get the interval expression.

        assert!(self
            .optimizer
            .is_node_type::<IntervalExtension>(entry_point));
        let value_dst = IntervalExtension::expr_input().to_location(entry_point);
        let expr_src = self.optimizer.graph.inport_src(value_dst).unwrap();
        let dynamic_dst = IntervalExtension::dynamic_input().to_location(entry_point);
        let interval_src = self
            .optimizer
            .graph
            .inport_src(IntervalExtension::interval().to_location(entry_point))
            .unwrap();

        //use the activity pass to _find_ all active _dynamic_ value
        let mut active_dynamics =
            ActivityAnalysis::setup(self.optimizer).execute(value_dst, dynamic_dst)?;

        #[cfg(feature = "log")]
        log::info!(
            "    Found {} active values for single dynamic value",
            active_dynamics
                .active
                .flags
                .iter()
                .fold(0, |res, this| if *this.1 { res + 1 } else { res })
        );

        //Now we have to copy parts of the _value_ expression into an equivalent
        // _interval_ expression. An observation is, that any non_active sub-expression can be treated as an minimum interval.
        // Example
        //
        // let a = p + c;
        // let ia = bound(a, p, [-INF..INF]);
        //
        // In this case only p is active in a, and c is constant. Therfore when transforming the expression of `a`, we only have to rewrite p as [-INF..INF], but c is the minimum-interval [c, c];
        //
        // In practice this means we can stop the forward-pass-style reverse-topology-order rewrite whenever we encounter a non-active value. In this case we just take the existing value, and create a IntervalConstruct for it.
        let rewritten_value =
            self.interval_rewrite(expr_src, interval_src, &mut active_dynamics)?;

        //Finally replace the entry_point consumers with the new value
        self.optimizer
            .graph
            .replace_outport_uses(entry_point.output(0), rewritten_value)
            .map_err(|err| VolaError::new(err.into()))?;

        self.optimizer
            .names
            .set(rewritten_value.into(), format!("Extended interval"));

        Ok(rewritten_value)
    }

    fn interval_rewrite(
        &mut self,
        value: OutportLocation,
        interval: OutportLocation,
        activity: &mut Activity,
    ) -> Result<OutportLocation, VolaError<OptError>> {
        //setup the internal rewriter queue.
        self.rewriter_queue.clear();
        self.output_interval_mapping.clear();
        self.input_interval_mapping.clear();
        self.seen.clear();
        //initialize the queue with the value
        self.rewriter_queue.push_front(value);
        self.seen.insert(value);

        //now, while the queue is none-empty:
        // take the next output, check its activity
        // if active:
        //     is equivalent to dynamic value?:
        //         - replace dst interval
        //     no:
        //         - copy node (if needed)
        //             - add to mapping
        //         - (If there are sub-regions), enque all value producers of the sub-regions.
        //         - enqueue all ancestors
        //         - connect to successor's interval representation
        // if not active:
        //     - Create the minimum-interval of the inactive value (if not already in cache)
        //     - connect to successor's interval representation.

        while let Some(next) = self.rewriter_queue.pop_back() {
            let region = self.optimizer.graph[next.node].parent.unwrap();

            if self.optimizer.graph[next.node].node_type.is_apply() {
                let span = self
                    .optimizer
                    .find_span(next.node.into())
                    .unwrap_or(Span::empty());
                return Err(VolaError::new(OptError::Interval(
                    IntervalError::UnsupportedNodeType(AbstractNodeType::Apply),
                ))
                .with_error(span, "here"));
            }

            //Try to use a cached representation
            let interval_port = if let Some(cached) = self.output_interval_mapping.get(&next) {
                *cached
            } else {
                //no cached found. Next check whether its active. If so we copy the node
                // and setup / enque the caching process. Otherwise we build the minimum interval and return that.
                if activity.is_active_port(&self.optimizer, next) {
                    if activity.is_wrt_producer(&next) {
                        //Is equivalent to the wrt port, use the interval
                        assert!(self
                            .output_interval_mapping
                            .insert(next, interval)
                            .is_none());
                        interval
                    } else {
                        //is not wrt proucer, active, and not yet cached. Therefore copy the node, register all inputs/outputs in the
                        // maps, and return the mapped port
                        let copy = self.optimizer.graph.deep_copy_node(next.node, region);
                        self.optimizer
                            .span_tags
                            .copy(&next.node.into(), copy.into());
                        self.optimizer.names.copy(&next.node.into(), copy.into());

                        //TODO: handle the enquing of value producers _in_ this node.
                        if self.optimizer.graph[copy].regions().len() > 0 {
                            unreachable!()
                        }

                        //register all inputs and outputs in mapping
                        // also register all input-connected nodes for further consideration.
                        for inport in self.optimizer.graph.inports(next.node) {
                            let mapped = inport.input.to_location(copy);
                            assert!(
                                self.input_interval_mapping.insert(inport, mapped).is_none(),
                                "If already in there, cache should be used"
                            );
                            if let Some(connected) = self.optimizer.graph.inport_src(inport) {
                                if !self.seen.contains(&connected) {
                                    self.rewriter_queue.push_front(connected);
                                    self.seen.insert(connected);
                                }
                            }
                        }
                        for output in self.optimizer.graph.outports(next.node) {
                            let mapped = output.output.to_location(copy);
                            assert!(
                                self.output_interval_mapping
                                    .insert(output, mapped)
                                    .is_none(),
                                "If already in there, cache should be used"
                            );
                        }

                        //now the searched-for port _must be_ present
                        *self.output_interval_mapping.get(&next).unwrap()
                    }
                } else {
                    //not active, just build the minimum interval
                    let span = self
                        .optimizer
                        .find_span(next.into())
                        .unwrap_or(Span::empty());
                    let output = self
                        .optimizer
                        .graph
                        .on_region(&region, |reg| {
                            let (node, _) = reg
                                .connect_node(
                                    OptNode::new(IntervalConstruct::default(), span),
                                    [next, next],
                                )
                                .unwrap();
                            node.output(0)
                        })
                        .unwrap();

                    //register mapping, we might need it again.
                    self.output_interval_mapping.insert(next, output);
                    output
                }
            };

            //Finally rewire the original edge
            //do this by finding the _originally-connected-to_ inports.
            // If they are mapped already, they are part of the interval-expression
            // therefore doublicate the edge
            // and rebuild it in the interval representation.
            for edge in self.optimizer.graph[next].edges.clone() {
                let dst = *self.optimizer.graph[edge].dst();
                if self.input_interval_mapping.contains_key(&dst) {
                    //is mapped, therfore copy edge type, and rebuild the edge on both mappings
                    let edge_type = if self.optimizer.graph[edge].ty.is_state_edge() {
                        OptEdge::State
                    } else {
                        OptEdge::value_edge_unset()
                    };
                    let mapped_src = *self.output_interval_mapping.get(&next).unwrap();
                    assert_eq!(mapped_src, interval_port);
                    let mapped_dst = *self.input_interval_mapping.get(&dst).unwrap();

                    self.optimizer
                        .graph
                        .connect(mapped_src, mapped_dst, edge_type)
                        .unwrap();
                }
            }
        }

        //Return the mapped initial output
        Ok(*self
            .output_interval_mapping
            .get(&value)
            .expect("Initial value must be mapped at least"))
    }
}
