/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use std::{collections::VecDeque, ops::Not};

use ahash::{AHashMap, AHashSet};
use rvsdg::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    region::RegionLocation,
    util::abstract_node_type::AbstractNodeType,
    NodeRef,
};
use vola_common::{Span, VolaError};

use crate::{
    common::Ty,
    interval::{IntervalError, IntervalExtension},
    passes::activity::{Activity, ActivityAnalysis},
    typelevel::IntervalConstruct,
    OptEdge, OptError, OptNode, Optimizer, TypeState,
};

/// Pass that handles the [interval-extension](https://en.wikipedia.org/wiki/Interval_arithmetic#Interval_extensions_of_general_functions) for any call to
/// `bound()` in the source program.
pub struct IntervalExtensionPass<'opt> {
    optimizer: &'opt mut Optimizer,
    //caches the vec-deque so we don't have to allocate all the time.
    rewriter_queue: VecDeque<OutportLocation>,
    ///Caches the rewire-ports _in-order_ so we can rebuild the interval-typed
    /// expression after copying/transforming intervals
    wire_queue: Vec<(OutportLocation, OutportLocation)>,
    output_interval_mapping: AHashMap<OutportLocation, OutportLocation>,
    input_interval_mapping: AHashMap<InportLocation, InportLocation>,
    seen: AHashSet<OutportLocation>,
}

impl<'opt> IntervalExtensionPass<'opt> {
    pub fn setup(optimizer: &'opt mut Optimizer) -> Self {
        IntervalExtensionPass {
            optimizer,
            rewriter_queue: VecDeque::with_capacity(0),
            wire_queue: Vec::default(),
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
            let ep_span = self.optimizer.find_span(ep).unwrap_or(Span::empty());

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

        self.interval_rewrite(expr_src, interval_src, &mut active_dynamics)?;
        //Now, assuming all rewrites happened, re-trace the value producer of the interval-expression
        // (it might have changed while rewriting), and look-up its interval-representation.
        let rewritten_value = *self
            .output_interval_mapping
            .get(&self.optimizer.graph.inport_src(value_dst).unwrap())
            .unwrap();
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
    ) -> Result<(), VolaError<OptError>> {
        //setup the internal rewriter queue.
        self.rewriter_queue.clear();
        self.wire_queue.clear();
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

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_PRE_INTERVAL_EXT").is_ok()
        {
            self.optimizer
                .push_debug_state_with(&format!("pre-interval-ext-{}", value.node), |w| {
                    w.with_flags("Activity", &activity.active)
                });
        }

        while let Some(next) = self.rewriter_queue.pop_back() {
            let region = self.optimizer.graph[next.node].parent.unwrap();
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
                        // Is not wrt proucer, active, and not yet cached.
                        // Check the node type. For simple nodes, copy the node,
                        // register all inputs/outputs in the maps, and return
                        // the mapped port.
                        // For all others, shell out to a special handling routine, Those mostly handle
                        // the copying of the node (gamma/theta/lambda), and setting up activity state in those nodes
                        // in order to make activity tracing _correct_ too.
                        match self.optimizer.graph[next.node].into_abstract() {
                            AbstractNodeType::Simple => self.handle_simple(region, next.node)?,
                            //NOTE: we start handling of λ nodes on the first invocation. I.e. We should never encounter a
                            //      λ-node itself.
                            AbstractNodeType::Apply => {
                                //NOTE: we currently just inline the apply node
                                //      therefore the output becomes invalid which is
                                //      why we simply contiue afterward
                                self.handle_apply(activity, next.node)?;
                                continue;
                            }
                            AbstractNodeType::Gamma => self.handle_gamma(activity, next.node)?,
                            AbstractNodeType::Theta => self.handle_theta(activity, next.node)?,
                            other => {
                                let span =
                                    self.optimizer.find_span(next.node).unwrap_or(Span::empty());
                                return Err(VolaError::new(OptError::Interval(
                                    IntervalError::UnsupportedNodeType(other),
                                ))
                                .with_error(span, "here"));
                            }
                        };

                        //now the searched-for port _must be_ present
                        *self.output_interval_mapping.get(&next).unwrap()
                    }
                } else {
                    //not active, just build the minimum interval, i.e the interval [x..x]
                    let span = self.optimizer.find_span(next).unwrap_or(Span::empty());
                    let src_reg = self.optimizer.graph.outport_region(next);
                    let edgety = OptEdge::value_edge()
                        .with_type(self.optimizer.get_out_type_mut(next).unwrap());
                    let output = self
                        .optimizer
                        .graph
                        .on_region(&src_reg, |reg| {
                            let (node, _) = reg
                                .connect_node_with(
                                    OptNode::new(IntervalConstruct::default(), span),
                                    [(next, edgety.clone()), (next, edgety)],
                                )
                                .unwrap();
                            node.output(0)
                        })
                        .unwrap();

                    //register mapping, we might need it again.
                    let _ = self.output_interval_mapping.insert(next, output);
                    output
                }
            };

            self.wire_queue.push((next, interval_port));
        }

        let mut swap = Vec::with_capacity(0);
        std::mem::swap(&mut self.wire_queue, &mut swap);
        for (original, interval) in swap {
            self.rewire_for_interval(original, interval)?;
        }

        //Return the mapped initial output
        Ok(())
    }

    fn rewire_for_interval(
        &mut self,
        port: OutportLocation,
        interval_port: OutportLocation,
    ) -> Result<(), VolaError<OptError>> {
        //do this by finding the _originally-connected-to_ inports.
        // If they are mapped already, they are part of the interval-expression
        // therefore doublicate the edge
        // and rebuild it in the interval representation.
        for edge in self.optimizer.graph[port].edges.clone() {
            let dst = *self.optimizer.graph[edge].dst();
            if self.input_interval_mapping.contains_key(&dst) {
                //is mapped, therfore copy edge type, and rebuild the edge on both mappings
                let edge_type = match self.optimizer.graph[edge].ty.clone() {
                    OptEdge::State => OptEdge::State,
                    OptEdge::Value { ty } => {
                        let ty = match ty {
                            TypeState::Derived(t) => {
                                assert!(t.is_interval().not());
                                TypeState::Derived(Ty::Interval(Box::new(t)))
                            }
                            TypeState::Set(t) => {
                                assert!(t.is_interval().not());
                                TypeState::Set(Ty::Interval(Box::new(t)))
                            }
                            TypeState::Unset => TypeState::Unset,
                        };
                        OptEdge::Value { ty }
                    }
                };
                let mapped_src = *self.output_interval_mapping.get(&port).unwrap();
                assert_eq!(mapped_src, interval_port);
                let mapped_dst = *self.input_interval_mapping.get(&dst).unwrap();

                let src_region = self.optimizer.graph.outport_region(mapped_src);
                let dst_region = self.optimizer.graph.inport_region(mapped_dst);

                let src_port = if src_region != dst_region {
                    assert!(
                        self.optimizer
                            .graph
                            .is_in_parent(dst_region.node, src_region.node),
                        "Src region must be same or parent to dst-region"
                    );
                    //import src into dst
                    let (imported_at, path) = self
                        .optimizer
                        .graph
                        .import_argument(mapped_src, dst_region)
                        .map_err(|e| VolaError::new(e.into()))?;
                    if let Some(path) = path {
                        self.optimizer.type_path(&path).unwrap();
                    }
                    imported_at
                } else {
                    mapped_src
                };

                self.optimizer
                    .graph
                    .connect(src_port, mapped_dst, edge_type)
                    .unwrap();
            }
        }

        Ok(())
    }

    fn handle_simple(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<(), VolaError<OptError>> {
        //For simple nodes, just copy the node itself, carry over some
        // tags and return
        let copy = self.optimizer.graph.deep_copy_node(node, region);
        self.optimizer.span_tags.copy(&node.into(), copy.into());
        self.optimizer.names.copy(&node.into(), copy.into());

        //register all inputs and outputs in mapping
        // also register all input-connected nodes for further consideration.
        for inport in self.optimizer.graph.inports(node) {
            let mapped = inport.input.to_location(copy);
            assert!(
                self.input_interval_mapping.insert(inport, mapped).is_none(),
                "If already in there, cache should be used"
            );
            self.register_inport_queue(inport);
        }
        for output in self.optimizer.graph.outports(node) {
            let mapped = output.output.to_location(copy);
            assert!(
                self.output_interval_mapping
                    .insert(output, mapped)
                    .is_none(),
                "If already in there, cache should be used"
            );
        }

        Ok(())
    }

    ///Registers the `inport`'s produce, it it was not seen yet.
    fn register_inport_queue(&mut self, inport: InportLocation) {
        if let Some(connected) = self.optimizer.graph.inport_src(inport) {
            if !self.seen.contains(&connected) {
                self.rewriter_queue.push_front(connected);
                self.seen.insert(connected);
            }
        }
    }

    ///Handles the Gamma input/output interface
    /// to produce a valid interval-mapping
    /// for each input/output
    fn handle_gamma(
        &mut self,
        activity: &mut Activity,
        node: NodeRef,
    ) -> Result<(), VolaError<OptError>> {
        //For gamma (and theta) nodes our strategy iterates all inputs and results.
        // For each active port we create a copy for that port and register its mapping.
        // Then we register the input-connected nodes in the rewriter queue.
        //
        // This implicitly traverses region boundaries where needed.
        //
        // Since we do this for _all_ inports of this node, the caching later on pick up on it
        // which prevents multiple iterations.

        //Handle active entry-variables by creating a new entry-variable,
        // inserting it as a mapping, and enquing all entry-var-connected nodes
        // in the rewriter queue.
        for ev in 0..self.optimizer.graph[node]
            .node_type
            .unwrap_gamma_ref()
            .entry_var_count()
        {
            //Ignore none-active entry-variables
            if !activity
                .active
                .get(&InputType::EntryVariableInput(ev).to_location(node).into())
                .unwrap_or(&false)
            {
                continue;
            }

            //Create the new EV that stands for the interval
            let interval_ev = self.optimizer.graph[node]
                .node_type
                .unwrap_gamma_mut()
                .add_entry_var();

            //Insert the input mapping...
            assert!(self
                .input_interval_mapping
                .insert(
                    InputType::EntryVariableInput(ev).to_location(node),
                    InputType::EntryVariableInput(interval_ev).to_location(node),
                )
                .is_none());
            //.. and for each branch the output mapping
            for branch in 0..self.optimizer.graph[node].regions().len() {
                assert!(self
                    .output_interval_mapping
                    .insert(
                        OutputType::EntryVariableArgument {
                            branch,
                            entry_variable: ev,
                        }
                        .to_location(node),
                        OutputType::EntryVariableArgument {
                            branch,
                            entry_variable: interval_ev,
                        }
                        .to_location(node),
                    )
                    .is_none());
            }

            //Finally register each connected value in the queue
            self.register_inport_queue(InputType::EntryVariableInput(ev).to_location(node));
        }

        //Now do, conceptually the same for exit-variables:
        // Copy active exit-vars, register them in the mapping, and push all in-branch connected
        // values
        for ex in 0..self.optimizer.graph[node]
            .node_type
            .unwrap_gamma_ref()
            .exit_var_count()
        {
            let ex_output = OutputType::ExitVariableOutput(ex).to_location(node);
            //ignore inactive exit-variables
            if !activity.is_active_port(&self.optimizer, ex_output) {
                continue;
            }

            let interval_ex = self.optimizer.graph[node]
                .node_type
                .unwrap_gamma_mut()
                .add_exit_var();
            //register the output mapping
            assert!(self
                .output_interval_mapping
                .insert(
                    ex_output,
                    OutputType::ExitVariableOutput(interval_ex).to_location(node)
                )
                .is_none());
            //now iterate all branches, register the input mapping and push all connected sources
            for branch in 0..self.optimizer.graph[node].regions().len() {
                let branch_result = InputType::ExitVariableResult {
                    branch,
                    exit_variable: ex,
                }
                .to_location(node);
                assert!(self
                    .input_interval_mapping
                    .insert(
                        branch_result,
                        InputType::ExitVariableResult {
                            branch,
                            exit_variable: interval_ex
                        }
                        .to_location(node)
                    )
                    .is_none());
                self.register_inport_queue(branch_result);
            }
        }

        Ok(())
    }

    fn handle_theta(
        &mut self,
        activity: &mut Activity,
        node: NodeRef,
    ) -> Result<(), VolaError<OptError>> {
        //See the gamma-comments. The main idea is to create a copy for any _needed-as-interval_
        // ports and then record those into our mapping.

        for lv in 0..self.optimizer.graph[node]
            .node_type
            .unwrap_theta_ref()
            .loop_variable_count()
        {
            //for each loop variable, if its active add a interval-typed version
            let loop_out = OutputType::Output(lv).to_location(node);
            if !activity.is_active_port(&self.optimizer, loop_out) {
                continue;
            }

            let interval_lv = self.optimizer.graph[node]
                .node_type
                .unwrap_theta_mut()
                .add_loop_variable();
            //now add all, the input/arg and result/output compinations to the mapping. Then enqueue both, input-connected
            // and result-connected values to the queue
            assert!(self
                .input_interval_mapping
                .insert(
                    InputType::Input(lv).to_location(node),
                    InputType::Input(interval_lv).to_location(node)
                )
                .is_none());
            assert!(self
                .output_interval_mapping
                .insert(
                    OutputType::Argument(lv).to_location(node),
                    OutputType::Argument(interval_lv).to_location(node)
                )
                .is_none());

            assert!(self
                .input_interval_mapping
                .insert(
                    InputType::Result(lv).to_location(node),
                    InputType::Result(interval_lv).to_location(node)
                )
                .is_none());
            assert!(self
                .output_interval_mapping
                .insert(
                    OutputType::Output(lv).to_location(node),
                    OutputType::Output(interval_lv).to_location(node)
                )
                .is_none());

            //Now register both input types
            self.register_inport_queue(InputType::Input(lv).to_location(node));
            self.register_inport_queue(InputType::Result(lv).to_location(node));
        }

        Ok(())
    }

    fn handle_apply(
        &mut self,
        _activity: &mut Activity,
        node: NodeRef,
    ) -> Result<(), VolaError<OptError>> {
        //NOTE: we take a shortcut here and just inline the call.
        //
        //      The reason is, that the handling is not that easy:
        //      We can't just use the activity state to extent whatever is
        //      in the λ to produce an associated interval. IFF the λ is called
        //      multiple times, we'd have to do this for each call-site, leading to
        //      _many_ results. Same goes for any _active_ argument to the function. Those
        //      would need to be extented as well _on-each-callsite_...
        //
        //      We can't _just_ copy the λ, since within the λ might be an additional
        //      _to-be-extented_ interval-extension node. However, our extension pass currently
        //      does not handle that case. So in the end we just inline the call, and register all result-connected
        //      nodes in the queue
        //
        //TODO: 1. Use a queue for the interval-entry-points. This would let us copy λs
        //      2. Then _just_ copy the function and create a new apply-node that calls the
        //         interval version of a function.
        //     3. Also record the λ-decl port in our mapping, so we don't copy λ-nodes more often than we should.

        assert_eq!(self.optimizer.graph[node].outputs().len(), 1);
        let original_dsts = self.optimizer.graph.unique_dst_ports(node);
        self.optimizer.graph.inline_apply_node(node).unwrap();
        for dst in original_dsts {
            self.register_inport_queue(dst);
        }

        Ok(())
    }
}
