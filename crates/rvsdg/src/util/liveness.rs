/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Implements _liveness_ analysis.
//!
//! See
//!
//! - [liveness](crate::Rvsdg::liveness)
//! - [liveness_region](crate::Rvsdg::liveness_region)
//!
//! This is a analysis-only pass which builds a lookup table of all nodes (or nodes in a region). The lookup table flags all nodes that
//! are considered live as `true`.
//! All nodes that are dead are flagged `false` or not at all.

use std::collections::VecDeque;

use ahash::AHashSet;

use crate::{
    attrib::{AttribLocation, FlagStore},
    edge::{InportLocation, InputType, LangEdge, OutputType},
    nodes::{LangNode, NodeType},
    region::RegionLocation,
    NodeRef, Rvsdg,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Calculates the liveness of the whole graph. See [liveness_region](Self::liveness_region) for more information.
    pub fn liveness(&self) -> FlagStore<bool> {
        //just execute for the toplevel region.
        self.liveness_region(self.toplevel_region())
    }

    ///Does liveness analysis on `region` and all sub-regions of it. Returns a [FlagStore]
    ///That flags all known _live_ ports with `true`. All ports that are not flagged, or flagged `false` are dead.
    ///
    ///Since we are seeding with this function, all results of this `region` are considered _live_.
    pub fn liveness_region(&self, region: RegionLocation) -> FlagStore<bool> {
        //setup the initial flags for all results.
        //The _type of result_ depends on the region's node type. So we use that to explore the definitions
        let results = self.node(region.node).result_types(region.region_index);
        let mut flags = FlagStore::new();
        for res in results.into_iter().map(|r| InportLocation {
            node: region.node,
            input: r,
        }) {
            flags.set(res.into(), true);
        }

        //now hit up the algorithm
        self.calc_liveness(flags)
    }

    ///The recursive algorithm, This is more or less the _mark_ phase of the dead-node elimination algorithm described
    /// in [the source paper](http://arxiv.org/abs/1912.05036v2) in _Algorithm VI_.
    fn calc_liveness(&self, mut flags: FlagStore<bool>) -> FlagStore<bool> {
        //we basically do a breadth-first traversal on _edge_ / _port_ granularity.
        //to make things a little easier on the big-O, we checkout _simple-nodes_ and apply-nodes only once,
        //which is also the vast majority of nodes usually.

        //seed by using all initially alive ports.
        let mut waiting_ports = flags
            .flags
            .iter()
            .filter_map(|(k, v)| {
                if *v {
                    if let AttribLocation::InPort(p) = k {
                        Some(*p)
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect::<VecDeque<InportLocation>>();

        //Now breadth-first search, but also track already seen nodes, so we don't traverse edges multiple times.
        //however, only collect simple-like nodes. In case of nodes with inner region, bridge into the region itself.
        let mut seen_nodes: AHashSet<NodeRef> = AHashSet::default();

        while let Some(next_port) = waiting_ports.pop_front() {
            //find the src of that port, if there is any.
            let src_port = if let Some(src) = self.node(next_port.node).inport(&next_port.input) {
                if let Some(edg) = src.edge {
                    self.edge(edg).src().clone()
                } else {
                    continue;
                }
            } else {
                continue;
            };

            //mark the src_port
            flags.set(src_port.clone().into(), true);

            //NOTE: by definition `waiting_ports` only contains _live-ports_.
            //      so `src_port` is live at that point as well.
            //      This makes the node of _src_port_ live as well. we now distinguish two cases:
            //      1. src_port.node has sub-region(s):
            //         a) port is output-like (is outside of that region): add all sub-region's result ports to the waiting list,
            //            depending on the rules described in Algorithm VI
            //         b) port is argument-like: add the corresbonding input of that node's argument to the work list, and
            //            mark argument _live_.
            //      2. src_port.node has no sub-region -> add all input-ports to the waiting_ports list, only IFF
            //         src_port.node was not seen yet. (This prevents us traversing edges twice needlessly).

            let subreg_count = self.node(src_port.node).regions().len();

            //if src_port is _in the region_

            if subreg_count > 0 {
                //alright, this is _the hard part_.
                // most of the hard part is in fact bridged by the OutputType::map_to_in_region helper.
                // however, we have to take care of two additional edge cases:
                // 1. always mark the gamma-predicate,
                // 2. always mark the theta-internal theta-predicate.
                // 3. when tagging a λ-decleration, tag all results as live. (TODO: thats not compleatly correct, we should tag based on the unified _uses_ of all apply nodes, and only tag _all_ if the λ is exported).
                //

                //If we are on a node with sub-regions, and the port we are currently considering is
                //a argument. Then we need to map this argument _out-port_ to the argument
                // in_port of that node. For instance entry-variables of a Gamma node
                if src_port.output.is_argument() {
                    //so first step, the carry-over process
                    for _subregidx in 0..subreg_count {
                        //check if we can carry over
                        if let Some(carried_over_port) = src_port.output.map_out_of_region() {
                            let new_inport = InportLocation {
                                node: src_port.node,
                                input: carried_over_port,
                            };
                            //mark and push back.
                            flags.set(new_inport.clone().into(), true);
                            waiting_ports.push_back(new_inport);
                        }
                    }
                }

                //Now, regardless of the port type,
                //mark the gamma-theta predicate live or mark all results of a
                // Lmd / Phi node live

                //now handle the edge_cases.
                //NOTE: this in facts marks the Theta predicate multiple times. But that does not matter,
                //      since the next simple-like note will break that trace anyways. And I'm pretty sure
                //      that's cheaper than tracking _which predicate was marked already_.
                match &self.node(src_port.node).node_type {
                    NodeType::Lambda(_) | NodeType::Delta(_) => {
                        //activate _all_ results for the λ/ϕ/δ nodes.
                        //TODO: Not entirely correct, we should analyse if those results are in fact used
                        //      by the caller (in case of the delta and lambda nodes).
                        let is_seen = seen_nodes.contains(&src_port.node);
                        if !is_seen {
                            for resport in self.node(src_port.node).result_types(0) {
                                //flag as live and push back
                                let new_inport = InportLocation {
                                    node: src_port.node,
                                    input: resport,
                                };
                                flags.set(new_inport.clone().into(), true);
                                waiting_ports.push_back(new_inport);
                            }
                        }
                    }
                    NodeType::Gamma(_g) => {
                        let is_seen = seen_nodes.contains(&src_port.node);
                        //mark gamma predicate always as live
                        if !is_seen {
                            let gamma_pred = InportLocation {
                                node: src_port.node,
                                input: InputType::GammaPredicate,
                            };
                            flags.set(gamma_pred.clone().into(), true);
                            waiting_ports.push_back(gamma_pred);
                        }
                    }
                    NodeType::Theta(t) => {
                        let theta_seen = seen_nodes.contains(&src_port.node);
                        if !theta_seen {
                            let theta_pred = InportLocation {
                                node: src_port.node,
                                input: InputType::ThetaPredicate,
                            };
                            flags.set(theta_pred.clone().into(), true);
                            waiting_ports.push_back(theta_pred);
                            //mark all somehow connected lv-results and lv-outputs
                            //Since Theta loops, a output might not be connected, but the
                            //result still be in use.
                            for lvidx in 0..t.loop_variable_count() {
                                let is_result_in_use = t
                                    .lv_result(lvidx)
                                    .map(|port| port.edge.is_some())
                                    .unwrap_or(false);
                                let is_output_in_use = t
                                    .lv_output(lvidx)
                                    .map(|port| port.edges.len() > 0)
                                    .unwrap_or(false);
                                //if either is in use, mark output as live
                                if is_result_in_use || is_output_in_use {
                                    let lvport = InportLocation {
                                        node: src_port.node,
                                        input: InputType::Result(lvidx),
                                    };
                                    flags.set(lvport.into(), true);
                                    waiting_ports.push_back(lvport);
                                }
                            }
                        }
                    }
                    NodeType::Phi(_) => {
                        //if src_port is an RV-Argument, make sure that the RV-Result (which defines the
                        // argument's caller) is live.
                        //WARN: Right now, the Phi node could potentually create a dead lock, by hooking up
                        //      an RV-Argument to an RV-result.
                        if let OutputType::RecursionVariableArgument(i) = src_port.output {
                            let new_inport = InportLocation {
                                node: src_port.node,
                                input: InputType::RecursionVariableResult(i),
                            };
                            flags.set(new_inport.clone().into(), true);
                            waiting_ports.push_back(new_inport);
                        }
                    }
                    //all others are handeled by the mapping above.
                    _ => {}
                }

                //Finally, since we have >0 sub regions, mark the
                //equivalent _in-region_ port live, if there is one

                //so first step, the carry-over process
                for subregidx in 0..subreg_count {
                    //check if we can carry over, to dat
                    if let Some(carried_over_port) = src_port.output.map_to_in_region(subregidx) {
                        let new_inport = InportLocation {
                            node: src_port.node,
                            input: carried_over_port,
                        };
                        //mark and push back.
                        flags.set(new_inport.clone().into(), true);
                        waiting_ports.push_back(new_inport);
                    }
                }

                //add it to seen nodes regardless
                seen_nodes.insert(src_port.node);
            } else {
                if !seen_nodes.contains(&src_port.node) {
                    seen_nodes.insert(src_port.node);
                    for inty in self.node(src_port.node).inport_types() {
                        let new_inport = InportLocation {
                            node: src_port.node,
                            input: inty,
                        };
                        //mark the port alive
                        flags.set(new_inport.clone().into(), true);
                        waiting_ports.push_back(new_inport);
                    }
                }
            }
        }

        flags
    }
}
