/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Implements gamma-node (if-then-else) utility functions. For instance specializing for a chosen branch.

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    nodes::LangNode,
    region::RegionLocation,
    NodeRef, Rvsdg,
};

use super::copy::StructuralClone;

impl<N: LangNode + StructuralClone + 'static, E: LangEdge + StructuralClone + 'static> Rvsdg<N, E> {
    ///Specializes `gamma_node` to pull out `branch`. This effectively be semantic equal to always chosing `branch`.
    ///
    /// Thechnically `gamma_node` will be compleatly disconnected from the graph. All result-users will be connected to the apropriate
    /// `branch`-internal results, all arguments will be connected to the users of the branch _within_ `branch`.
    ///
    /// Panics if:
    ///
    /// - `gamma_node` is in fact no gamma-node
    /// - `gamma-node` has no parent region,
    pub fn gamma_specialize_for_branch(&mut self, gamma_node: NodeRef, branch: usize) {
        assert!(self.node(gamma_node).node_type.is_gamma());

        //The idea is pretty easy, we just deep-copy the content of `branch` into our host region, then rewrite all
        // input-connections to the just copied nodes, and all result-connections to all
        // output-connected nodes.

        let parent = self.node(gamma_node).parent.unwrap();
        let node_mapping = self.deep_copy_region_without_connection(
            RegionLocation {
                node: gamma_node,
                region_index: branch,
            },
            parent,
        );

        //reconnect all argument connected nodes
        for argidx in 0..self.node(gamma_node).regions()[branch].arguments.len() {
            for edg in self.node(gamma_node).regions()[branch].arguments[argidx]
                .edges
                .clone()
            {
                //check if we can find a connected src for the edg
                if let Some(src) = self.inport_src(InportLocation {
                    node: gamma_node,
                    input: InputType::EntryVariableInput(argidx),
                }) {
                    //now map the edges's dst node to the just-copied node, and clone the edge payload for reconnection
                    let (dst, payload) = {
                        let original_dst = self.edge(edg).dst.clone();
                        let payload = self.edge(edg).ty.structural_copy();
                        if let Some(mapped_dst) = node_mapping.get(&original_dst.node) {
                            let mut new_dst = original_dst.clone();
                            new_dst.node = mapped_dst.clone();
                            (new_dst, payload)
                        } else {
                            //Skip if we can't map
                            continue;
                        }
                    };

                    //now reconnect the new-dst to the just _found_ src
                    self.connect(src, dst, payload).unwrap();
                }
            }
        }

        //reconnect all result_connected nodes
        for outidx in 0..self.node(gamma_node).outputs().len() {
            if self.node(gamma_node).outputs()[outidx].edges.len() == 0 {
                continue;
            }

            //find the in-gamma-src,
            if let Some(in_region_src) =
                self.node(gamma_node).regions()[branch].result_src(self, outidx)
            {
                //We now have two cases:
                // 1. the result was connected to an entry-var of our self.
                //    In that case, find the out-of-gamma src for that, and hook up all output connected nodes
                // 2. the argument was connected to some kind of node,
                //    In that case, reconnect to that some kind of node based on the mapping.

                let new_src = if in_region_src.node == gamma_node
                    && in_region_src.output.is_argument()
                {
                    //is actually an argument, map that out of region, find its source and use that for the remapping
                    let mapped_out = in_region_src.output.map_out_of_region().unwrap();
                    let src = self
                        .inport_src(gamma_node.as_inport_location(mapped_out))
                        .unwrap();
                    src
                } else {
                    //map in _region_src to out_of_region src, then disconnect all results on _outidx_ and connect them
                    //to the mapped.
                    let mapped_node =
                        if let Some(out_of_region_node) = node_mapping.get(&in_region_src.node) {
                            out_of_region_node.clone()
                        } else {
                            continue;
                        };

                    let mut new_src = in_region_src.clone();
                    new_src.node = mapped_node;
                    new_src
                };

                for edg in self.node(gamma_node).outputs()[outidx].edges.clone() {
                    let dst = self.edge(edg).dst().clone();
                    let payload = self.disconnect(edg).unwrap();
                    self.connect(new_src, dst, payload).unwrap();
                }
            }
        }

        //finally remove the whole gamma node
        self.remove_node(gamma_node).unwrap();
    }
}

impl<N: LangNode, E: LangEdge> Rvsdg<N, E> {
    ///Returns true if the exit variable is connected to the same entry variable in both branches.
    ///
    ///Returns false if the port is either is not a exit_variable, or the former case is not given.
    pub fn is_ev_untouched(&self, exit_variable: OutportLocation) -> bool {
        match exit_variable.output {
            OutputType::ExitVariableOutput(_) => {}
            _ => return false,
        }

        let mut found_entry_variable_index = None;
        for region_index in 0..self[exit_variable.node].regions().len() {
            if let Some(inregion) = exit_variable.output.map_to_in_region(region_index) {
                if let Some(source) = self.inport_src(inregion.to_location(exit_variable.node)) {
                    match source.output {
                        OutputType::EntryVariableArgument { .. } => {
                            if let Some(found) = found_entry_variable_index {
                                //We already have a ev we search for. However, its not the same. So bail
                                if found != source.output {
                                    return false;
                                }
                            } else {
                                //If this is not the first region, this means that the first region was not connected to an ev, so we
                                //must bail.
                                if region_index != 0 {
                                    return false;
                                } else {
                                    //If this is the first region, and it is connected to an ev, set the found ev.
                                    found_entry_variable_index = Some(source.output);
                                }
                            }
                        }
                        //Bail if not connected to a entry variable argument
                        _ => return false,
                    }
                }
            } else {
                //Could not map, so some kind of invalid setup.
                #[cfg(feature = "log")]
                log::warn!("Could not map {exit_variable:?} to region {region_index}, possibly inconsistent node-state");

                return false;
            }
        }

        true
    }
}
