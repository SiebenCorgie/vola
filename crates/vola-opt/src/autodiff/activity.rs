/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::collections::VecDeque;

use ahash::AHashSet;
use rvsdg::{
    attrib::FlagStore,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    util::abstract_node_type::AbstractNodeType,
    NodeRef, SmallColl,
};

use crate::{autodiff::AutoDiffError, OptError, Optimizer};

impl Optimizer {
    ///Walks all predecessors of `entrypoint`, and tag all nodes and output-ports that are somehow influenced by `active_value`.
    ///
    ///The explorer enters sub regions, for instance gamma or theta nodes, and traces apply nodes.
    ///
    /// Assumes that `entrypoint` and `active_value` are in the same region.
    pub fn activity_explorer(
        &self,
        entrypoint: NodeRef,
        active_value: OutportLocation,
    ) -> Result<FlagStore<bool>, OptError> {
        //The strategy is to collect active_value's successors until we find `entry_point`, or exist the region.
        //
        //If we exit the region without finding it, there is no intersection of both, so we can return `none`.
        //If we find `entry_point`, we iterate the intersecting nodes, and possibly
        // - propagate _into_ regions (gamma/theta,context-variables)
        // - propagate _into_ λ/ϕ nodes (find the active argument(s) of apply node, pre_tag them in the region, then recurse the general tagger).

        //NOTE: `active_value might be a input argument to the region, so its okay if its an _inside_port_ of `entrypoint's` parent region.

        let active_value_region = if active_value.output.is_argument() {
            let region_index = active_value
                .output
                .argument_region_index()
                .expect("If is argument, expect argument index!");
            RegionLocation {
                node: active_value.node,
                region_index,
            }
        } else {
            self.graph.node(active_value.node).parent.unwrap()
        };

        let entrypoint_region = self.graph.node(entrypoint).parent.unwrap();

        //Test that both are in the same region.
        if active_value_region != entrypoint_region {
            return Err(OptError::from(
                AutoDiffError::ActivityExplorationRegionError,
            ));
        }

        let initial_successors =
            self.active_element_successors(&[active_value], active_value_region);

        //If there is no intersection of the initial successor list and the
        //entrypoint, this means that the entrypoint is independent from the active element.
        //
        //in that case we can just return an empty flags store.
        if !initial_successors.contains(&entrypoint) {
            return Ok(FlagStore::new());
        }

        //Tag all initial successors with _active_, then iterate
        //them and handle any sub-region nodes or apply nodes

        let mut active = FlagStore::new();

        active.set(active_value.into(), true);

        //Now start recursion for any node that _needs_ it
        for is in &initial_successors {
            active = self.handle_active_node(*is, active);
        }

        Ok(active)
    }

    fn active_element_successors(
        &self,
        active_values: &[OutportLocation],
        parent_region: RegionLocation,
    ) -> AHashSet<NodeRef> {
        let mut successors = AHashSet::default();
        let mut stack: VecDeque<_> = VecDeque::new();
        //Init stack with all active-value connected nodes
        for av in active_values {
            for dst in self.graph.outport_dsts(*av) {
                stack.push_back(dst);
            }
        }

        while let Some(next) = stack.pop_front() {
            //don't re_trace the same node
            if successors.contains(&next.node) {
                continue;
            }
            //Don't cross boundary
            if next.node == parent_region.node {
                continue;
            }

            //push all connected nodes and push self
            successors.insert(next.node);
            for succ in self.graph.node(next.node).succ(&self.graph) {
                stack.push_back(succ);
            }
        }

        successors
    }

    fn handle_active_node(&self, node: NodeRef, mut flags: FlagStore<bool>) -> FlagStore<bool> {
        match self.graph.node(node).into_abstract() {
            //Simple nodes don't need special care
            AbstractNodeType::Simple => {
                //set node and all outputs
                flags.set(node.into(), true);
                for output in self.graph.node(node).outport_types() {
                    flags.set(OutportLocation { node, output }.into(), true);
                }

                flags
            }
            //Import all tagged arguments to the apply node into the λ's  (or Phi's) region
            AbstractNodeType::Apply => {
                //map all active arguments of the _call_ (this apply node) in the called λ.
                //
                //NOTE: the DSL does not allow recursion, so this is okay. Otherwise we'd have to track _who_ / _at-which-point-in-time_ is calling.
                //
                // Then handle the λ's body internally, finally when returning, tag all apply-node outputs as active, where the
                // producer of the result in the λ body is active as well.

                let argument_count = self
                    .graph
                    .node(node)
                    .node_type
                    .unwrap_apply_ref()
                    .get_call_arg_count();

                let called_lambda = self
                    .graph
                    .find_producer_inp(InportLocation {
                        node,
                        input: InputType::Input(0),
                    })
                    .expect("Expected apply-nodes lambda decleration to be connected!");

                assert!(self.graph.node(called_lambda.node).is_callable());
                assert!(called_lambda.output == OutputType::LambdaDeclaration);

                for argidx in 0..argument_count {
                    //Check if the arg is connected
                    if let Some(argedge) = self
                        .graph
                        .node(node)
                        .node_type
                        .unwrap_apply_ref()
                        .argument_input(argidx)
                        .unwrap()
                        .edge
                    {
                        //get the src node and check if it is active
                        let src = self.graph.edge(argedge).src();
                        if flags.is_set(&src.into()) {
                            //Set the internal argument in the λ as active
                            flags.set(
                                OutportLocation {
                                    node: called_lambda.node,
                                    //NOTE the argidx is right here, since we don't need to offset by one _within_ the lambda's region.
                                    //     Since we use the OutputType we also don't overwrite the CV-inputs.
                                    output: OutputType::Argument(argidx),
                                }
                                .into(),
                                true,
                            );
                        }
                    }
                }

                //handle the λ's body
                flags = self.activity_region_handler(
                    RegionLocation {
                        node: called_lambda.node,
                        region_index: 0,
                    },
                    flags,
                );

                //Tag all outputs as active, that are active in the lambda as well.
                for outidx in 0..self.graph.node(node).outputs().len() {
                    let is_output_active = if let Some(src) =
                        self.graph.inport_src(InportLocation {
                            node: called_lambda.node,
                            input: InputType::Result(outidx),
                        }) {
                        //Check if the result producer is active.
                        flags.is_set(&src.into())
                    } else {
                        //not connected, so not active
                        false
                    };

                    if is_output_active {
                        //Set the apply-node's output as active if the producer in the λ is active
                        flags.set(
                            OutportLocation {
                                node,
                                output: OutputType::Output(outidx),
                            }
                            .into(),
                            true,
                        );
                    }
                }

                flags
            }
            //Based on https://arxiv.org/abs/1810.07951
            //we trace the activity _into_ each branch, and unify it at the outputs
            AbstractNodeType::Gamma => {
                todo!("gamma activity tracing not implemented")
            }

            AbstractNodeType::Theta => {
                todo!("theta activity tracing not implemented")
            }

            //This only happens if they are _within_ a region and use context. In that case tag the context ports.
            // but don't recurse.
            // The inner-region gets handled only iff the node is actually called.
            AbstractNodeType::Lambda | AbstractNodeType::Phi | AbstractNodeType::Delta => {
                for input in self.graph.node(node).inport_types() {
                    //Try to map the port _into_ the region. If that is not possible,
                    //we can ignore it
                    if let Some(inner) = input.map_to_in_region(0) {
                        let port = InportLocation { node, input };
                        //If there is a source for that port, check if its active, if so, mark the _in-region_
                        //port as active as well. This will be picked up by the region-handler later on.
                        if let Some(src) = self.graph.inport_src(port) {
                            if flags.get(&src.node.into()).is_some() {
                                let inner_port = OutportLocation {
                                    node,
                                    output: inner,
                                };
                                flags.set(inner_port.into(), true);
                            }
                        }
                    }
                }

                flags
            }
            //Shouldn't happen / has no relevance.
            AbstractNodeType::Omega => flags,
        }
    }

    ///Does activity tracing by inspecting the region's argument-ports activity state and pushing those
    /// through the net
    fn activity_region_handler(
        &self,
        region: RegionLocation,
        mut flags: FlagStore<bool>,
    ) -> FlagStore<bool> {
        let mut active_ports = SmallColl::new();

        for output in self
            .graph
            .node(region.node)
            .argument_types(region.region_index)
        {
            let port = OutportLocation {
                node: region.node,
                output,
            };

            if flags.is_set(&port.into()) {
                active_ports.push(port);
            }
        }

        //Do the same thing as the initial collector, but for all live ports
        let active_nodes_in_region = self.active_element_successors(&active_ports, region);

        //Now start recursion for any node that _needs_ it, or tag the simple-node and its outputs
        for an in active_nodes_in_region {
            flags = self.handle_active_node(an, flags);
        }

        flags
    }
}
