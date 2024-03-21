//! Liveness analysis.

use smallvec::SmallVec;

use crate::{
    attrib::FlagStore,
    edge::{InportLocation, InputType, LangEdge, OutportLocation},
    nodes::{LangNode, NodeType},
    region::RegionLocation,
    NodeRef, Rvsdg,
};

impl<N: LangNode + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Calculates the liveness of the whole graph. See [liveness_region] for more information.
    pub fn liveness(&self) -> FlagStore<bool> {
        //just execute for the toplevel region.
        self.liveness_region(self.toplevel_region())
    }

    ///Does liveness analysis on `region` and all sub-regions of it. Returns a [FlagStore](crate::attrib::FlagStore)
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
        self.calc_liveness(region, flags)
    }

    fn is_live_outport(flags: &FlagStore<bool>, port: OutportLocation) -> bool {
        if let Some(value) = flags.get(&port.into()) {
            *value
        } else {
            false
        }
    }

    fn is_live_inport(flags: &FlagStore<bool>, port: InportLocation) -> bool {
        if let Some(value) = flags.get(&port.into()) {
            *value
        } else {
            false
        }
    }

    ///Imports livess flags as set for the region's outputs to the region's results
    fn carry_liveness_into_region(
        &self,
        region: RegionLocation,
        mut flags: FlagStore<bool>,
    ) -> FlagStore<bool> {
        let outputs = self.node(region.node).outport_types();
        for out in outputs {
            //if there is an equivalent, port in the region, copy over liveness information, if there is none
            //already.
            let inner_port = if let Some(result_eq) = out.map_to_in_region(region.region_index) {
                InportLocation {
                    node: region.node,
                    input: result_eq,
                }
            } else {
                continue;
            };

            //if there is already a livness info, skip
            if let Some(liveness) = flags.get(&inner_port.clone().into()) {
                //Do not overwrite if already set as live
                if *liveness {
                    continue;
                }
            }

            if let Some(src_liveness) = flags.get(
                &OutportLocation {
                    node: region.node,
                    output: out,
                }
                .into(),
            ) {
                let old = flags.set(inner_port.into(), *src_liveness);
                assert!(old.is_none());
            }
        }

        //touch up the result, by always marking the theta-predicate as always live
        if let NodeType::Theta(_t) = &self.node(region.node).node_type {
            flags.set(
                InportLocation {
                    node: region.node,
                    input: InputType::ThetaPredicate,
                }
                .into(),
                true,
            );
        }

        flags
    }

    ///Exports the liveness information of the region's argument-like ports to the region's
    /// inputs.
    fn carry_liveness_out_of_region(
        &self,
        region: RegionLocation,
        mut flags: FlagStore<bool>,
    ) -> FlagStore<bool> {
        //Idea is the same as with carrying into the region. This time however, we also add
        //any gamma-predicate to the liveness

        let args = self.node(region.node).argument_types(region.region_index);

        for arg in args {
            let outer_port = if let Some(eq) = arg.map_out_of_region() {
                InportLocation {
                    node: region.node,
                    input: eq,
                }
            } else {
                continue;
            };

            if let Some(f) = flags.get(&outer_port.clone().into()) {
                //if already set as alive, then do not overwrite with anything.
                if *f {
                    continue;
                }
            }

            if let Some(src_liveness) = flags.get(
                &OutportLocation {
                    node: region.node,
                    output: arg,
                }
                .into(),
            ) {
                let old = flags.set(outer_port.into(), *src_liveness);
                assert!(old.is_none());
            }
        }

        if let NodeType::Gamma(_) = &self.node(region.node).node_type {
            //always set gamma predicate as life
            flags.set(
                InportLocation {
                    node: region.node,
                    input: InputType::ThetaPredicate,
                }
                .into(),
                true,
            );
        }

        flags
    }

    //Tags this node's arguments, and all ports that are connected to any argument as live
    fn tag_node_live(&self, node: NodeRef, flags: &mut FlagStore<bool>) {
        //tag our output_ports live, and all our input-ports
        let incount = self.node(node).inputs().len();
        let outcount = self.node(node).outputs().len();

        for input in self.node(node).inport_types() {
            flags.set(
                InportLocation {
                    node,
                    input: input.clone(),
                }
                .into(),
                true,
            );
            //and propagate to the connected nodes
            if let Some(inport) = self.node(node).inport(&input) {
                if let Some(edg) = inport.edge {
                    let src = self.edge(edg).src().clone();
                    let _old = flags.set(src.into(), true);
                }
            } else {
                #[cfg(feature = "log")]
                log::warn!("Detected invalid inport, but should be valid!");
            }
        }

        for output in self.node(node).outport_types() {
            flags.set(OutportLocation { node, output }.into(), true);
        }
    }

    ///The recursive algorithm, This is more or less the _mark_ phase of the dead-node elimination algorithm described
    /// in [the source paper](http://arxiv.org/abs/1912.05036v2) in _Algorithm VI_.
    fn calc_liveness(&self, region: RegionLocation, mut flags: FlagStore<bool>) -> FlagStore<bool> {
        //prepare by carrying over all liveness information
        let mut flags = self.carry_liveness_into_region(region, flags);

        //now continue by seeding the the walk through with all
        //result-conneted nodes.
        //
        // For each node we checkout, if the dst of any output-connected edge is set as live. If so,
        // the output is live as well, and therefore the node (and all of its inputs) are live as well.
        let rescount = self.region(&region).unwrap().results.len();
        for residx in 0..rescount {
            let seed_node =
                if let Some(sn) = self.region(&region).unwrap().result_src(&self, residx) {
                    sn.node
                } else {
                    //no can do
                    continue;
                };

            //Build the order in which we walk the region.
            let mut node_order = self
                .node(seed_node)
                .outport_types()
                .into_iter()
                .map(|pt| OutportLocation {
                    node: seed_node,
                    output: pt,
                })
                .collect::<Vec<_>>();
            node_order.append(&mut self.walk_predecessors(seed_node).collect::<Vec<_>>());

            //now walk all nodes.
            //for each, unify livness on each output-port. Then recurse if needed,
            //and finaly export livness if needed, or in case of apply/simple node,
            //just mark all inputs live, if any output is live.

            for node in node_order {
                let outtys = self.node(node.node).outport_types();
                let mut is_node_live = false;
                for outidx in 0..self.node(node).outputs().len() {
                    let any_live = false;
                    'anylive: for dst in self
                        .node(node)
                        .output_dsts(&self, outidx)
                        .unwrap_or(SmallVec::new())
                    {
                        for d in dst {
                            if Self::is_live_inport(&flags, d) {
                                any_live = true;
                                //if it is live on any port
                                is_node_live = true;
                                break 'anylive;
                            }
                        }
                    }

                    //mark
                    flags.set(
                        OutportLocation {
                            node,
                            output: outtys[outidx],
                        }
                        .into(),
                        any_live,
                    );
                }

                //now do the _inner-region-recursion_, since we did setup the output-ports already
                let subregcount = self.node(node).regions().len();
                for regidx in 0..subregcount {
                    flags = self.calc_liveness(
                        RegionLocation {
                            node,
                            region_index: regidx,
                        },
                        flags,
                    );
                }

                //recursion should have taken care of the _carry-out-step_
                //only if there are no regions we havet to do that our selfs
                if subregcount == 0 {
                    if is_node_live {
                        let intys = self.node(node).inport_types();
                        for inty in intys {
                            //overwrite as live
                            let _old = flags.set(InportLocation { node, input: inty }, true);
                        }
                    }
                }
            }
        }

        //finally end, by carrying the final liveness information _out_ of our
        //region
        let flags = self.carry_liveness_out_of_region(region, flags);

        flags
    }
}
