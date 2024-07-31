/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Implements _Common-Node-Elemination_. The pass effectively fuses nodes that are known to describe the same operation.
//!
//! See
//!
//! - [common_node_elemination](crate::Rvsdg::common_node_elemination)
//! - [cne_region](crate::Rvsdg::cne_region)
//!
//! For the implemetation to become available, the node type `N` of `Rvsdg<N, E>` has to implement [NodeTypeEq]. This allows the pass
//! to generically decide if two nodes describe the same operation.
use std::fmt::Debug;

use thiserror::Error;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::{LangNode, Node, NodeType, StructuralNode},
    region::RegionLocation,
    util::{abstract_node_type::AbstractNodeType, node_equality::RegionEqNodeTracker},
    NodeRef, Rvsdg, SmallColl,
};

use super::node_equality::NodeTypeEq;

#[derive(Error, Debug, Clone)]
pub enum CneError {
    #[error(transparent)]
    GraphErr(#[from] GraphError),
}

impl<N: LangNode + NodeTypeEq + Debug + 'static, E: LangEdge + 'static> Rvsdg<N, E> {
    ///Performs common-node-elemination on the whole graph. If successful, returns all eleminated nodes.
    pub fn common_node_elemination(&mut self) -> Result<Vec<Node<N>>, CneError> {
        let mut nodes = Vec::new();
        self.cne_region(self.toplevel_region(), &mut nodes)?;
        Ok(nodes)
    }

    ///Applies CNE to the region (and all its sub regions). Collects all eleminated nodes in `deleted_node`.
    pub fn cne_region(
        &mut self,
        region: RegionLocation,
        deleted_node: &mut Vec<Node<N>>,
    ) -> Result<(), CneError> {
        //Builds a lookup table of which are equal in type.
        let eq_tracker = RegionEqNodeTracker::build_for_region(self, region);
        //process all nodes in region, possibly recursing.
        //
        // we chose the topological order, in order to be sure, that any port we check for concurrency by
        // equivalent-mark-chaseing (see [chase_fold]) is already _fully seen_. So that we can be sure that any
        // port-sink, is in fact a sink, and won't be marked afterwards.
        let topord = self.topological_order_region(region);
        //NOTE: it could be that the topoord leaves dead nodes out, however, there should never be more nodes in the topo-ord.
        assert!(topord.len() <= self.region(&region).unwrap().nodes.len());

        //For each node, try to find _any_ equivalent.
        //if found, unify them and continue
        for node in topord.into_iter() {
            if !self.nodes.contains_key(node) {
                continue;
            }
            match self.node(node).into_abstract() {
                AbstractNodeType::Simple | AbstractNodeType::Apply => {
                    //For simple nodes we unify based on the equality tracker + the nodes
                    //input signature.
                    //NOTE: Apply nodes in this case function like simple nodes. While their first input is a
                    //      _callable_, they are also defined through their called CV-Arg or LambdaDef, so
                    //      they would have been unifed before, if any candidate λ is indeed the same.
                    //
                    //      Different side-effects would be visible on the state edges, so those are taken care of
                    //      implicitly as well.
                    let signature = self.build_src_map(node, self.node(node));
                    //TODO: cache the signatures, so we don't have to _discover_ them everytime
                    for candidate in eq_tracker.get_candidates(node) {
                        //Ignore self, or ignore if the candidate doesn't exist anymore
                        if candidate == &node || !self.nodes.contains_key(*candidate) {
                            continue;
                        }
                        //build the candidates signature. If its matches, unify `node` to the candidate
                        let candidate_sig = self.build_src_map(*candidate, self.node(*candidate));
                        if candidate_sig == signature {
                            let removed = self.unify_simple(node, *candidate)?;
                            deleted_node.push(removed);
                            //and stop
                            break;
                        }
                    }
                }
                //For them, unify context variables, then recurse into the sub regions
                AbstractNodeType::Delta | AbstractNodeType::Lambda | AbstractNodeType::Phi => {
                    let cv_count = match &self.node(node).node_type {
                        NodeType::Phi(p) => p.context_variable_count(),
                        NodeType::Lambda(l) => l.context_variable_count(),
                        NodeType::Delta(d) => d.context_variable_count(),
                        _ => panic!(),
                    };

                    //This is pretty simple, we just linearly search if any cv-idx is followed by a cv-arg,
                    //that is connected to the same src.
                    'unify_loop: for cvarg in 0..cv_count {
                        //NOTE: only search if actually connected though
                        if let Some(src) = self.inport_src(InportLocation {
                            node,
                            input: InputType::ContextVariableInput(cvarg),
                        }) {
                            for following_cvarg in (cvarg + 1)..cv_count {
                                if let Some(follow_source) = self.inport_src(InportLocation {
                                    node,
                                    input: InputType::ContextVariableInput(following_cvarg),
                                }) {
                                    if follow_source == src {
                                        self.divert_cv(node, cvarg, following_cvarg)?;
                                        //and stop searching for this cv
                                        continue 'unify_loop;
                                    }
                                }
                            }
                        }
                    }

                    //Now recurse into the region
                    self.cne_region(
                        RegionLocation {
                            node,
                            region_index: 0,
                        },
                        deleted_node,
                    )?;
                }
                //Unify loop variable-inputs _before recursing_, then recurse into the body, afterwards
                // unify the loop outputs
                AbstractNodeType::Theta => {
                    let lv_count = self
                        .node(node)
                        .node_type
                        .unwrap_theta_ref()
                        .loop_variable_count();
                    'lv_eq_search: for lvidx in 0..lv_count {
                        if let Some(srcport) = self.inport_src(InportLocation {
                            node,
                            input: InputType::Input(lvidx),
                        }) {
                            for follow_idx in (lvidx + 1)..lv_count {
                                if let Some(follower_port) = self.inport_src(InportLocation {
                                    node,
                                    input: InputType::Input(follow_idx),
                                }) {
                                    if srcport == follower_port {
                                        self.divert_lv(node, lvidx, follow_idx)?;
                                        continue 'lv_eq_search;
                                    }
                                }
                            }
                        }
                    }
                    self.cne_region(
                        RegionLocation {
                            node,
                            region_index: 0,
                        },
                        deleted_node,
                    )?;

                    //now univy output variables that have the same source in a similar way
                    let lo_count = self.node(node).node_type.unwrap_theta_ref().outputs().len();
                    'lo_eq_search: for loidx in 0..lo_count {
                        if let Some(src) = self.inport_src(InportLocation {
                            node,
                            input: InputType::Result(loidx),
                        }) {
                            for followeridx in (loidx + 1)..lo_count {
                                if let Some(follower_src) = self.inport_src(InportLocation {
                                    node,
                                    input: InputType::Result(followeridx),
                                }) {
                                    if follower_src == src {
                                        self.divert_lo(node, loidx, followeridx)?;
                                        continue 'lo_eq_search;
                                    }
                                }
                            }
                        }
                    }
                }
                //Right now, just recurse into the branches, but not sure
                //how we _could_ unify those really.
                //NOTE: I'm pretty sure the reference algorithm's mark phase is buggy there
                AbstractNodeType::Gamma => {
                    for region_index in 0..self.node(node).regions().len() {
                        self.cne_region(RegionLocation { node, region_index }, deleted_node)?;
                    }
                }
                AbstractNodeType::Omega => {
                    //shouldn't happen, but here we go
                    self.cne_region(
                        RegionLocation {
                            node,
                            region_index: 0,
                        },
                        deleted_node,
                    )?;
                }
            }
        }

        Ok(())
    }

    fn unify_simple(&mut self, from: NodeRef, to: NodeRef) -> Result<Node<N>, CneError> {
        //NOTE: for sanity
        assert!(self.node(from).outputs().len() == self.node(to).outputs().len());

        for output in self.node(from).outport_types() {
            //NOTE: for sanity
            assert!(self.node(to).outport(&output).is_some());
            //Divert all outputs
            self.divert(
                OutportLocation { node: to, output },
                OutportLocation { node: from, output },
            )?;
        }

        //now disconnect the from node from the graph
        let disconnected = self.remove_node(from)?;

        //NOTE for sanity, make sure they are still equal
        assert!(disconnected.node_type.is_simple() || disconnected.node_type.is_apply());
        match &disconnected.node_type {
            NodeType::Simple(s) => {
                assert!(s.type_equal(self.node(to).node_type.unwrap_simple_ref()))
            }
            NodeType::Apply(_a) => {
                //TODO: add a good check that is not _too expensive_ instead
            }
            _ => panic!(
                "Expected unified node to be simple or apply, was {}",
                disconnected.node_type
            ),
        }
        Ok(disconnected)
    }

    fn divert_lv(&mut self, node: NodeRef, from: usize, to: usize) -> Result<(), CneError> {
        //similar to the CVs
        self.divert(
            OutportLocation {
                node,
                output: OutputType::Argument(to),
            },
            OutportLocation {
                node,
                output: OutputType::Argument(from),
            },
        )?;

        //Now, for sanity make sure the inputs are indeed the same, then
        //remove the old one.

        assert!(
            self.inport_src(InportLocation {
                node,
                input: InputType::Input(to),
            }) == self.inport_src(InportLocation {
                node,
                input: InputType::Input(from),
            })
        );

        if let Some(old_edge) = self
            .node(node)
            .inport(&InputType::Input(from))
            .unwrap()
            .edge
        {
            self.disconnect(old_edge)?;
        } else {
            panic!("There should have been an edge");
        }
        Ok(())
    }

    fn divert_lo(&mut self, node: NodeRef, from: usize, to: usize) -> Result<(), CneError> {
        //similar to the CVs
        self.divert(
            OutportLocation {
                node,
                output: OutputType::Output(to),
            },
            OutportLocation {
                node,
                output: OutputType::Output(from),
            },
        )?;

        //Now, for sanity make sure the inputs are indeed the same, then
        //remove the old one.

        assert!(
            self.inport_src(InportLocation {
                node,
                input: InputType::Result(to),
            }) == self.inport_src(InportLocation {
                node,
                input: InputType::Result(from),
            })
        );

        if let Some(old_edge) = self
            .node(node)
            .inport(&InputType::Result(from))
            .unwrap()
            .edge
        {
            self.disconnect(old_edge)?;
        } else {
            panic!("There should have been an edge");
        }
        Ok(())
    }

    fn divert_cv(&mut self, node: NodeRef, from: usize, to: usize) -> Result<(), CneError> {
        self.divert(
            OutportLocation {
                node,
                output: OutputType::ContextVariableArgument(to),
            },
            OutportLocation {
                node,
                output: OutputType::ContextVariableArgument(from),
            },
        )?;

        //Now, for sanity make sure the inputs are indeed the same, then
        //remove the old one.

        assert!(
            self.inport_src(InportLocation {
                node,
                input: InputType::ContextVariableInput(to),
            }) == self.inport_src(InportLocation {
                node,
                input: InputType::ContextVariableInput(from),
            })
        );

        if let Some(old_edge) = self
            .node(node)
            .inport(&InputType::ContextVariableInput(from))
            .unwrap()
            .edge
        {
            self.disconnect(old_edge)?;
        } else {
            panic!("There should have been an edge");
        }
        Ok(())
    }

    ///Builds the input-src map, that is already unified to the currently known port sinks. So any port in the output
    /// collection is in fact a sink
    fn build_src_map(
        &self,
        node: NodeRef,
        noderef: &Node<N>,
    ) -> SmallColl<Option<OutportLocation>> {
        let mut srcs = SmallColl::default();
        for input in noderef.inport_types() {
            if let Some(src) = self.inport_src(InportLocation { node, input }) {
                srcs.push(Some(src))
            } else {
                srcs.push(None)
            }
        }

        srcs
    }

    fn divert(
        &mut self,
        to_port: OutportLocation,
        from_port: OutportLocation,
    ) -> Result<(), CneError> {
        //Diverting works by disconnecting all edges of `from_port`,
        // and reconnecting them to `to_port`.
        for edge in self
            .node(from_port.node)
            .outport(&from_port.output)
            .unwrap()
            .edges
            .clone()
        {
            let dst = self.edge(edge).dst;
            let val = self.disconnect(edge)?;
            self.connect(to_port, dst, val)?;
        }

        Ok(())
    }
}
