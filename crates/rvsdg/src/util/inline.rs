/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Generic inliner.

use core::panic;

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::{LangNode, NodeType, StructuralNode},
    region::RegionLocation,
    NodeRef, Rvsdg, SmallColl,
};
use ahash::AHashMap;
use smallvec::SmallVec;

use super::{copy::StructuralClone, Path};

use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum InlineError {
    #[error("Node {0} was not an apply node")]
    NodeWasNotApplyNode(NodeRef),
    #[error("The call-port of the apply-node was not connected")]
    CallPortNotConnected,
    #[error("Could not find the Lambda or Phi definition port")]
    NoDefPortFound,
    #[error("Phi node inlining not supported (yet)")]
    WasPhiNode,
    #[error("Failed to import context for inlined apply-node: {0}")]
    FailedToImportContext(GraphError),
}

impl<
        N: LangNode + StructuralClone + 'static,
        E: LangEdge + StructuralClone + PartialEq + 'static,
    > Rvsdg<N, E>
{
    ///inlines the given apply-node. Returns an error if `apply_node` is not in fact an apply node, or
    /// if the apply-node's calldef couldn't be found.
    ///
    ///
    /// NOTE: the inliner currently does not handle Phi nodes.
    ///
    /// Returns any created path of edges. This mostly happens if the inlined calldef uses context.
    pub fn inline_apply_node(&mut self, node: NodeRef) -> Result<SmallColl<Path>, InlineError> {
        let dst_region = self
            .node(node)
            .parent
            .expect("expected node to have a parent when inlining");

        //first step is to find the call def.
        let (call_lmd, arg_srcs, res_dsts) = if let NodeType::Apply(a) = &self.node(node).node_type
        {
            let callport = a.get_callabel_decl();
            let call_src = {
                if let Some(calledge) = callport.edge {
                    self.edge(calledge).src().clone()
                } else {
                    return Err(InlineError::CallPortNotConnected);
                }
            };

            //now trace it, NOTE that only the first input _must_ be a callable.
            let call_lmd_def = if let Some(calldef) = self.find_callabel_def(call_src) {
                calldef.node
            } else {
                return Err(InlineError::NoDefPortFound);
            };

            //Also collect where the apply node connects to/was connected from.
            let mut arg_srcs: SmallVec<[Option<(OutportLocation, E)>; 3]> = SmallVec::new();
            let mut res_dsts: SmallVec<[SmallVec<[(InportLocation, E); 3]>; 3]> = SmallVec::new();
            for i in 0..a.get_call_arg_count() {
                if let Some(src) = a.argument_input(i).unwrap().edge {
                    let srcport = self.edge(src).src().clone();
                    let ty = self.edge(src).ty.structural_copy();
                    arg_srcs.push(Some((srcport, ty)));
                } else {
                    arg_srcs.push(None);
                }
            }

            for i in 0..a.outputs.len() {
                let mut dsts = SmallVec::default();
                for dstedg in &a.outputs[i].edges {
                    let edg = self.edge(*dstedg);

                    dsts.push((edg.dst().clone(), edg.ty.structural_copy()));
                }
                res_dsts.push(dsts);
            }

            (call_lmd_def, arg_srcs, res_dsts)
        } else {
            return Err(InlineError::NodeWasNotApplyNode(node));
        };

        if let NodeType::Phi(_) = &self.node(call_lmd).node_type {
            return Err(InlineError::WasPhiNode);
        }

        //we now have all information, so delete the apply node
        let _ = self.remove_node(node).unwrap();

        //We now do something similar to the deep_copy_region function. But instead
        // of hooking up to _the same_ region_arguments, we use the apply nodes's arguments to hook up our edges.

        //NOTE: this also works for phi-nodes, since those have also only one region.
        let lmd_region = RegionLocation {
            node: call_lmd,
            region_index: 0,
        };
        let mut argument_remapping: AHashMap<OutportLocation, Option<(OutportLocation, E)>> =
            AHashMap::default();
        let mut cv_remapping: AHashMap<OutportLocation, OutportLocation> = AHashMap::default();
        let mut result_remapping: AHashMap<InportLocation, SmallVec<[(InportLocation, E); 3]>> =
            AHashMap::default();

        //build the remapping structure, by iterating the call_lmd's args/results, and inserting the call_lmd's port-location
        // as key, and the corresponding arg_src / res_dst as value. This allows us to catch all args and
        // dst later when reconnecting all node, an connect them to the correct dst immediately.

        let (src_lambda_arg_count, src_lambda_result_count) = match &self.node(call_lmd).node_type {
            NodeType::Phi(phi) => (phi.argument_count(), phi.result_count()),
            NodeType::Lambda(lmd) => (lmd.argument_count(), lmd.result_count()),
            _ => panic!("Unexpected node type for callable!"),
        };
        assert!(src_lambda_arg_count == arg_srcs.len());
        assert!(src_lambda_result_count == res_dsts.len());

        //Check that there are no rvs
        let cvcount = match &self.node(call_lmd).node_type {
            NodeType::Phi(phi) => {
                assert!(
                    phi.rv_count == 0,
                    "Inliner does not support recursion variables!"
                );
                phi.context_variable_count()
            }
            NodeType::Lambda(l) => l.context_variable_count(),
            _ => panic!("Unexpected node type for callable in inliner"),
        };

        //Add the argument remapping
        for (argidx, src) in arg_srcs.into_iter().enumerate() {
            let lmd_port = OutportLocation {
                node: call_lmd,
                //Checked beforehand that we don't have cv-s or rv-s
                output: OutputType::Argument(argidx),
            };
            argument_remapping.insert(lmd_port, src);
        }

        let mut paths = SmallColl::new();
        //Add context variable remapping
        //This is a little more involved. We add a new CV to the dst_lmd for each
        //context variable that is connected in the src_region.
        for cvidx in 0..cvcount {
            //Try to find a producer for the orginal cvidx. If there is none, we can ignore that
            let src_cv_port = OutportLocation {
                node: call_lmd,
                output: OutputType::ContextVariableArgument(cvidx),
            };
            if let Some(connected_callable) = self.find_producer_out(src_cv_port) {
                //was connected, import the callable into the dst region and add the remapping
                match self.import_context(connected_callable, dst_region) {
                    Ok((dst_cv, path)) => {
                        //add to remapping
                        let is_new = cv_remapping.insert(src_cv_port, dst_cv);
                        //and push path if needed
                        if let Some(p) = path {
                            paths.push(p);
                        }
                        assert!(is_new.is_none());
                    }
                    Err(e) => {
                        return Err(InlineError::FailedToImportContext(e));
                    }
                }
            }
            //now import the source of the orginal cv-input to the just created one
        }

        for (res_idx, dsts) in res_dsts.into_iter().enumerate() {
            let resport = InportLocation {
                node: call_lmd,
                input: InputType::Result(res_idx),
            };
            result_remapping.insert(resport, dsts);
        }

        //Finally, enter the deep-copy phase. First deep-copy all nodes over,
        // and track the remapping. Then copy over all edges and use our port remapping as well as
        // node remapping
        let mut node_mapping = AHashMap::default();
        let src_nodes = self.region(&lmd_region).unwrap().nodes.clone();
        for srcnode in src_nodes {
            let dstnode = self.deep_copy_node(srcnode, dst_region);
            node_mapping.insert(srcnode, dstnode);
        }

        let src_edges = self.region(&lmd_region).unwrap().edges.clone();
        for srcedg in src_edges {
            //we have two paths. if the srcedg.src() is a argument, or the srcedg.dst() is an result,
            // we use the remapped regions.
            let (mut src, mut dst, ty) = {
                let edg = self.edge(srcedg);
                (
                    edg.src().clone(),
                    edg.dst().clone(),
                    edg.ty.structural_copy(),
                )
            };

            //remap src.
            //We first check if its a context variable, if so we use that
            //map to do the remapping. Otherwise we check if its some _other_
            //kind of region argument. For that we use the _argumenti_ remapping. And finally
            //if it ain't that, its some kind of _normal_ region-internal sourec port, so we use that.
            if let OutputType::ContextVariableArgument(_) = src.output {
                let new_src_port = cv_remapping.get(&src).unwrap();
                src = new_src_port.clone();
            } else if src.output.is_argument() {
                //use the argument remapping
                if let Some((remap_port, remap_ty)) = argument_remapping.get(&src).unwrap() {
                    //NOTE: This was an assert before. However, those frequently do not match,
                    //      If there is a more involved edge system. So we now warn if they are unequal,
                    //      and only panic, if there is a state/value-edge missmatch
                    if remap_ty.is_value_edge() != ty.is_value_edge() {
                        panic!(
                            "Inliner: Value/State edge missmatch for apply-node output {:?}",
                            remap_port.output
                        )
                    }
                    #[cfg(feature = "log")]
                    if remap_ty != &ty {
                        log::warn!(
                            "Edge type missmatch on port {:?} while inlining!",
                            remap_port
                        );
                    }
                    src = remap_port.clone();
                } else {
                    //no remapping needed, since the arg was not connected initially
                    continue;
                }
            } else {
                //use the node substitution instead
                src.node = *node_mapping.get(&src.node).unwrap();
            }

            //remap dst. This is a little more involved, since we might map 1:n.
            // this is also the reason why we do it last, cause we can fan out nicely here, since
            // src is already set.

            if dst.input.is_result() {
                //checkout the rempping
                for (remapped_edg, remapped_ty) in result_remapping.get(&dst).unwrap() {
                    //use the remapped dst, and the initial src, to connect a copy of ty

                    if remapped_ty.is_value_edge() != ty.is_value_edge() {
                        panic!(
                            "Inliner: Value/State edge missmatch for apply-node input {:?}",
                            remapped_edg.input
                        )
                    }
                    #[cfg(feature = "log")]
                    if remapped_ty != &ty {
                        log::warn!(
                            "Edge type missmatch on port {:?} while inlining!",
                            remapped_edg
                        );
                    }
                    self.connect(src.clone(), remapped_edg.clone(), ty.structural_copy())
                        .unwrap();
                }
            } else {
                //In this case, simply remap
                dst.node = *node_mapping.get(&dst.node).unwrap();
                //and immediatly connect
                self.connect(src, dst, ty).unwrap();
            }
        }

        Ok(paths)
    }
}
