//! Generic inliner.

use crate::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    err::GraphError,
    nodes::{LangNode, NodeType, StructuralNode},
    region::RegionLocation,
    NodeRef, Rvsdg,
};
use ahash::AHashMap;
use smallvec::SmallVec;

use super::copy::StructuralClone;

use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum InlineError {
    #[error("Node {0} was not an apply node")]
    NodeWasNotApplyNode(NodeRef),
    #[error("The call-port of the apply-node was not connected")]
    CallPortNotConnected,
    #[error("Could not find the Lambda or Phi definition port")]
    NoDefPortFound,
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
    /// NOTE: the inliner currently does not handle context and recursion variables.
    pub fn inline_apply_node(&mut self, node: NodeRef) -> Result<(), InlineError> {
        let dst_region = self
            .node(node)
            .parent
            .expect("expected node to have a parent when inlining");

        //first step is to find the call def.
        let (call_lmd, argcount, result_count, arg_srcs, res_dsts) = if let NodeType::Apply(a) =
            &self.node(node).node_type
        {
            let callport = a.get_callabel_decl();
            let call_src = {
                if let Some(calledge) = callport.edge {
                    self.edge(calledge).src().clone()
                } else {
                    return Err(InlineError::CallPortNotConnected);
                }
            };

            let inport = InportLocation {
                node: node,
                input: InputType::Input(0),
            };
            let indef = self.find_producer_inp(inport);

            //now trace it
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

            (
                call_lmd_def,
                a.get_call_arg_count(),
                a.outputs.len(),
                arg_srcs,
                res_dsts,
            )
        } else {
            return Err(InlineError::NodeWasNotApplyNode(node));
        };

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
        let mut result_remapping: AHashMap<InportLocation, SmallVec<[(InportLocation, E); 3]>> =
            AHashMap::default();

        //build the remapping structure, by iterating the call_lmd's args/results, and inserting the call_lmd's port-location
        // as key, and the corresponding arg_src / res_dst as value. This allows us to catch all args and
        // dst later when reconnecting all node, an connect them to the correct dst immediately.

        assert!(self.region(&lmd_region).unwrap().arguments.len() == arg_srcs.len());
        assert!(self.region(&lmd_region).unwrap().results.len() == res_dsts.len());

        //Check that there are no cv or rvs
        match &self.node(call_lmd).node_type {
            NodeType::Phi(phi) => {
                assert!(
                    phi.cv_argument(0).is_none(),
                    "Inliner does not support context variables!"
                );
                assert!(
                    phi.rv_argument(0).is_none(),
                    "Inliner does not support recursion variables!"
                );
            }
            NodeType::Lambda(lmd) => {
                assert!(
                    self.node(call_lmd)
                        .node_type
                        .unwrap_lambda_ref()
                        .cv_argument(0)
                        .is_none(),
                    "Inliner does not support context variables!"
                );
            }
            _ => panic!("Unexpected node type for callable in inliner"),
        }

        for (argidx, src) in arg_srcs.into_iter().enumerate() {
            let lmd_port = OutportLocation {
                node: call_lmd,
                //Checked beforehand that we don't have cv-s or rv-s
                output: OutputType::Argument(argidx),
            };
            argument_remapping.insert(lmd_port, src);
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

            //remap src
            if src.output.is_argument() {
                //use the argument remapping
                if let Some((remap_port, remap_ty)) = argument_remapping.get(&src).unwrap() {
                    assert!(remap_ty == &ty);
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
                    assert!(remapped_ty == &ty);
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

        Ok(())
    }
}
