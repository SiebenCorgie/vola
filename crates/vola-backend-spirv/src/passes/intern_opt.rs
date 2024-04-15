/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Simple `GraphTypeTransformer` pass that transforms an optimizer-graph to the spirv-backend graph.

use std::ops::Deref;

use ahash::AHashMap;
use rvsdg::{
    attrib::{AttribLocation, AttribStore, FlagStore},
    edge::{InportLocation, OutportLocation},
    region::{Input, Output},
    smallvec::smallvec,
    util::graph_type_transform::{GraphMapping, GraphTypeTransformer, GraphTypeTransformerError},
    NodeRef,
};
use vola_common::dot::graphviz_rust::dot_structures::NodeId;
use vola_opt::{OptEdge, OptNode, Optimizer};

use crate::{
    graph::{BackendEdge, BackendNode, BackendOp},
    spv::{SpvNode, SpvType},
    SpirvBackend,
};

pub struct InterningTransformer;

impl GraphTypeTransformer for InterningTransformer {
    type SrcNode = OptNode;
    type SrcEdge = OptEdge;
    type DstNode = BackendNode;
    type DstEdge = BackendEdge;

    fn transform_edge(&mut self, src_edge: &Self::SrcEdge) -> Self::DstEdge {
        //This basically just erases the type information atm.
        match src_edge {
            OptEdge::State => BackendEdge::State,
            OptEdge::Value { ty } => BackendEdge::Value(
                ty.get_type()
                    .map(|t| {
                        t.try_into().expect(
                            "Failed to convert opt-edge type to SPIR-V type. This indicates a bug!",
                        )
                    })
                    .unwrap_or(SpvType::undefined()),
            ),
        }
    }
    fn transform_simple_node(&mut self, src_node: &Self::SrcNode) -> Self::DstNode {
        let op = if let Some(sop) = SpvNode::try_from_opt_node(src_node) {
            BackendOp::SpirvOp(sop)
        } else {
            BackendOp::Dummy
        };

        let node = BackendNode {
            inputs: smallvec![Input::default(); src_node.node.inputs().len()],
            output: Output::default(),
            op,
        };

        node
    }
    /*
    fn on_mapping(
        &mut self,
        src_graph: &rvsdg::Rvsdg<Self::SrcNode, Self::SrcEdge>,
        src_node: rvsdg::NodeRef,
        dst_graph: &mut rvsdg::Rvsdg<Self::DstNode, Self::DstEdge>,
        dst_node: rvsdg::NodeRef,
    ) {
        //for each mapping, check if there is a port-type, if so we add the mapped-type to
        //the node in `dst_graph`.
        for outty in src_graph.node(src_node).outport_types() {
            let edge_count = src_graph
                .node(src_node)
                .outport(&outty)
                .unwrap()
                .edges
                .len();
            for edg_idx in 0..edge_count {
                let src_edge_id = src_graph.node(src_node).outport(&outty).unwrap().edges[edg_idx];
                let dst_edg_id = dst_graph.node(dst_node).outport(&outty).unwrap().edges[edg_idx];

                if let Some(ty) = src_graph.edge(src_edge_id).ty.get_type() {
                    if let BackendEdge::Value(dst_ty) = &mut dst_graph.edge_mut(dst_edg_id).ty {
                        *dst_ty = ty.clone().into();
                    } else {
                        #[cfg(feature = "log")]
                        log::warn!("expected typed value-edge");
                    }
                }
            }
        }
    }
    */
}

impl SpirvBackend {
    pub fn intern_opt_graph(&mut self, opt: &Optimizer) -> Result<(), GraphTypeTransformerError> {
        //right now we do expect the module to have no imports at all. Since we don't expect to link anything
        assert!(
            opt.graph
                .region(&opt.graph.toplevel_region())
                .unwrap()
                .arguments
                .len()
                == 0,
            "Unexpected import on optimizer graph!"
        );

        let mut transformer = InterningTransformer;
        //to be sure that we carry over all exports, first transform into a local graph, then merge with the existing one.
        let (new_graph, remapping) = opt.graph.transform_new(&mut transformer)?;

        #[cfg(feature = "log")]
        {
            //emit error if the current graph is not empty
            //TODO: implement graph merging instead, which should append the exports.
            if self
                .graph
                .region(&self.graph.toplevel_region())
                .unwrap()
                .nodes
                .len()
                > 0
            {
                log::error!("Merging of backend-graphs not yet supported, overwriting!")
            }
        }

        //As part of the interning progress we try to recover all known type information from the opt-graph
        //and move that into the backend-graph.
        self.graph = new_graph;

        //now use the remapping to transfer identifiers and source spans
        self.transfer_debug_info(&remapping, opt);
        self.transfer_type_info(&remapping, opt);

        Ok(())
    }

    fn transfer_type_info(&mut self, remapping: &GraphMapping, opt: &Optimizer) {
        for (location, ty) in opt.typemap.flags.iter() {
            let remapped_attrib_location = match location {
                AttribLocation::InPort(port) => {
                    let mut remapped_port = port.clone();
                    if let Some(remapped_node) = remapping.node_mapping.get(&port.node) {
                        remapped_port.node = *remapped_node;
                        AttribLocation::InPort(remapped_port)
                    } else {
                        //Could not remap, therfore ignore
                        continue;
                    }
                }
                AttribLocation::OutPort(port) => {
                    let mut remapped_port = port.clone();
                    if let Some(remapped_node) = remapping.node_mapping.get(&port.node) {
                        remapped_port.node = *remapped_node;
                        AttribLocation::OutPort(remapped_port)
                    } else {
                        //Could not remap, therfore ignore
                        continue;
                    }
                }
                AttribLocation::Region(reg) => {
                    if let Some(remapped_reg_node) = remapping.node_mapping.get(&reg.node) {
                        let mut regloc = reg.clone();
                        regloc.node = *remapped_reg_node;
                        AttribLocation::Region(regloc)
                    } else {
                        continue;
                    }
                }
                AttribLocation::Node(node) => {
                    if let Some(rmn) = remapping.node_mapping.get(node) {
                        AttribLocation::Node(*rmn)
                    } else {
                        continue;
                    }
                }
                AttribLocation::Edge(_edg) => {
                    //Those are taken care of later
                    continue;
                }
            };

            //now try to get the type info, and if there is some, transfore it to the new attribloc
            if let Ok(newty) = ty.clone().try_into() {
                self.typemap.set(remapped_attrib_location, newty);
            }
        }
        //now do all the edges explicitly
        for (src_edg, dst_edg) in remapping.edge_mapping.iter() {
            if let Some(ty) = opt.graph.edge(*src_edg).ty.get_type() {
                if let Ok(converted_type) = ty.clone().try_into() {
                    //replace type info, make sure we only overide untyped edges
                    assert!(self
                        .graph
                        .edge_mut(*dst_edg)
                        .ty
                        .set_type(converted_type)
                        .is_some());
                }
            }
        }
    }

    fn transfer_debug_info(&mut self, remapping: &GraphMapping, opt: &Optimizer) {
        for (src_node, dst_node) in remapping.node_mapping.iter() {
            if let Some(name) = opt.names.get(&src_node.into()) {
                self.idents.set(dst_node.into(), name.clone());
            }
            if let Some(span) = opt.span_tags.get(&src_node.into()) {
                self.spans.set(dst_node.into(), span.clone());
            }
        }
    }
}
