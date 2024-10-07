/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    util::graph_type_transform::{GraphMapping, GraphTypeTransformer},
    SmallColl,
};
use vola_common::{error::error_reporter, report};
use vola_opt::{OptEdge, OptNode, Optimizer};

use crate::{
    error::WasmError,
    graph::{WasmEdge, WasmNode, WasmTy},
    WasmBackend,
};

struct InterningTransformer<'a> {
    opt: &'a Optimizer,
    has_failed_node: bool,
}

impl<'a> GraphTypeTransformer for InterningTransformer<'a> {
    type SrcNode = OptNode;
    type SrcEdge = OptEdge;
    type DstNode = WasmNode;
    type DstEdge = WasmEdge;

    fn transform_simple_node(&mut self, src_node: &Self::SrcNode) -> Self::DstNode {
        //collect the nodes input signature to make an informed decission about the WASM node
        //that is generated
        let insig = src_node
            .node
            .inputs()
            .iter()
            .map(|port| {
                if let Some(edg) = port.edge {
                    self.opt.graph[edg].ty.get_type().cloned()
                } else {
                    None
                }
            })
            .collect::<SmallColl<_>>();

        let outsig = src_node
            .node
            .outputs()
            .iter()
            .map(|port| {
                if let Some(first_edge) = port.edges.get(0) {
                    let port = self.opt.graph[*first_edge].src().clone();
                    self.opt.find_type(&port.into())
                } else {
                    //Has no connected edge, therfore no type
                    None
                }
            })
            .collect::<SmallColl<_>>();

        match WasmNode::try_from_opt(src_node, &insig, &outsig) {
            Ok(n) => n,
            Err(e) => {
                self.has_failed_node = true;
                let span = src_node.span.clone();
                report(error_reporter(e, span).finish());
                WasmNode::error_for_opt(src_node)
            }
        }
    }

    fn transform_edge(&mut self, src_edge: &Self::SrcEdge) -> Self::DstEdge {
        match src_edge {
            OptEdge::State => WasmEdge::State,
            OptEdge::Value { ty } => {
                let ty = if let Some(t) = ty.get_type() {
                    WasmTy::from(t)
                } else {
                    WasmTy::Undefined
                };

                WasmEdge::Value(ty)
            }
        }
    }
}

impl WasmBackend {
    pub(crate) fn intern_optimizer(&mut self, optimizer: &Optimizer) -> Result<(), WasmError> {
        //Our interning strategy currently just translates all "Alge" nodes into "WASM" nodes with best effort fitting.
        //For instance a Vector-Vector multiplication is translated into a MulF32.
        //
        //The following scalarize and legalization passes will take care of unfolding etc.

        let mut transformer = InterningTransformer {
            opt: optimizer,
            has_failed_node: false,
        };
        let (new_graph, remapping) = optimizer.graph.transform_new(&mut transformer)?;

        if transformer.has_failed_node {
            return Err(WasmError::InterningFailed);
        }

        self.graph = new_graph;

        //transfer debug info
        self.transfer_debug_info(&remapping, optimizer);

        Ok(())
    }

    fn transfer_debug_info(&mut self, remapping: &GraphMapping, opt: &Optimizer) {
        for (src_node, dst_node) in remapping.node_mapping.iter() {
            if let Some(name) = opt.names.get(&src_node.into()) {
                self.names.set(dst_node.into(), name.clone());
            }
            if let Some(span) = opt.span_tags.get(&src_node.into()) {
                self.spans.set(dst_node.into(), span.clone());
            }
        }
    }
}
