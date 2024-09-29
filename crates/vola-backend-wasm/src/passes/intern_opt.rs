/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::util::graph_type_transform::{GraphMapping, GraphTypeTransformer};
use vola_common::{error::error_reporter, report, Span};
use vola_opt::{common::Ty, OptEdge, OptNode, Optimizer};

use crate::{
    error::WasmError,
    graph::{TyShape, WasmEdge, WasmNode, WasmTy},
    WasmBackend,
};

struct InterningTransformer {
    has_failed_node: bool,
}

impl GraphTypeTransformer for InterningTransformer {
    type SrcNode = OptNode;
    type SrcEdge = OptEdge;
    type DstNode = WasmNode;
    type DstEdge = WasmEdge;

    fn transform_simple_node(&mut self, src_node: &Self::SrcNode) -> Self::DstNode {
        match WasmNode::try_from(src_node) {
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
                let (ty, shape) = if let Some(ty) = ty.get_type() {
                    match ty {
                        Ty::Scalar => (WasmTy::F32, TyShape::Scalar),
                        Ty::Nat => (WasmTy::I32, TyShape::Scalar),
                        Ty::Bool => (WasmTy::I32, TyShape::Scalar),
                        Ty::Vector { width } => (WasmTy::F32, TyShape::Vector { width }),
                        Ty::Matrix { width, height } => {
                            (WasmTy::F32, TyShape::Matrix { width, height })
                        }
                        Ty::Tensor { dim } => (
                            WasmTy::F32,
                            TyShape::Tensor {
                                dim: dim.iter().cloned().collect(),
                            },
                        ),
                        other => {
                            report(
                                error_reporter(WasmError::UnexpectedType(other), Span::empty())
                                    .finish(),
                            );
                            (WasmTy::Undefined, TyShape::Scalar)
                        }
                    }
                } else {
                    (WasmTy::Undefined, TyShape::Scalar)
                };

                WasmEdge::Value { ty, shape }
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
