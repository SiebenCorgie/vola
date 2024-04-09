/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Simple `GraphTypeTransformer` pass that transforms an optimizer-graph to the spirv-backend graph.

use rvsdg::{
    region::{Input, Output},
    smallvec::smallvec,
    util::graph_type_transform::{GraphTypeTransformer, GraphTypeTransformerError},
};
use vola_opt::{OptEdge, OptNode, Optimizer};

use crate::{
    graph::{BackendEdge, BackendNode, BackendOp},
    spv::SpvNode,
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
            OptEdge::Value { ty: _ } => BackendEdge::Value,
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
        let new_graph = opt.graph.transform_new(&mut transformer)?;

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

        self.graph = new_graph;
        Ok(())
    }
}
