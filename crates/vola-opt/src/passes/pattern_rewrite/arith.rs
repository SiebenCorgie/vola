/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Collection of arithmetic rewrites.

use rvsdg::{nodes::NodeType, smallvec::SmallVec};
use rvsdg_pattern_rewrite::{PatternRewrite, Speed};

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp},
        constant_fold::handle_constant_mul,
    },
    imm::{ImmMatrix, ImmScalar, ImmVector},
    DialectNode, OptEdge, OptNode, Optimizer,
};

///Folds constant-foldable _simple_ binary expressions. I.e. where both inputs are immediate values and of a known shape.
pub struct FoldBinarySimple;
impl PatternRewrite<OptNode, OptEdge, Optimizer, Speed> for FoldBinarySimple {
    fn matches(&self, ctx: &Optimizer, node: rvsdg::NodeRef) -> bool {
        //if the node is a binary node, and the two source nodes are in the immediate dialect,
        // matches...
        if let NodeType::Simple(s) = &ctx.graph[node].node_type {
            if s.try_downcast_ref::<BinaryArith>().is_none() {
                return false;
            }
        } else {
            //Not simple note
            return false;
        }

        //At this point we _know_ its a binary arith, so its safe to assume two inputs
        let Some(lhs) = ctx.graph.inport_src(node.input(0)) else {
            return false;
        };
        let Some(rhs) = ctx.graph.inport_src(node.input(1)) else {
            return false;
        };

        //Figure out whether we support the given folding pattern
        //Right now we support all bin-ops on scalars, vectors and matrices
        if let (NodeType::Simple(lhs), NodeType::Simple(rhs)) = (
            &ctx.graph[lhs.node].node_type,
            &ctx.graph[rhs.node].node_type,
        ) {
            if lhs.try_downcast_ref::<ImmScalar>().is_some()
                && rhs.try_downcast_ref::<ImmScalar>().is_some()
            {
                return true;
            }
            if lhs.try_downcast_ref::<ImmVector>().is_some()
                && rhs.try_downcast_ref::<ImmVector>().is_some()
            {
                return true;
            }
            if lhs.try_downcast_ref::<ImmMatrix>().is_some()
                && rhs.try_downcast_ref::<ImmMatrix>().is_some()
            {
                return true;
            }
        }
        false
    }

    fn benefit(&self) -> &Speed {
        &Speed(10)
    }

    fn apply(&self, ctx: &mut Optimizer, node: rvsdg::NodeRef) {
        //right now we can just reuse the old folding code
        let folded = {
            let source_nodes = ctx
                .graph
                .build_src_map(node)
                .iter()
                .map(|port| port.map(|port| &ctx.graph[port.node]))
                .collect::<SmallVec<[_; 2]>>();
            ctx.graph[node]
                .node_type
                .unwrap_simple_ref()
                .try_downcast_ref::<BinaryArith>()
                .expect("Should be binary arith, if matched before")
                .try_constant_fold(&source_nodes)
                .expect("Expected folded node")
        };
        let region = ctx.graph[node].parent.unwrap();

        log::info!("BinaryFold to {}", folded.node.name());

        let inserted = ctx
            .graph
            .on_region(&region, |reg| reg.insert_node(folded))
            .unwrap();

        ctx.graph.replace_node_uses(node, inserted).unwrap();
    }
}

///Special folder that handles constant multiplication of none-equal-typed pairs. I.e. matrix-vector / vector-matrix multiplication etc.
pub struct FoldMuliplication;
impl PatternRewrite<OptNode, OptEdge, Optimizer, Speed> for FoldMuliplication {
    fn matches(&self, ctx: &Optimizer, node: rvsdg::NodeRef) -> bool {
        //if the node is a binary node, and the two source nodes are in the immediate dialect,
        // matches...
        if let NodeType::Simple(s) = &ctx.graph[node].node_type {
            if let Some(ba) = s.try_downcast_ref::<BinaryArith>() {
                if ba.op != BinaryArithOp::Mul {
                    return false;
                }
            }
        } else {
            //Not simple note
            return false;
        }

        //At this point we _know_ its a binary arith, so its safe to assume two inputs
        let Some(lhs) = ctx.graph.inport_src(node.input(0)) else {
            return false;
        };
        let Some(rhs) = ctx.graph.inport_src(node.input(1)) else {
            return false;
        };

        //Figure out whether we support the given folding pattern.
        // Right now this is
        // Vector/Scalar
        // Matrix/Scalar
        // Vector/Matrix
        // Matrix/Vector
        if let (NodeType::Simple(lhs), NodeType::Simple(rhs)) = (
            &ctx.graph[lhs.node].node_type,
            &ctx.graph[rhs.node].node_type,
        ) {
            if lhs.try_downcast_ref::<ImmVector>().is_some()
                && rhs.try_downcast_ref::<ImmScalar>().is_some()
            {
                return true;
            }
            if lhs.try_downcast_ref::<ImmMatrix>().is_some()
                && rhs.try_downcast_ref::<ImmScalar>().is_some()
            {
                return true;
            }
            if lhs.try_downcast_ref::<ImmVector>().is_some()
                && rhs.try_downcast_ref::<ImmMatrix>().is_some()
            {
                return true;
            }
            if lhs.try_downcast_ref::<ImmMatrix>().is_some()
                && rhs.try_downcast_ref::<ImmVector>().is_some()
            {
                return true;
            }
        }
        false
    }

    fn benefit(&self) -> &Speed {
        &Speed(10)
    }

    fn apply(&self, ctx: &mut Optimizer, node: rvsdg::NodeRef) {
        //right now we can just reuse the old folding code
        let folded = {
            let source_nodes = ctx
                .graph
                .build_src_map(node)
                .iter()
                .map(|port| port.map(|port| &ctx.graph[port.node]))
                .collect::<SmallVec<[_; 2]>>();

            //Call into the old code
            handle_constant_mul(
                source_nodes[0].unwrap().node_type.unwrap_simple_ref(),
                source_nodes[1].unwrap().node_type.unwrap_simple_ref(),
            )
            .expect("expected multiplication to fold")
        };
        let region = ctx.graph[node].parent.unwrap();

        log::info!("BinaryFold Multiplication to {}", folded.node.name());

        let inserted = ctx
            .graph
            .on_region(&region, |reg| reg.insert_node(folded))
            .unwrap();

        ctx.graph.replace_node_uses(node, inserted).unwrap();
    }
}
