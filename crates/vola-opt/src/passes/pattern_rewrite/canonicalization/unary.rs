/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::NodeRef;
use rvsdg_pattern_rewrite::{CodeSize, PatternRewrite};
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp},
        buildin::{Buildin, BuildinOp},
    },
    imm::ImmScalar,
    OptEdge, OptNode, Optimizer,
};

///Canonicalizes Abs into an aproximation `|f(x)| => sqrt(f(x)^2 + c)` where C is defined in the optimizer's config.
pub struct AproxAbs;

impl PatternRewrite<OptNode, OptEdge, Optimizer, CodeSize> for AproxAbs {
    fn matches(&self, ctx: &Optimizer, node: NodeRef) -> bool {
        if let Some(unary) = ctx.try_unwrap_node::<UnaryArith>(node) {
            unary.op == UnaryArithOp::Abs
        } else {
            false
        }
    }
    fn name(&self) -> &str {
        "AproxAbs"
    }
    fn benefit(&self) -> &CodeSize {
        //Doesn't gain anything
        &CodeSize(0)
    }
    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        #[cfg(feature = "log")]
        log::info!("Aproximating Abs {node}");

        let span = ctx.find_span(node).unwrap_or(Span::empty());
        let region = ctx.graph[node].parent.unwrap();
        //transform into a smooth abs function
        // as |f(x)| => sqrt(f(x)^2 + 0.001 )
        let f_src = ctx.graph.inport_src(node.input(0)).unwrap();
        let ty = ctx.get_out_type_mut(f_src).unwrap();
        let c_splat = ctx.splat_scalar(
            region,
            ImmScalar::new(ctx.config.autodiff.smooth_abs_c),
            ty.clone(),
        );

        let new_result = ctx
            .graph
            .on_region(&region, |g| {
                let (fx_square, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                        [f_src, f_src],
                    )
                    .unwrap();
                let (with_const, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Add), span.clone()),
                        [fx_square.output(0), c_splat],
                    )
                    .unwrap();
                let (sqrt, _) = g
                    .connect_node(
                        OptNode::new(Buildin::new(BuildinOp::SquareRoot), span.clone()),
                        [with_const.output(0)],
                    )
                    .unwrap();

                sqrt
            })
            .unwrap();

        ctx.graph
            .replace_node_uses(node, new_result)
            .expect("Could not replace node")
    }
}
