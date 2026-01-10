/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2026 Tendsin Mende
 */

use rvsdg::{edge::InputType, NodeRef, SmallColl};
use rvsdg_pattern_rewrite::{CodeSize, PatternRewrite};
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp},
        buildin::{Buildin, BuildinOp},
    },
    common::{DataType, Shape, Ty},
    imm::ImmScalar,
    typelevel::ConstantIndex,
    OptEdge, OptNode, Optimizer,
};

///Rewrites a `length(v)` into `sqrt(v.x*v.x + v.y*v.y + ..)`
pub struct LowerLength;
impl PatternRewrite<OptNode, OptEdge, Optimizer, CodeSize> for LowerLength {
    fn matches(&self, ctx: &Optimizer, node: NodeRef) -> bool {
        if let Some(buildin) = ctx.try_unwrap_node::<Buildin>(node) {
            buildin.op == BuildinOp::Length
        } else {
            false
        }
    }
    fn name(&self) -> &str {
        "LowerLength"
    }
    fn benefit(&self) -> &CodeSize {
        &CodeSize(0)
    }
    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        #[cfg(feature = "log")]
        log::info!("Lower Length {node}");

        let src_value = ctx
            .graph
            .inport_src(node.as_inport_location(InputType::Input(0)))
            .unwrap();
        let src_ty = ctx.get_out_type_mut(src_value).unwrap();
        let span = ctx.find_span(node).unwrap_or(Span::empty());
        let region = ctx.graph[node].parent.unwrap();

        //find the input type to the length node. We use that to
        //determin how often we need to index (and square)
        //the vector
        let index_count = if let Ty::Shaped {
            ty: DataType::Real,
            shape: Shape::Vec { width },
        } = src_ty
        {
            width
        } else {
            panic!("Vec used invalid vector input");
        };

        let new_producer = ctx
            .graph
            .on_region(&region, |g| {
                //Index into vector n-times
                let mut indices = SmallColl::new();
                for idx in 0..index_count {
                    let (new_node, _edges) = g
                        .connect_node(
                            OptNode::new(ConstantIndex::new(idx), span.clone()),
                            [src_value],
                        )
                        .unwrap();
                    indices.push(new_node.output(0));
                }

                //square each
                let squared = indices
                    .into_iter()
                    .map(|indexed| {
                        let (node, _edg) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                [indexed, indexed],
                            )
                            .unwrap();

                        node.output(0)
                    })
                    .collect::<SmallColl<_>>();

                //Add all squared indices
                assert!(squared.len() >= 2);
                let (mut last_add, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Add), span.clone()),
                        [squared[0], squared[1]],
                    )
                    .unwrap();
                //now build the _staggered_ add chain.
                for next_idx in 2..squared.len() {
                    let (new_last, _) = g
                        .connect_node(
                            OptNode::new(BinaryArith::new(BinaryArithOp::Add), span.clone()),
                            [last_add.output(0), squared[next_idx]],
                        )
                        .unwrap();

                    last_add = new_last;
                }

                let (sqrt, _) = g
                    .connect_node(
                        OptNode::new(Buildin::new(BuildinOp::SquareRoot), span),
                        [last_add.output(0)],
                    )
                    .unwrap();

                sqrt
            })
            .unwrap();

        ctx.graph
            .replace_node_uses(node, new_producer)
            .expect("Failed to replace lenght instruction");
    }
}

///Aproximates Min/Max operations inspired by this: <https://auto-differentiation.github.io/ref/math/>
///implement as:
///
///min: (x + y - abs(x-y)) / 2
///max: (x + y + abs(x-y)) / 2
pub struct AproxMinMax;
impl PatternRewrite<OptNode, OptEdge, Optimizer, CodeSize> for AproxMinMax {
    fn matches(&self, ctx: &Optimizer, node: NodeRef) -> bool {
        if let Some(buildin) = ctx.try_unwrap_node::<Buildin>(node) {
            buildin.op == BuildinOp::Min || buildin.op == BuildinOp::Max
        } else {
            false
        }
    }
    fn name(&self) -> &str {
        "AproxMinMax"
    }
    fn benefit(&self) -> &CodeSize {
        &CodeSize(0)
    }
    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        #[cfg(feature = "log")]
        log::info!("Lower Min/Max to aproximation for {node}");

        let buildin_node_op = ctx.try_unwrap_node::<Buildin>(node).map(|n| n.op).unwrap();
        let span = ctx.find_span(node).unwrap_or(Span::empty());
        let region = ctx.graph[node].parent.unwrap();

        let inner_op = if buildin_node_op == BuildinOp::Min {
            BinaryArithOp::Sub
        } else {
            BinaryArithOp::Add
        };

        let ty = ctx
            .get_out_type_mut(node.output(0))
            .expect("Expected type to be set!");
        let imm_two = ctx.splat_scalar(region, ImmScalar::new(2.0), ty.clone());

        let x_src = ctx.graph.inport_src(node.input(0)).unwrap();
        let y_src = ctx.graph.inport_src(node.input(1)).unwrap();
        let x_ty = ctx.get_out_type_mut(x_src).unwrap();
        let y_ty = ctx.get_out_type_mut(y_src).unwrap();
        assert!(x_ty == y_ty);

        let output = ctx
            .graph
            .on_region(&region, |g| {
                let (x_min_y, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Sub), span.clone()),
                        [x_src, y_src],
                    )
                    .unwrap();

                let (abs, to_abs_edge) = g
                    .connect_node(
                        OptNode::new(UnaryArith::new(UnaryArithOp::Abs), span.clone()),
                        [x_min_y.output(0)],
                    )
                    .unwrap();
                //pre_set the abs-type, otherwise the shortcut to canonicalize abs won't work later on.
                assert!(to_abs_edge.len() == 1);
                g.ctx_mut().edge_mut(to_abs_edge[0]).ty.set_type(x_ty);

                //add or subtract
                let (min_max_add_sub, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(inner_op), span.clone()),
                        [y_src, abs.output(0)],
                    )
                    .unwrap();

                //add
                let (add, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Add), span.clone()),
                        [x_src, min_max_add_sub.output(0)],
                    )
                    .unwrap();
                //div with two
                let (div, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Div), span.clone()),
                        [add.output(0), imm_two],
                    )
                    .unwrap();

                div
            })
            .unwrap();

        //now replace callers of that min/max call with the new output
        ctx.graph
            .replace_node_uses(node, output)
            .expect("Failed to replace min/max");
    }
}

///Approximates a _Mix_ operation via: `mix(x,y,a) => x*(1-a) + y*a`.
pub struct AproxMix;
impl PatternRewrite<OptNode, OptEdge, Optimizer, CodeSize> for AproxMix {
    fn matches(&self, ctx: &Optimizer, node: NodeRef) -> bool {
        if let Some(buildin) = ctx.try_unwrap_node::<Buildin>(node) {
            buildin.op == BuildinOp::Mix
        } else {
            false
        }
    }
    fn benefit(&self) -> &CodeSize {
        &CodeSize(0)
    }
    fn name(&self) -> &str {
        "AproxMix"
    }
    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        #[cfg(feature = "log")]
        log::info!("Lower Mix to aproximation for {node}");

        let span = ctx.find_span(node).unwrap_or(Span::empty());
        let region = ctx.graph[node].parent.unwrap();
        let x_src = ctx.graph.inport_src(node.input(0)).unwrap();
        let y_src = ctx.graph.inport_src(node.input(1)).unwrap();
        let a_src = ctx.graph.inport_src(node.input(2)).unwrap();

        let a_ty = ctx.get_out_type_mut(a_src).unwrap();
        let one = ctx.splat_scalar(region, ImmScalar::new(1.0), a_ty);
        let new_result = ctx
            .graph
            .on_region(&region, |g| {
                let (one_minus, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Sub), span.clone()),
                        [one, a_src],
                    )
                    .unwrap();

                let (x_times, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                        [x_src, one_minus.output(0)],
                    )
                    .unwrap();
                let (y_times, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                        [y_src, a_src],
                    )
                    .unwrap();

                let (result, _) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Add), span.clone()),
                        [x_times.output(0), y_times.output(0)],
                    )
                    .unwrap();
                result
            })
            .unwrap();

        ctx.graph.replace_node_uses(node, new_result).unwrap();
    }
}

///Aproximates a clamp operation via: `clamp(v, l, u) => min(max(v, l), u)`
pub struct AproxClamp;
impl PatternRewrite<OptNode, OptEdge, Optimizer, CodeSize> for AproxClamp {
    fn matches(&self, ctx: &Optimizer, node: NodeRef) -> bool {
        if let Some(buildin) = ctx.try_unwrap_node::<Buildin>(node) {
            buildin.op == BuildinOp::Clamp
        } else {
            false
        }
    }
    fn name(&self) -> &str {
        "AproxClamp"
    }
    fn benefit(&self) -> &CodeSize {
        &CodeSize(0)
    }
    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        #[cfg(feature = "log")]
        log::info!("Lower Clamp to aproximation for {node}");

        let x_src = ctx.graph.inport_src(node.input(0)).unwrap();
        let expr_ty = ctx.get_out_type_mut(x_src).unwrap();
        let min_src = ctx.graph.inport_src(node.input(1)).unwrap();
        let max_src = ctx.graph.inport_src(node.input(2)).unwrap();
        let region = ctx.graph[node].parent.unwrap();
        let span = ctx.find_span(node).unwrap_or(Span::empty());

        let result = ctx
            .graph
            .on_region(&region, |g| {
                let (inner_max, max_edg) = g
                    .connect_node(
                        OptNode::new(Buildin::new(BuildinOp::Max), span.clone()),
                        [x_src, min_src],
                    )
                    .unwrap();
                let (inner_min, min_edg) = g
                    .connect_node(
                        OptNode::new(Buildin::new(BuildinOp::Min), span.clone()),
                        [inner_max.output(0), max_src],
                    )
                    .unwrap();

                for edg in max_edg.into_iter().chain(min_edg.into_iter()) {
                    g.ctx_mut().edge_mut(edg).ty.set_type(expr_ty.clone());
                }

                inner_min
            })
            .unwrap();

        //if succesful, replace
        ctx.graph.replace_node_uses(node, result).unwrap();
    }
}
