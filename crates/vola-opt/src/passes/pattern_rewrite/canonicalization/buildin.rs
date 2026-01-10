/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2026 Tendsin Mende
 */

use rvsdg::{builder::RegionBuilder, edge::OutportLocation, NodeRef, SmallColl};
use rvsdg_pattern_rewrite::{CodeSize, PatternRewrite};
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp},
        buildin::{Buildin, BuildinOp},
    },
    common::Ty,
    imm::ImmScalar,
    route_new,
    typelevel::{ConstantIndex, IntervalConstruct, UniformConstruct},
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
        //Doesn't gain anything
        &CodeSize(0)
    }
    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        #[cfg(feature = "log")]
        log::info!("Lower Length {node}");

        let extract_component_count = |ty: &Ty| {
            if ty.is_vector() {
                Some(ty.shape().unwrap().component_count())
            } else {
                None
            }
        };

        let index_count = match ctx.get_in_type_mut(node.input(0)).unwrap() {
            //Cannot lower length for interval-valued length call
            Ty::Interval(_i) => panic!("Can not lower length over interval"),
            other => extract_component_count(&other).expect("Could not extract component count"),
        };

        //Now index into all components, square them, build the
        // square-root of it and return the output
        let mut node_collector = Vec::with_capacity(index_count * 2);
        let span = ctx.find_span(node).unwrap_or(Span::empty());
        let region = ctx.graph[node].parent.unwrap();
        let src_value = ctx.graph.inport_src(node.input(0)).unwrap();

        let new_producer = ctx
            .graph
            .on_region(&region, |g| {
                //Index into vector n-times
                let mut indices = SmallColl::new();
                for idx in 0..index_count {
                    let new_node =
                        route_new!(g, ConstantIndex::new(idx), span.clone(), [src_value]);

                    node_collector.push(new_node);
                    indices.push(new_node.output(0));
                }

                //square each
                let squared = indices
                    .into_iter()
                    .map(|indexed| {
                        let node =
                            route_new!(g, BinaryArithOp::Mul, span.clone(), [indexed, indexed]);

                        node_collector.push(node);
                        node.output(0)
                    })
                    .collect::<SmallColl<_>>();

                //Add all squared indices
                assert!(squared.len() >= 2);
                let mut last_add = route_new!(
                    g,
                    BinaryArithOp::Add,
                    span.clone(),
                    [squared[0], squared[1]]
                );
                node_collector.push(last_add);

                //now build the _staggered_ add chain.
                for next_idx in 2..squared.len() {
                    let new_last = route_new!(
                        g,
                        BinaryArithOp::Add,
                        span.clone(),
                        [last_add.output(0), squared[next_idx]]
                    );
                    node_collector.push(new_last);
                    last_add = new_last;
                }

                let sqrt = route_new!(g, BuildinOp::SquareRoot, span, [last_add.output(0)]);
                node_collector.push(sqrt);
                sqrt
            })
            .unwrap();

        ctx.graph.replace_node_uses(node, new_producer).unwrap();
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
        //Doesn't gain anything
        &CodeSize(0)
    }
    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        #[cfg(feature = "log")]
        log::info!("Lower Min/Max to aproximation for {node}");

        let is_min = ctx.try_unwrap_node::<Buildin>(node).unwrap().op == BuildinOp::Min;

        let inner_op = if is_min {
            BinaryArithOp::Sub
        } else {
            BinaryArithOp::Add
        };

        let ty = ctx
            .get_out_type_mut(node.output(0))
            .expect("Expected type to be set!");
        let region = ctx.graph[node].parent.unwrap();
        let span = ctx.find_span(node).unwrap_or(Span::empty());
        let imm_two = ctx.splat_scalar(region, ImmScalar::new(2.0), ty.clone());

        let x_src = ctx.graph.inport_src(node.input(0)).unwrap();
        let y_src = ctx.graph.inport_src(node.input(1)).unwrap();
        let x_ty = ctx.get_out_type_mut(x_src).unwrap();
        let y_ty = ctx.get_out_type_mut(y_src).unwrap();
        assert!(x_ty == y_ty);
        let mut node_collector = Vec::with_capacity(4);

        let output = ctx
            .graph
            .on_region(&region, |g| {
                let x_min_y = route_new!(g, BinaryArithOp::Sub, span.clone(), [x_src, y_src]);
                node_collector.push(x_min_y);

                let abs = route_new!(g, UnaryArithOp::Abs, span.clone(), x_min_y.output(0));
                node_collector.push(abs);

                //add or subtract
                let min_max_add_sub = route_new!(
                    g,
                    BinaryArith::new(inner_op),
                    span.clone(),
                    [y_src, abs.output(0)]
                );
                node_collector.push(min_max_add_sub);

                //add
                let add = route_new!(
                    g,
                    BinaryArithOp::Add,
                    span.clone(),
                    [x_src, min_max_add_sub.output(0)]
                );
                node_collector.push(add);

                //div with two
                let div = route_new!(g, BinaryArithOp::Div, span, [add.output(0), imm_two]);
                node_collector.push(div);
                div
            })
            .unwrap();

        //now replace callers of that min/max call with the new output
        ctx.graph.replace_node_uses(node, output).unwrap();
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
        //Doesn't gain anything
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
        //Doesn't gain anything
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

///Unrolls a `cross` operation: `cross(a, b)` (for 2,3,4 components) => (for vec3): `[a.y * b.z - a.z * b.y, ...]` i.e. the typical way.
pub struct UnrollCross;
impl PatternRewrite<OptNode, OptEdge, Optimizer, CodeSize> for UnrollCross {
    fn name(&self) -> &str {
        "UnrollCross"
    }
    fn benefit(&self) -> &CodeSize {
        //Doesn't gain anything
        &CodeSize(0)
    }
    fn matches(&self, ctx: &Optimizer, node: NodeRef) -> bool {
        if let Some(buildin) = ctx.try_unwrap_node::<Buildin>(node) {
            buildin.op == BuildinOp::Cross
        } else {
            false
        }
    }
    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        let extract_component_count = |ty: &Ty| {
            if ty.is_vector() {
                Some(ty.shape().unwrap().component_count())
            } else {
                None
            }
        };

        let index_count = match ctx.get_in_type_mut(node.input(0)).unwrap() {
            //Not yet implemented, see the dot-implementation (mainly the extract_index closure)
            // on how that would work in practice...
            Ty::Interval(_i) => panic!("can not lower cross over intervals"),
            other => extract_component_count(&other).expect("Could not extract component count"),
        };

        let region = ctx.graph[node].parent.unwrap();
        let span = ctx.find_span(node).unwrap_or(Span::empty());
        let mut node_collector = Vec::new();

        let value_reducer = |g: &mut RegionBuilder<_, _>,
                             span: Span,
                             node_collector: &mut Vec<NodeRef>,
                             al: OutportLocation,
                             bl: OutportLocation,
                             ar: OutportLocation,
                             br: OutportLocation| {
            let mul_left = route_new!(g, BinaryArithOp::Mul, span.clone(), [al, bl]);
            node_collector.push(mul_left);
            let mul_right = route_new!(g, BinaryArithOp::Mul, span.clone(), [ar, br]);
            node_collector.push(mul_right);
            let sub = route_new!(
                g,
                BinaryArithOp::Sub,
                span.clone(),
                [mul_left.output(0), mul_right.output(0)]
            );
            node_collector.push(sub);
            sub
        };

        let a_src = ctx.graph.inport_src(node.input(0)).unwrap();
        let b_src = ctx.graph.inport_src(node.input(1)).unwrap();

        let result = match index_count {
            3 => {
                //reduce into
                // [ a.y * b.z - a.z * b.y,
                //   a.z * b.x - a.x * b.z,
                //   a.x * b.y - a.y * b.x ]
                ctx.graph
                    .on_region(&region, |g| {
                        let ax = route_new!(g, ConstantIndex::new(0), span.clone(), [a_src]);
                        node_collector.push(ax);
                        let ay = route_new!(g, ConstantIndex::new(1), span.clone(), [a_src]);
                        node_collector.push(ay);
                        let az = route_new!(g, ConstantIndex::new(2), span.clone(), [a_src]);
                        node_collector.push(az);
                        let bx = route_new!(g, ConstantIndex::new(0), span.clone(), [b_src]);
                        node_collector.push(bx);
                        let by = route_new!(g, ConstantIndex::new(1), span.clone(), [b_src]);
                        node_collector.push(by);
                        let bz = route_new!(g, ConstantIndex::new(2), span.clone(), [b_src]);
                        node_collector.push(bz);

                        //a.y * b.z - a.z * b.y
                        let new_x = value_reducer(
                            g,
                            span.clone(),
                            &mut node_collector,
                            ay.output(0),
                            bz.output(0),
                            az.output(0),
                            by.output(0),
                        );
                        node_collector.push(new_x);
                        //a.z * b.x - a.x * b.z
                        let new_y = value_reducer(
                            g,
                            span.clone(),
                            &mut node_collector,
                            az.output(0),
                            bx.output(0),
                            ax.output(0),
                            bz.output(0),
                        );
                        node_collector.push(new_y);
                        //a.x * b.y - a.y * b.x
                        let new_z = value_reducer(
                            g,
                            span.clone(),
                            &mut node_collector,
                            ax.output(0),
                            by.output(0),
                            ay.output(0),
                            bx.output(0),
                        );
                        node_collector.push(new_z);

                        //Finally construct the new vector
                        let new_vec = route_new!(
                            g,
                            UniformConstruct::new().with_inputs(3),
                            span.clone(),
                            [new_x.output(0), new_y.output(0), new_z.output(0)]
                        );
                        node_collector.push(new_vec);
                        new_vec
                    })
                    .unwrap()
            }
            count => panic!("can not lower cross over {count} components"),
        };

        ctx.graph
            .replace_outport_uses(node.output(0), result.output(0))
            .unwrap();
    }
}

///Unrolls a `dot-product`: `dot(a, b)` => `a.x * b.x + a.y * b.y + ...`
pub struct UnrollDot;
impl PatternRewrite<OptNode, OptEdge, Optimizer, CodeSize> for UnrollDot {
    fn name(&self) -> &str {
        "UnrollDot"
    }
    fn benefit(&self) -> &CodeSize {
        //Doesn't gain anything
        &CodeSize(0)
    }
    fn matches(&self, ctx: &Optimizer, node: NodeRef) -> bool {
        if let Some(buildin) = ctx.try_unwrap_node::<Buildin>(node) {
            buildin.op == BuildinOp::Dot
        } else {
            false
        }
    }
    fn apply(&self, ctx: &mut Optimizer, node: NodeRef) {
        let extract_component_count = |ty: &Ty| {
            if ty.is_vector() {
                Some(ty.shape().unwrap().component_count())
            } else {
                None
            }
        };

        let region = ctx.graph[node].parent.unwrap();
        let span = ctx.find_span(node).unwrap_or(Span::empty());
        let mut node_collector = Vec::new();
        let a = ctx.graph.inport_src(node.input(0)).unwrap();
        let b = ctx.graph.inport_src(node.input(1)).unwrap();

        struct IntervalSrcs {
            astart: OutportLocation,
            aend: OutportLocation,
            bstart: OutportLocation,
            bend: OutportLocation,
        }

        let (index_count, interval_bounds) = match ctx.get_in_type_mut(node.input(0)).unwrap() {
            Ty::Interval(i) => {
                //For intervals, load the lower and upper bounds, and return those. They will be used by the
                // extraction function to setup scalar intervals for each index.

                let interval_extractors = ctx
                    .graph
                    .on_region(&region, |reg| {
                        let astart = route_new!(reg, ConstantIndex::new(0), span.clone(), [a]);
                        node_collector.push(astart);
                        let aend = route_new!(reg, ConstantIndex::new(1), span.clone(), [a]);
                        node_collector.push(aend);

                        let bstart = route_new!(reg, ConstantIndex::new(0), span.clone(), [b]);
                        node_collector.push(bstart);
                        let bend = route_new!(reg, ConstantIndex::new(1), span.clone(), [b]);
                        node_collector.push(bend);

                        IntervalSrcs {
                            astart: astart.output(0),
                            aend: aend.output(0),
                            bstart: bstart.output(0),
                            bend: bend.output(0),
                        }
                    })
                    .unwrap();

                (
                    extract_component_count(i.as_ref()).expect("Could not extract component count"),
                    Some(interval_extractors),
                )
            }
            other => (
                extract_component_count(&other).expect("Could not extract component count"),
                None,
            ),
        };

        if index_count < 2 {
            panic!("Dot over {index_count} components makes no sense");
        }

        let extract_index = |g: &mut RegionBuilder<_, _>,
                             collector: &mut Vec<NodeRef>,
                             span: Span,
                             index: usize| {
            if let Some(bounds) = &interval_bounds {
                // we already pre-fetched the interval bound,
                // now just setup the scalar-interval for the given index
                // for both a and b, and return

                let ia_start =
                    route_new!(g, ConstantIndex::new(index), span.clone(), [bounds.astart]);
                collector.push(ia_start);
                let ia_end = route_new!(g, ConstantIndex::new(index), span.clone(), [bounds.aend]);
                collector.push(ia_end);
                let ia = route_new!(
                    g,
                    IntervalConstruct::default(),
                    span.clone(),
                    [ia_start.output(0), ia_end.output(0)]
                );
                collector.push(ia);

                let ib_start =
                    route_new!(g, ConstantIndex::new(index), span.clone(), [bounds.bstart]);
                collector.push(ib_start);
                let ib_end = route_new!(g, ConstantIndex::new(index), span.clone(), [bounds.bend]);
                collector.push(ib_end);
                let ib = route_new!(
                    g,
                    IntervalConstruct::default(),
                    span.clone(),
                    [ib_start.output(0), ib_end.output(0)]
                );
                collector.push(ib);
                (ia, ib)
            } else {
                let ia = route_new!(g, ConstantIndex::new(index), span.clone(), [a]);
                collector.push(ia);
                let ib = route_new!(g, ConstantIndex::new(index), span, [b]);
                collector.push(ib);
                (ia, ib)
            }
        };

        let new = ctx
            .graph
            .on_region(&region, |reg| {
                let (ax, bx) = extract_index(reg, &mut node_collector, span.clone(), 0);
                //iterate all indices, and add each _line_ together
                let mut last_result = route_new!(
                    reg,
                    BinaryArithOp::Mul,
                    span.clone(),
                    [ax.output(0), bx.output(0)]
                );
                node_collector.push(last_result);
                for i in 1..index_count {
                    let (ia, ib) = extract_index(reg, &mut node_collector, span.clone(), i);
                    let newline = route_new!(
                        reg,
                        BinaryArithOp::Mul,
                        span.clone(),
                        [ia.output(0), ib.output(0)]
                    );
                    node_collector.push(newline);
                    let to_result = route_new!(
                        reg,
                        BinaryArithOp::Add,
                        span.clone(),
                        [last_result.output(0), newline.output(0)]
                    );
                    node_collector.push(to_result);
                    last_result = to_result;
                }

                last_result
            })
            .unwrap();

        ctx.graph.replace_node_uses(node, new).unwrap();
    }
}
