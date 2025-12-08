/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */
use rvsdg::{builder::RegionBuilder, edge::OutportLocation, NodeRef, SmallColl};
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp},
        buildin::{Buildin, BuildinOp},
    },
    common::{DataType, Shape, Ty},
    imm::ImmScalar,
    route_new,
    typelevel::{ConstantIndex, UniformConstruct},
    Optimizer,
};

mod mul;

///Stateless util that handles the lowering of the following Ops
/// into operations made form _lower-level_ operations, i.e. do not contain the op-type this was used on:
///
/// - `length(v)` => `sqrt(v[0] * v[0] + v[1] * v[1] + ...)`
/// - `mix(x,y,a)` => `x*(1-a) + y*a`
/// - `clamp(x, minval, maxval)` => `min(max(x, minval), maxval)`
/// - `cross(a, b)` (for 2,3,4 components) => (for vec3): `[a.y * b.z - a.z * b.y, ...]` i.e. the typical unwraping.
/// - `min(x, y)` => `(x + y - abs(x-y)) / 2`
/// - `max(x, y)` => `(x + y + abs(x-y)) / 2`
/// - MulMatrixMatrix => (unroll)
/// - MulMatrixVector => (unroll)
/// - MulVectorMatrix => (unroll)
///
/// This can help when there is no good implementaion for a lowering
/// for instance. However, its always prefered to stay with higher level nodes as long as possible though.
///
/// # Important
///
/// By definition, the returned nodes are in toplogical order.
pub struct Simplify<'opt> {
    opt: &'opt mut Optimizer,
    node: NodeRef,
    allow_approximation: bool,
}

impl<'opt> Simplify<'opt> {
    ///Lowers `node` into an equivalent (or, if `allow_approximation` is true, possibly an approximation) expression. Returns all newly created nodes for that expression.
    ///
    /// If successful, replaces `node` with the newly created equivalent. Otherwise returns None.
    ///
    /// Use [execute](Self::execute) to let the helper figure out whice lowering to use, or,
    /// if the node-type is know, address it directly via [lower_length](Self::lower_length) for instance.
    ///
    pub fn new(optimizer: &'opt mut Optimizer, node: NodeRef, allow_approximation: bool) -> Self {
        Self {
            opt: optimizer,
            allow_approximation,
            node,
        }
    }

    pub fn execute(self) -> Option<Vec<NodeRef>> {
        if let Some(buildin) = self.opt.try_unwrap_node::<Buildin>(self.node) {
            match buildin.op.clone() {
                BuildinOp::Length => return self.lower_length(),
                BuildinOp::Mix => return self.lower_mix(),
                BuildinOp::Clamp => return self.lower_clamp(),
                BuildinOp::Cross => return self.lower_cross(),
                BuildinOp::Min => return self.lower_min_max(true),
                BuildinOp::Max => return self.lower_min_max(false),
                _ => {}
            }
        }

        if let Some(binop) = self.opt.try_unwrap_node::<BinaryArith>(self.node) {
            if binop.op == BinaryArithOp::Mul {
                return self.unroll_multiplication();
            }
        }

        None
    }

    ///Transforms `length(v)` into `sqrt(v.x ^ 2 + v.y ^ 2 ...)`
    pub fn lower_length(self) -> Option<Vec<NodeRef>> {
        let extract_component_count = |ty: &Ty| {
            if ty.is_vector() {
                Some(ty.shape().unwrap().component_count())
            } else {
                None
            }
        };

        let index_count = match self.opt.get_in_type_mut(self.node.input(0)).unwrap() {
            Ty::Interval(i) => extract_component_count(i.as_ref())?,
            other => extract_component_count(&other)?,
        };

        //Now index into all components, square them, build the
        // square-root of it and return the output
        let mut node_collector = Vec::with_capacity(index_count * 2);
        let span = self.opt.find_span(self.node).unwrap_or(Span::empty());
        let region = self.opt.graph[self.node].parent.unwrap();
        let src_value = self.opt.graph.inport_src(self.node.input(0)).unwrap();

        let new_producer = self
            .opt
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

        self.opt
            .graph
            .replace_node_uses(self.node, new_producer)
            .unwrap();

        //Return successfuly
        Some(node_collector)
    }

    pub fn lower_mix(self) -> Option<Vec<NodeRef>> {
        //we canonicalize this to
        //mix(x,y,a) => x*(1-a) + y*a

        if !self.allow_approximation {
            return None;
        }

        let region = self.opt.graph[self.node].parent.unwrap();
        let span = self.opt.find_span(self.node).unwrap_or(Span::empty());
        let mut node_collector = Vec::with_capacity(4);

        let x_src = self.opt.graph.inport_src(self.node.input(0)).unwrap();
        let y_src = self.opt.graph.inport_src(self.node.input(1)).unwrap();
        let a_src = self.opt.graph.inport_src(self.node.input(2)).unwrap();

        let a_ty = self.opt.get_out_type_mut(a_src).unwrap();
        let one = self.opt.splat_scalar(region, ImmScalar::new(1.0), a_ty);
        let new_result = self
            .opt
            .graph
            .on_region(&region, |g| {
                let one_minus = route_new!(g, BinaryArithOp::Sub, span.clone(), [one, a_src]);
                node_collector.push(one_minus);

                let x_times = route_new!(
                    g,
                    BinaryArithOp::Mul,
                    span.clone(),
                    [x_src, one_minus.output(0)]
                );
                node_collector.push(x_times);

                let y_times = route_new!(g, BinaryArithOp::Mul, span.clone(), [y_src, a_src]);
                node_collector.push(y_times);

                let result = route_new!(
                    g,
                    BinaryArithOp::Add,
                    span,
                    [x_times.output(0), y_times.output(0)]
                );
                node_collector.push(result);

                result
            })
            .unwrap();

        self.opt
            .graph
            .replace_node_uses(self.node, new_result)
            .unwrap();
        Some(node_collector)
    }
    pub fn lower_clamp(self) -> Option<Vec<NodeRef>> {
        let x_src = self.opt.graph.inport_src(self.node.input(0)).unwrap();
        let min_src = self.opt.graph.inport_src(self.node.input(1)).unwrap();
        let max_src = self.opt.graph.inport_src(self.node.input(2)).unwrap();

        let region = self.opt.graph[self.node].parent.clone().unwrap();
        let span = self.opt.find_span(self.node).unwrap_or(Span::empty());
        let mut node_collector = Vec::with_capacity(2);

        let result = self
            .opt
            .graph
            .on_region(&region, |g| {
                let inner_max = route_new!(g, BuildinOp::Max, span.clone(), [x_src, min_src]);
                node_collector.push(inner_max);

                let inner_min = route_new!(
                    g,
                    BuildinOp::Min,
                    span.clone(),
                    [inner_max.output(0), max_src]
                );
                node_collector.push(inner_min);

                inner_min
            })
            .unwrap();

        //if succesful, replace
        self.opt.graph.replace_node_uses(self.node, result).unwrap();
        Some(node_collector)
    }
    pub fn lower_cross(self) -> Option<Vec<NodeRef>> {
        let extract_component_count = |ty: &Ty| {
            if ty.is_vector() {
                Some(ty.shape().unwrap().component_count())
            } else {
                None
            }
        };

        let index_count = match self.opt.get_in_type_mut(self.node.input(0)).unwrap() {
            Ty::Interval(i) => extract_component_count(i.as_ref())?,
            other => extract_component_count(&other)?,
        };

        let region = self.opt.graph[self.node].parent.unwrap();
        let span = self.opt.find_span(self.node).unwrap_or(Span::empty());
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

        let a_src = self.opt.graph.inport_src(self.node.input(0)).unwrap();
        let b_src = self.opt.graph.inport_src(self.node.input(1)).unwrap();

        let result = match index_count {
            3 => {
                //reduce into
                // [ a.y * b.z - a.z * b.y,
                //   a.z * b.x - a.x * b.z,
                //   a.x * b.y - a.y * b.x ]
                self.opt
                    .graph
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
            _ => return None,
        };

        self.opt
            .graph
            .replace_outport_uses(self.node.output(0), result.output(0))
            .unwrap();

        Some(node_collector)
    }

    pub fn lower_min_max(self, is_min: bool) -> Option<Vec<NodeRef>> {
        //Inspired by this: https://auto-differentiation.github.io/ref/math/
        //implement as:
        //min: (x + y - abs(x-y)) / 2
        //max: (x + y + abs(x-y)) / 2
        if !self.allow_approximation {
            return None;
        }

        let inner_op = if is_min {
            BinaryArithOp::Sub
        } else {
            BinaryArithOp::Add
        };

        let ty = self
            .opt
            .get_out_type_mut(self.node.output(0))
            .expect("Expected type to be set!");
        let region = self.opt.graph[self.node].parent.unwrap();
        let span = self.opt.find_span(self.node).unwrap_or(Span::empty());
        let imm_two = self
            .opt
            .splat_scalar(region, ImmScalar::new(2.0), ty.clone());

        let x_src = self.opt.graph.inport_src(self.node.input(0)).unwrap();
        let y_src = self.opt.graph.inport_src(self.node.input(1)).unwrap();
        let x_ty = self.opt.get_out_type_mut(x_src).unwrap();
        let y_ty = self.opt.get_out_type_mut(y_src).unwrap();
        assert!(x_ty == y_ty);
        let mut node_collector = Vec::with_capacity(4);

        let output = self
            .opt
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
        self.opt.graph.replace_node_uses(self.node, output).unwrap();

        Some(node_collector)
    }

    ///Unrolls a matrix-matrix, matrix-vector and vector-matrix multiplications
    /// into _simple_ scalar multiplication.
    pub fn unroll_multiplication(self) -> Option<Vec<NodeRef>> {
        //checkout what kind of inputs / outputs we have
        let left_src = self.opt.graph.inport_src(self.node.input(0)).unwrap();
        let right_src = self.opt.graph.inport_src(self.node.input(1)).unwrap();

        let left_type = self.opt.get_out_type_mut(left_src).unwrap();
        let right_type = self.opt.get_out_type_mut(right_src).unwrap();

        match (left_type.clone(), right_type.clone()) {
            (
                Ty::Shaped {
                    shape: Shape::Matrix { .. },
                    ty: DataType::Real,
                },
                Ty::Shaped {
                    shape: Shape::Vec { .. },
                    ty: DataType::Real,
                },
            ) => {
                //this is canonicalized into a unrolled multiplication
                return self.unroll_matrix_vector(left_type, right_type, left_src, right_src);
            }
            (
                Ty::Shaped {
                    shape: Shape::Vec { .. },
                    ty: DataType::Real,
                },
                Ty::Shaped {
                    shape: Shape::Matrix { .. },
                    ty: DataType::Real,
                },
            ) => {
                //this is canonicalized into a unrolled multiplication
                return self.unroll_vector_matrix(left_type, right_type, left_src, right_src);
            }
            (
                Ty::Shaped {
                    shape: Shape::Matrix { .. },
                    ty: DataType::Real,
                },
                Ty::Shaped {
                    shape: Shape::Matrix { .. },
                    ty: DataType::Real,
                },
            ) => {
                return self.unroll_matrix_matrix(left_type, right_type, left_src, right_src);
            }
            //All other multiplications can't be unrolled
            _ => None,
        }
    }
}
