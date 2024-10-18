/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use crate::{
    autodiff::{activity::Activity, AutoDiffError},
    common::Ty,
    Optimizer,
};

use rvsdg::{
    edge::{InportLocation, OutportLocation},
    region::RegionLocation,
    smallvec::smallvec,
    NodeRef, SmallColl,
};
use rvsdg_viewer::View;
use vola_common::Span;

use crate::{
    alge::arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp},
    OptEdge, OptNode,
};

impl Optimizer {
    pub(super) fn build_diff_binary_arith(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
        activity: &mut Activity,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        let op = self.graph[node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<BinaryArith>()
            .unwrap()
            .op
            .clone();
        let span = self.find_span(node.into()).unwrap_or(Span::empty());
        //NOTE: Safe, since this _must_ be a binary op
        let left_src = self.graph.inport_src(node.input(0)).unwrap();
        let right_src = self.graph.inport_src(node.input(1)).unwrap();

        let mut subdiff = SmallColl::new();

        match op {
            BinaryArithOp::Add | BinaryArithOp::Sub => {
                //Build new node that takes both derivatives
                let result = self
                    .graph
                    .on_region(&region, |g| {
                        let node = g.insert_node(OptNode::new(BinaryArith::new(op), span));

                        //Add both diffst to the deferred list
                        subdiff.push((left_src, smallvec![node.input(0)]));
                        subdiff.push((right_src, smallvec![node.input(1)]));

                        self.names.set(node.into(), "Sum/Diff-Rule".to_string());
                        node.output(0)
                    })
                    .unwrap();

                Ok((result, subdiff))
            }
            BinaryArithOp::Mul => {
                //We need to distinguish two cases here.
                //- The product-rule (where both parts are _active_)
                //- The constan-factor-rule (where only one is active).
                //
                //There is also the added caveat, that, for the double_active part,
                //there should not be a matrix-matrix, matrix-vector or vector-matrix multiplication.
                //Only scalar-scalar are _right now_ implemented

                let left_type = self.find_type(&left_src.into()).unwrap();
                let right_type = self.find_type(&right_src.into()).unwrap();
                println!("Yeet[{node:?}]: {left_type:?}, {right_type:?}");

                //We have implementations for scalar-scalar, scalar-vector, vector-scalar, vector-vector.
                match (left_type, right_type) {
                    (Ty::Scalar, Ty::Scalar) => {}
                    (Ty::Scalar, Ty::Vector { .. }) | (Ty::Vector { .. }, Ty::Scalar) => {}
                    (Ty::Vector { width: w2 }, Ty::Vector { width: w1 }) => {
                        assert!(w2 == w1);
                    }
                    (a, b) => {
                        println!("That Aint it");
                        return Err(AutoDiffError::NoAdImpl(format!(
                            "Multiplication derivative not implemented for {a} * {b}"
                        )));
                    }
                }

                match (
                    activity.is_active_port(self, left_src),
                    activity.is_active_port(self, right_src),
                ) {
                    (true, true) => {
                        //product-rule: (left is f, right is g):
                        //(f(x)*g(x))' = f'(x) * g(x) + f(x) * g'(x).
                        //turns to vector-product rule for vectorns

                        let result = self
                            .graph
                            .on_region(&region, |g| {
                                let mul_left = g.insert_node(OptNode::new(
                                    BinaryArith::new(BinaryArithOp::Mul),
                                    span.clone(),
                                ));

                                //connect right side (g(x))
                                g.ctx_mut()
                                    .connect(
                                        right_src,
                                        mul_left.input(1),
                                        OptEdge::value_edge_unset(),
                                    )
                                    .unwrap();
                                //defere connection of f'(x)
                                subdiff.push((left_src, smallvec![mul_left.input(0)]));

                                self.names
                                    .set(mul_left.into(), "ProductRule f'(x) + g(x)".to_string());
                                let (mul_right, _) = g
                                    .connect_node(
                                        OptNode::new(
                                            BinaryArith::new(BinaryArithOp::Mul),
                                            span.clone(),
                                        ),
                                        &[left_src],
                                    )
                                    .unwrap();
                                //defer connection of differentiated right_src
                                subdiff.push((right_src, smallvec![mul_right.input(1)]));

                                self.names
                                    .set(mul_right.into(), "ProductRule f(x) + g'(x)".to_string());

                                let (added, _) = g
                                    .connect_node(
                                        OptNode::new(BinaryArith::new(BinaryArithOp::Add), span),
                                        &[mul_left.output(0), mul_right.output(0)],
                                    )
                                    .unwrap();

                                self.names
                                    .set(added.into(), "ProductRule left + right".to_string());

                                added.output(0)
                            })
                            .unwrap();

                        return Ok((result, subdiff));
                    }
                    (false, false) => panic!("{node:?} should not be active"),
                    (is_left_diff, is_right_diff) => {
                        assert!(is_left_diff != is_right_diff);
                        println!("Had {is_left_diff}, {is_right_diff}");
                        //constant-factor-rule (only one is active).
                        let diff_defered_src = if is_left_diff { left_src } else { right_src };
                        let constant_src = if is_left_diff { right_src } else { left_src };

                        let result = self
                            .graph
                            .on_region(&region, |g| {
                                let (node, _edg) = g
                                    .connect_node(
                                        OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span),
                                        &[constant_src],
                                    )
                                    .unwrap();

                                subdiff.push((diff_defered_src, smallvec![node.input(1)]));

                                self.names
                                    .set(node.into(), "ConstantFactorRule".to_string());
                                node.output(0)
                            })
                            .unwrap();
                        Ok((result, subdiff))
                    }
                }
            }
            BinaryArithOp::Div => {
                //Quotient rule
                let u_src = self.graph.inport_src(node.input(0)).unwrap();
                let v_src = self.graph.inport_src(node.input(1)).unwrap();

                let (pd_u, pd_v, output) = self
                    .graph
                    .on_region(&region, |g| {
                        //the u' * v term, but post-diff u`
                        let (mul_left, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                &[v_src],
                            )
                            .unwrap();
                        let pd_u = mul_left.input(1);

                        let (mul_right, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                &[u_src],
                            )
                            .unwrap();
                        let pd_v = mul_right.input(1);

                        //sub both
                        let (sub, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Sub), span.clone()),
                                &[mul_left.output(0), mul_right.output(0)],
                            )
                            .unwrap();

                        let (v_pow_two, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                &[v_src, v_src],
                            )
                            .unwrap();

                        let (div, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Div), span.clone()),
                                &[sub.output(0), v_pow_two.output(0)],
                            )
                            .unwrap();

                        (pd_u, pd_v, div.output(0))
                    })
                    .unwrap();
                //post div u and v

                subdiff.push((u_src, smallvec![pd_u]));
                subdiff.push((v_src, smallvec![pd_v]));

                Ok((output, subdiff))
            }
            BinaryArithOp::Mod => {
                //If allowed diff |x| => x / |x|
                //NOTE that this is undefined on x == 0
                if self.config.autodiff.abort_on_undiff {
                    return Err(AutoDiffError::UndiffNode(format!(
                        "{}",
                        self.graph[node].name()
                    )));
                }

                let x_src = self.graph.inport_src(node.input(0)).unwrap();
                let result = self
                    .graph
                    .on_region(&region, |g| {
                        let (x_abs, _) = g
                            .connect_node(
                                OptNode::new(UnaryArith::new(UnaryArithOp::Abs), span.clone()),
                                &[x_src],
                            )
                            .unwrap();

                        let (div_out, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Div), span),
                                &[x_src, x_abs.output(0)],
                            )
                            .unwrap();

                        div_out
                    })
                    .unwrap();

                Ok(self.build_chain_rule_for(&region, result.output(0), x_src))
            }
        }
    }
}
