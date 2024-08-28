/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    edge::{InportLocation, OutportLocation},
    region::RegionLocation,
    smallvec::{smallvec, SmallVec},
    NodeRef, SmallColl,
};
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp},
        buildin::{Buildin, BuildinOp},
        ConstantIndex,
    },
    imm::ImmScalar,
    OptEdge, OptNode, Optimizer,
};

use super::{activity::Activity, AutoDiffError};

impl Optimizer {
    ///General dispatcher that can handle all _alge_ nodes differentiation.
    ///
    ///When called, produces the the derivative of this node (first outport), and return a list of ports that need to be further
    ///differentiated (OutportLocation) and where they need to be connected (InportLocation) too.
    ///
    /// Example: given a node `sqrt(sub_expr)`. The rule for sqrt is implemented as
    /// `f'(subexpr) = 1/2 * sqrt(subexpr) * subexpr' `
    ///
    /// So to _compleat_ the derivative, we need to known `subexpr'`. For that reason, the port, to which `subexpr'` is connected, as well as
    /// the source of `subexpr` is returned.
    ///
    /// In practice the differentiation handler just needs to differentiate that graph, and hook it up to the expected port.
    pub fn build_diff_value(
        &mut self,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        //FIXME: It _would be cool_ to use TypeId::of::<T> as a match
        //       criterion to dispatch the actual node here. However, that is
        //       currently unstabel (https://github.com/rust-lang/rust/issues/77125)
        //       as well as const match, which is also unstable
        //       (https://github.com/rust-lang/rust/issues/76001).
        //
        //       For now we do the _test till we can branch_ style dispatch, which
        //       takes longer. But using a dynamic-dispatched function in the DialectNode
        //       ain't it either, since this only concerns the alge-dialect.

        todo!()
    }

    fn build_diff_constant_index(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        //constant index is handled by just pushing the trace _over_ the node and reapplying the
        //index. Lets say we are indexing for x into a vector v = f(x), in that case we'd continue doing the the derivative with
        // respect to that vector, but once v' _arrives_ here, we'd still just consider the component, v`.x
        let access_index = self.graph[node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<ConstantIndex>()
            .unwrap()
            .access;
        let span = self.find_span(node.into()).unwrap_or(Span::empty());
        let sub_src = self.graph.inport_src(node.input(0)).unwrap();
        let mut subdiffs = SmallVec::new();
        let result = self
            .graph
            .on_region(&region, |g| {
                let index_diff =
                    g.insert_node(OptNode::new(ConstantIndex::new(access_index), span));

                //Add the input to the index to the subdiffs
                subdiffs.push((sub_src, smallvec![index_diff.input(0)]));
                index_diff.output(0)
            })
            .unwrap();

        Ok((result, subdiffs))
    }

    fn build_diff_constant_construct(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        todo!()
    }

    fn build_diff_binary_arith(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
        activity: &Activity,
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
                match (
                    activity.is_active(left_src.node),
                    activity.is_active(right_src.node),
                ) {
                    (true, true) => {
                        //product-rule: (left is f, right is g):
                        //(f(x)*g(x))' = f'(x) * g(x) + f(x) * g'(x).

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
                    (false, false) => panic!("Should not be active"),
                    (is_left_diff, is_right_diff) => {
                        assert!(is_left_diff != is_right_diff);
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
                todo!()
            }
            BinaryArithOp::Mod => {
                todo!()
            }
        }
    }

    fn build_diff_unary_arith(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        todo!()
    }

    fn build_diff_binary_logic(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        todo!()
    }

    fn build_diff_unary_logic(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        todo!()
    }

    fn build_diff_unary_matrix(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        todo!()
    }

    fn build_diff_trig(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        todo!()
    }

    fn build_diff_buildin(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        let span = self.find_span(node.into()).unwrap_or(Span::empty());
        let mut subdiffs = SmallColl::new();
        match self.graph[node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<Buildin>()
            .unwrap()
            .op
            .clone()
        {
            BuildinOp::SquareRoot => {
                //apply chain rule:
                // sqrt(f(x)) = [1.0 / (2.0 * sqrt(f(x)))] * f'(x)
                let f_src = self.graph.inport_src(node.input(0)).unwrap();

                //TODO / FIXME: We _could_ reformulate as 0.5 * sqrt(x) * f'(x).
                //      and checkout how much faster that is ...
                let result = self
                    .graph
                    .on_region(&region, |g| {
                        let imm_one =
                            g.insert_node(OptNode::new(ImmScalar::new(1.0), span.clone()));
                        let imm_two =
                            g.insert_node(OptNode::new(ImmScalar::new(2.0), span.clone()));
                        // 2.0 * expr
                        let (mul_inner, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                &[imm_two.output(0), node.output(0)],
                            )
                            .unwrap();

                        //div one
                        let (div_one, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Div), span.clone()),
                                &[imm_one.output(0), mul_inner.output(0)],
                            )
                            .unwrap();

                        //now mul the post-part
                        let (post_mul, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                &[div_one.output(0)],
                            )
                            .unwrap();

                        //add the second input deferred as derivative later on
                        subdiffs.push((f_src, smallvec![post_mul.input(1)]));

                        post_mul.output(0)
                    })
                    .unwrap();

                self.names
                    .set(result.node.into(), "Sqrt chain-rule right".to_string());

                Ok((result, subdiffs))
            }
            _other => Err(AutoDiffError::FwadNoImpl(node)),
        }
    }
}
