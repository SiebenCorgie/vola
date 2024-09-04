/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation},
    region::RegionLocation,
    smallvec::{smallvec, SmallVec},
    NodeRef, SmallColl,
};
use rvsdg_viewer::View;
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp},
        buildin::{Buildin, BuildinOp},
        logical::{BinaryBool, UnaryBool},
        matrix::UnaryMatrix,
        relational::{BinaryRel, BinaryRelOp},
        trigonometric::{Trig, TrigOp},
        ConstantIndex, Construct,
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
        //FIXME: It _would be cool_ to use TypeId::of::<T> as a match
        //       criterion to dispatch the actual node here. However, that is
        //       currently unstabel (https://github.com/rust-lang/rust/issues/77125)
        //       as well as const match, which is also unstable
        //       (https://github.com/rust-lang/rust/issues/76001).
        //
        //       For now we do the _test till we can branch_ style dispatch, which
        //       takes longer. But using a dynamic-dispatched function in the DialectNode
        //       ain't it either, since this only concerns the alge-dialect.

        if self.is_node_type::<ConstantIndex>(node) {
            return self.build_diff_constant_index(region, node);
        }

        if self.is_node_type::<Construct>(node) {
            return self.build_diff_constant_construct(region, node);
        }

        if self.is_node_type::<BinaryArith>(node) {
            return self.build_diff_binary_arith(region, node, activity);
        }

        if self.is_node_type::<UnaryArith>(node) {
            return self.build_diff_unary_arith(region, node);
        }

        if self.is_node_type::<BinaryBool>(node) {
            return self.build_diff_binary_logic(region, node);
        }

        if self.is_node_type::<UnaryBool>(node) {
            return self.build_diff_unary_logic(region, node);
        }

        if self.is_node_type::<UnaryMatrix>(node) {
            return self.build_diff_unary_matrix(region, node);
        }

        if self.is_node_type::<Trig>(node) {
            return self.build_diff_trig(region, node);
        }
        if self.is_node_type::<Buildin>(node) {
            return self.build_diff_buildin(region, node, activity);
        }

        Err(AutoDiffError::ActivityExplorationRegionError)
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
        //Just build a vector of all derivatives
        let span = self.find_span(node.into()).unwrap_or(Span::empty());

        let srcs = self.graph[node].input_srcs(&self.graph);
        let const_width = srcs.len();

        let mut subdiffs = SmallVec::new();
        let result = self
            .graph
            .on_region(&region, |g| {
                let index_diff = g.insert_node(OptNode::new(
                    Construct::new().with_inputs(const_width),
                    span,
                ));

                index_diff
            })
            .unwrap();

        for (inidx, src) in srcs.into_iter().enumerate() {
            if let Some(src) = src {
                subdiffs.push((src, smallvec![result.input(inidx)]));
            }
        }

        Ok((result.output(0), subdiffs))
    }

    fn build_diff_binary_arith(
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
        let span = self.find_span(node.into()).unwrap_or(Span::empty());
        let src = self.graph.inport_src(node.input(0)).unwrap();
        match self.graph[node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<UnaryArith>()
            .unwrap()
            .op
            .clone()
        {
            UnaryArithOp::Abs => {
                //Abs can not be diffed for f(x) == 0, however, for all others
                // |f(x)|' = (f(x) / |f(x)|) * f'(x) exists.
                //
                // for x == 0 this will naturally be Nan, since |f(x)| == 0 -> f(x) / 0.0 == Nan.
                if self.config.autodiff.abort_on_undiff {
                    return Err(AutoDiffError::UndiffNode(format!(
                        "{}",
                        self.graph[node].name()
                    )));
                }

                let (subdiff_dst, output) = self
                    .graph
                    .on_region(&region, |g| {
                        let (abs_f, _) = g
                            .connect_node(
                                OptNode::new(UnaryArith::new(UnaryArithOp::Abs), span.clone()),
                                &[src],
                            )
                            .unwrap();

                        let (f_div_abs_div, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Div), span.clone()),
                                &[src, abs_f.output(0)],
                            )
                            .unwrap();

                        let (mul, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                &[f_div_abs_div.output(0)],
                            )
                            .unwrap();

                        let subdiff_dst = mul.input(1);

                        (subdiff_dst, mul.output(0))
                    })
                    .unwrap();

                let subdiff = smallvec![(src, smallvec![subdiff_dst])];
                Ok((output, subdiff))
            }
            UnaryArithOp::Neg => {
                //special case of (cf)' => c f' for c=constant
                //in this case c = -1.
                let f_src = self.graph.inport_src(node.input(0)).unwrap();
                let f_ty = self.find_type(&f_src.into()).unwrap();
                let negone = self.splat_scalar(region, ImmScalar::new(-1.0), f_ty);
                let (diff_dst, result) = self
                    .graph
                    .on_region(&region, |g| {
                        let (mul, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                &[negone],
                            )
                            .unwrap();
                        let diff_dst = mul.input(1);

                        (diff_dst, mul)
                    })
                    .unwrap();

                let subdiff = smallvec![(f_src, smallvec![diff_dst])];
                Ok((result.output(0), subdiff))
            }
            UnaryArithOp::Ceil
            | UnaryArithOp::Floor
            | UnaryArithOp::Round
            | UnaryArithOp::Fract => {
                //Emit as zero (and do not subdiff chain rule, since that will always be zero)
                //see: https://math.stackexchange.com/questions/305949/derivative-of-floor-function
                //
                //Abort if the exact thing is needed
                if self.config.autodiff.abort_on_undiff {
                    return Err(AutoDiffError::UndiffNode(format!(
                        "{}",
                        self.graph[node].name()
                    )));
                }

                let x_src = self.graph.inport_src(node.input(0)).unwrap();
                let x_ty = self.find_type(&x_src.into()).unwrap();
                let zero = self.splat_scalar(region, ImmScalar::new(0.0), x_ty);

                Ok((zero, SmallColl::new()))
            }
        }
    }

    fn build_diff_binary_logic(
        &mut self,
        _region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        Err(AutoDiffError::NoAdImpl(self.graph[node].name().to_string()))
    }

    fn build_diff_unary_logic(
        &mut self,
        _region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        Err(AutoDiffError::NoAdImpl(self.graph[node].name().to_string()))
    }

    fn build_diff_unary_matrix(
        &mut self,
        _region: RegionLocation,
        node: NodeRef,
    ) -> Result<
        (
            OutportLocation,
            SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
        ),
        AutoDiffError,
    > {
        Err(AutoDiffError::NoAdImpl(self.graph[node].name().to_string()))
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
        let span = self.find_span(node.into()).unwrap_or(Span::empty());
        let src = self.graph.inport_src(node.input(0)).unwrap();
        match self.graph[node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<Trig>()
            .unwrap()
            .op
            .clone()
        {
            TrigOp::Sin => {
                //sin' = cos
                let cos_output = self
                    .graph
                    .on_region(&region, |g| {
                        let (f_diff, _) = g
                            .connect_node(OptNode::new(Trig::new(TrigOp::Cos), span), &[src])
                            .unwrap();
                        f_diff.output(0)
                    })
                    .unwrap();

                Ok(self.build_chain_rule_for(&region, cos_output, src))
            }
            TrigOp::Cos => {
                //cos' = -sin
                let diffout = self
                    .graph
                    .on_region(&region, |g| {
                        let (sinout, _) = g
                            .connect_node(
                                OptNode::new(Trig::new(TrigOp::Sin), span.clone()),
                                &[src],
                            )
                            .unwrap();
                        let (negged, _) = g
                            .connect_node(
                                OptNode::new(UnaryArith::new(UnaryArithOp::Neg), span),
                                &[sinout.output(0)],
                            )
                            .unwrap();
                        negged.output(0)
                    })
                    .unwrap();
                Ok(self.build_chain_rule_for(&region, diffout, src))
            }
            TrigOp::Tan => {
                //See https://math.stackexchange.com/questions/1108131/what-is-cos%C2%B2x
                todo!()
            }
            _ => todo!(),
        }
    }

    fn build_diff_buildin(
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
        let span = self.find_span(node.into()).unwrap_or(Span::empty());
        let op = self.graph[node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<Buildin>()
            .unwrap()
            .op
            .clone();
        match op {
            BuildinOp::Dot | BuildinOp::Cross => {
                //Both can be diffed according to the product rule
                let u_src = self.graph.inport_src(node.input(0)).unwrap();
                let v_src = self.graph.inport_src(node.input(1)).unwrap();

                let (u_diff_dst, v_diff_dst, result) = self
                    .graph
                    .on_region(&region, |g| {
                        //NOTE is not commutative, so we have to pay attention to the order here
                        let udiff_op_v =
                            g.insert_node(OptNode::new(Buildin::new(op.clone()), span.clone()));
                        //preconnect u to v` x u
                        g.ctx_mut()
                            .connect(v_src, udiff_op_v.input(1), OptEdge::value_edge_unset())
                            .unwrap();
                        let u_diff_dst = udiff_op_v.input(0);

                        let u_op_diffv =
                            g.insert_node(OptNode::new(Buildin::new(op.clone()), span.clone()));
                        //preconnect v to v x u'
                        g.ctx_mut()
                            .connect(u_src, u_op_diffv.input(0), OptEdge::value_edge_unset())
                            .unwrap();
                        let v_diff_dst = u_op_diffv.input(1);

                        //add both
                        let (add, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Add), span.clone()),
                                &[udiff_op_v.output(0), u_op_diffv.output(0)],
                            )
                            .unwrap();

                        (u_diff_dst, v_diff_dst, add.output(0))
                    })
                    .unwrap();

                //add both post-diffs and return

                let mut subdiff = SmallColl::new();

                subdiff.push((u_src, smallvec![u_diff_dst]));
                subdiff.push((v_src, smallvec![v_diff_dst]));

                Ok((result, subdiff))
            }

            BuildinOp::SquareRoot => {
                //apply chain rule:
                // sqrt(f(x)) = [1.0 / (2.0 * sqrt(f(x)))] * f'(x)
                let f_src = self.graph.inport_src(node.input(0)).unwrap();
                let ty = self.find_type(&f_src.into()).unwrap();
                let imm_one = self.splat_scalar(region, ImmScalar::new(1.0), ty.clone());
                let imm_two = self.splat_scalar(region, ImmScalar::new(2.0), ty);

                //TODO / FIXME: We _could_ reformulate as 0.5 * sqrt(x) * f'(x).
                //      and checkout how much faster that is ...
                let div_result = self
                    .graph
                    .on_region(&region, |g| {
                        // 2.0 * expr
                        let (mul_inner, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                &[imm_two, node.output(0)],
                            )
                            .unwrap();

                        //div one
                        let (div_one, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Div), span.clone()),
                                &[imm_one, mul_inner.output(0)],
                            )
                            .unwrap();

                        div_one.output(0)
                    })
                    .unwrap();

                //make chain rule
                let (result, subdiff) = self.build_chain_rule_for(&region, div_result, f_src);

                self.names
                    .set(result.node.into(), "Sqrt chain-rule right".to_string());

                Ok((result, subdiff))
            }
            BuildinOp::Min | BuildinOp::Max => {
                if self.config.autodiff.abort_on_undiff {
                    return Err(AutoDiffError::UndiffNode(format!(
                        "{}",
                        self.graph[node].name()
                    )));
                }

                //NOTE: not really diff-abel, but we use
                //      this: https://math.stackexchange.com/questions/150960/derivative-of-the-fx-y-minx-y
                //      for now.
                //      Another way would be to use this approach while canonicalizing:
                //      https://auto-differentiation.github.io/tutorials/smoothed_math/#__tabbed_1_1

                let left_src = self.graph.inport_src(node.input(0)).unwrap();
                let right_src = self.graph.inport_src(node.input(0)).unwrap();

                let relop = if op == BuildinOp::Min {
                    //use a < check to emit 1
                    BinaryRelOp::Lte
                } else {
                    //use a > check to emit 1
                    BinaryRelOp::Gte
                };

                let feed_through_src = if op == BuildinOp::Min {
                    left_src
                } else {
                    right_src
                };

                //Zero value that is typed _correctly_
                let zero_splat = self.emit_zero_for_node(region, node);

                //The idea now is that we either route through the derivative of left, right, depending on
                //the test, and depending on the value of the rest expression.
                //
                //So for min(f,g), we route through f' for f < g, and 0, for f>g
                //   for max(f, g) we route through g' for f > g, and 0 for f<g.
                //
                //This is a shortening of the chain rule:
                // example for min(a, b);
                // f(g(x)) = f'(g(x)) * g'(x).
                //
                // f'(g(x)) = 1  iff a < b
                //            0  else
                // g'(x) = a'(x) iff a < b
                //          Nan  else
                //
                // can be reduced to:
                //
                // = a'(x) iff a<b
                // = 0    else
                //
                // This is not really correct (see the nan case), but we are a DSL, so its okay I guess...
                let (gamma_node, value_input, value_output) = self
                    .graph
                    .on_region(&region, |g| {
                        let (gamma_node, (value_input, zero_input, value_output)) = g
                            .new_decission(|g| {
                                let (entry_value, _) = g.add_entry_variable();
                                let (entry_zero, _) = g.add_entry_variable();
                                let (exid, exit_result) = g.add_exit_variable();

                                //Branch that emits whatever derivative
                                let _ = g.new_branch(|builder, bidx| {
                                    builder
                                        .connect_to_result(
                                            OutportLocation {
                                                node: entry_value.node,
                                                output: entry_value
                                                    .input
                                                    .map_to_in_region(bidx)
                                                    .unwrap(),
                                            },
                                            InputType::ExitVariableResult {
                                                branch: bidx,
                                                exit_variable: exid,
                                            },
                                        )
                                        .unwrap();
                                });

                                //Branch that emits zero
                                let _ = g.new_branch(|builder, bidx| {
                                    builder
                                        .connect_to_result(
                                            OutportLocation {
                                                node: entry_zero.node,
                                                output: entry_zero
                                                    .input
                                                    .map_to_in_region(bidx)
                                                    .unwrap(),
                                            },
                                            InputType::ExitVariableResult {
                                                branch: bidx,
                                                exit_variable: exid,
                                            },
                                        )
                                        .unwrap();
                                });

                                (entry_value, entry_zero, exit_result)
                            });

                        //hookup the zero input
                        g.ctx_mut()
                            .connect(zero_splat, zero_input, OptEdge::value_edge_unset())
                            .unwrap();

                        //Build comperator
                        let (cmp, _) = g
                            .connect_node(
                                OptNode::new(BinaryRel::new(relop), span),
                                &[left_src, right_src],
                            )
                            .unwrap();
                        //hookup the choosing criterium
                        g.ctx_mut()
                            .connect(
                                cmp.output(0),
                                gamma_node.as_inport_location(InputType::GammaPredicate),
                                OptEdge::value_edge_unset(),
                            )
                            .unwrap();

                        (gamma_node, value_input, value_output)
                    })
                    .unwrap();

                self.names
                    .set(gamma_node.into(), format!("diff({op:?}) choice"));
                Ok((
                    value_output,
                    //Tell the differ, that thae feed-through-src needs to be diffed, and hooked up to the
                    //gamma-node
                    smallvec![(feed_through_src, smallvec![value_input])],
                ))
            }
            BuildinOp::Exp => {
                //e^x == e^x
                Ok(self.build_chain_rule_for(
                    &region,
                    node.output(0),
                    self.graph.inport_src(node.input(0)).unwrap(),
                ))
            }
            BuildinOp::Pow => {
                //We have two cases here.
                //One is where the exponent is not active:
                //x^n => nx^(n-1)
                //One where the exponent is active
                //x^x => x^x * (1 + ln x).
                let x_src = self.graph.inport_src(node.input(0)).unwrap();
                let n_src = self.graph.inport_src(node.input(1)).unwrap();

                if activity.is_active_port(self, n_src) {
                    //the x^x case
                    return Err(AutoDiffError::NoAdImpl(format!(
                        "No AD impl for x^x (where the exponent is part of the derivative)"
                    )));
                } else {
                    //the x^n case

                    let nty = self.find_type(&n_src.into()).unwrap();
                    let one = self.splat_scalar(region, ImmScalar::new(1.0), nty);
                    let diffed_output = self
                        .graph
                        .on_region(&region, |g| {
                            //n-1
                            let (n_minus_one, _) = g
                                .connect_node(
                                    OptNode::new(
                                        BinaryArith::new(BinaryArithOp::Sub),
                                        span.clone(),
                                    ),
                                    &[n_src, one],
                                )
                                .unwrap();
                            //into exponent
                            let (x_times, _) = g
                                .connect_node(
                                    OptNode::new(Buildin::new(BuildinOp::Pow), span.clone()),
                                    &[x_src, n_minus_one.output(0)],
                                )
                                .unwrap();
                            //mul n
                            let (result, _) = g
                                .connect_node(
                                    OptNode::new(
                                        BinaryArith::new(BinaryArithOp::Mul),
                                        span.clone(),
                                    ),
                                    &[n_src, x_times.output(0)],
                                )
                                .unwrap();

                            result.output(0)
                        })
                        .unwrap();

                    Ok(self.build_chain_rule_for(&region, diffed_output, x_src))
                }
            }
            _other => Err(AutoDiffError::NoAdImpl(
                self.graph[node]
                    .node_type
                    .unwrap_simple_ref()
                    .name()
                    .to_string(),
            )),
        }
    }

    //Small helper that makes chaining derivatives easier (building the chain rule).
    //Basically for a given f(g(x)) you supply `f'(g(x))` (`diffed_output`), and tell it what the source of `g(x)` is (active_sub_src). It'll return the outport of `f'(g'(x)) * g'(x)` where `g'(x)` is enqued in the deferred list.
    fn build_chain_rule_for(
        &mut self,
        region: &RegionLocation,
        diffed_output: OutportLocation,
        active_sub_src: OutportLocation,
    ) -> (
        OutportLocation,
        SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
    ) {
        let mut subdiff = SmallColl::new();
        let mul_out = self
            .graph
            .on_region(region, |g| {
                let (mul, _edg) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Mul), Span::empty()),
                        &[diffed_output],
                    )
                    .unwrap();

                //enque
                subdiff.push((active_sub_src, smallvec![mul.input(1)]));
                mul.output(0)
            })
            .unwrap();

        (mul_out, subdiff)
    }
}
