/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
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
        activity: &Activity,
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
            return self.build_diff_buildin(region, node);
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
        let sub_src = self.graph.inport_src(node.input(0)).unwrap();

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
                    activity.is_active(left_src.node).unwrap_or(false),
                    activity.is_active(right_src.node).unwrap_or(false),
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
                //For abs we currently do something similar to the min/max case.
                //this time, do:
                // f'(x)          (for x > 0)
                // `-1.0 * f'(x)` (for x < 1)
                // 0.0            (for x == 0)

                let src = self.graph.inport_src(node.input(0)).unwrap();
                let src_ty = self.find_type(&src.into()).unwrap();
                let splatted_zero = self.splat_scalar(region, ImmScalar::new(1.0), src_ty);

                let (gamma_node, value_output, diffinput) = self
                    .graph
                    .on_region(&region, |g| {
                        let (gammanode, (value_output, value_input, diffinput, zero_input)) = g
                            .new_decission(|gb| {
                                let (diff_input, _) = gb.add_entry_variable();
                                let (value_input, _) = gb.add_entry_variable();
                                let (zero_input, _) = gb.add_entry_variable();
                                let (exid, value_output) = gb.add_exit_variable();

                                //case x == 0
                                gb.new_branch(|bb, bidx| {
                                    //hookup for x == 0
                                    bb.connect_to_result(
                                        OutportLocation {
                                            node: zero_input.node,
                                            output: zero_input
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

                                //now build the x > 0 || x < 0 cases
                                gb.new_branch(|bb, bidx| {
                                    let (is_gt_zero, _) = bb
                                        .connect_node(
                                            OptNode::new(
                                                BinaryRel::new(BinaryRelOp::Gt),
                                                span.clone(),
                                            ),
                                            &[
                                                OutportLocation {
                                                    node: value_input.node,
                                                    output: value_input
                                                        .input
                                                        .map_to_in_region(bidx)
                                                        .unwrap(),
                                                },
                                                OutportLocation {
                                                    node: zero_input.node,
                                                    output: zero_input
                                                        .input
                                                        .map_to_in_region(bidx)
                                                        .unwrap(),
                                                },
                                            ],
                                        )
                                        .unwrap();

                                    //build both branches, that either route through the diff-value (true), or negate it and route through (false)

                                    let (gt_gamma, (gt_result, gt_diff_input)) =
                                        bb.new_decission(|gtbuilder| {
                                            let (gt_diff_input, _) = gtbuilder.add_entry_variable();
                                            let (gt_exid, gt_result) =
                                                gtbuilder.add_exit_variable();

                                            //the x > 0 => f'(x)
                                            gtbuilder.new_branch(|bb, bidx| {
                                                bb.connect_to_result(
                                                    OutportLocation {
                                                        node: gt_diff_input.node,
                                                        output: gt_diff_input
                                                            .input
                                                            .map_to_in_region(bidx)
                                                            .unwrap(),
                                                    },
                                                    InputType::ExitVariableResult {
                                                        branch: bidx,
                                                        exit_variable: gt_exid,
                                                    },
                                                )
                                                .unwrap();
                                            });

                                            //the x < 0 => -f'(x)
                                            //we use Neg for that

                                            gtbuilder.new_branch(|bb, bidx| {
                                                let (neg_diff, _) = bb
                                                    .connect_node(
                                                        OptNode::new(
                                                            UnaryArith::new(UnaryArithOp::Neg),
                                                            span.clone(),
                                                        ),
                                                        &[OutportLocation {
                                                            node: gt_diff_input.node,
                                                            output: gt_diff_input
                                                                .input
                                                                .map_to_in_region(bidx)
                                                                .unwrap(),
                                                        }],
                                                    )
                                                    .unwrap();

                                                bb.connect_to_result(
                                                    neg_diff.output(0),
                                                    InputType::ExitVariableResult {
                                                        branch: bidx,
                                                        exit_variable: gt_exid,
                                                    },
                                                )
                                                .unwrap();
                                            });

                                            (gt_result, gt_diff_input)
                                        });

                                    //hookup the criterion, the diff-value input, and route the result to the outer gamma's result
                                    bb.ctx_mut()
                                        .connect(
                                            is_gt_zero.output(0),
                                            gt_gamma.as_inport_location(InputType::GammaPredicate),
                                            OptEdge::value_edge_unset(),
                                        )
                                        .unwrap();

                                    bb.ctx_mut()
                                        .connect(
                                            OutportLocation {
                                                node: diff_input.node,
                                                output: diff_input
                                                    .input
                                                    .map_to_in_region(bidx)
                                                    .unwrap(),
                                            },
                                            gt_diff_input,
                                            OptEdge::value_edge_unset(),
                                        )
                                        .unwrap();

                                    bb.connect_to_result(
                                        gt_result,
                                        InputType::ExitVariableResult {
                                            branch: bidx,
                                            exit_variable: exid,
                                        },
                                    )
                                    .unwrap()
                                });

                                (value_output, value_input, diff_input, zero_input)
                            });

                        //build x == 0 criterion
                        let (is_zero, _) = g
                            .connect_node(
                                OptNode::new(BinaryRel::new(BinaryRelOp::Eq), span),
                                &[src, splatted_zero],
                            )
                            .unwrap();
                        //hookup to gamma node
                        g.ctx_mut()
                            .connect(
                                is_zero.output(0),
                                gammanode.as_inport_location(InputType::GammaPredicate),
                                OptEdge::value_edge_unset(),
                            )
                            .unwrap();

                        //hookup zero splat
                        g.ctx_mut()
                            .connect(splatted_zero, zero_input, OptEdge::value_edge_unset())
                            .unwrap();

                        //feed the actual value and into the gamma node
                        g.ctx_mut()
                            .connect(src, value_input, OptEdge::value_edge_unset())
                            .unwrap();

                        (gammanode, value_output, diffinput)
                    })
                    .unwrap();

                let subdiff = smallvec![(src, smallvec![diffinput])];
                Ok((value_output, subdiff))
            }
            other => Err(AutoDiffError::NoAdImpl(format!(
                "UnaryArithOp {other:?} not implemented!"
            ))),
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
            BuildinOp::SquareRoot => {
                //apply chain rule:
                // sqrt(f(x)) = [1.0 / (2.0 * sqrt(f(x)))] * f'(x)
                let f_src = self.graph.inport_src(node.input(0)).unwrap();

                //TODO / FIXME: We _could_ reformulate as 0.5 * sqrt(x) * f'(x).
                //      and checkout how much faster that is ...
                let div_result = self
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
