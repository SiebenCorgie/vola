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
    smallvec::smallvec,
    NodeRef, SmallColl,
};
use rvsdg_viewer::View;
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp},
        buildin::{Buildin, BuildinOp},
        relational::{BinaryRel, BinaryRelOp},
    },
    autodiff::activity::Activity,
    imm::ImmScalar,
    OptEdge, OptNode, Optimizer,
};

use super::AutoDiffError;

impl Optimizer {
    pub(super) fn build_diff_buildin(
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
                let zero_splat = self.emit_zero_for_port(region, node.output(0));

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
}
