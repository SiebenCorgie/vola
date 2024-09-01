/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Implements all canonicalization transformations

use rvsdg::{
    edge::{InportLocation, InputType},
    region::RegionLocation,
    NodeRef, SmallColl,
};
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp},
        buildin::{Buildin, BuildinOp},
        ConstantIndex,
    },
    common::Ty,
    imm::ImmScalar,
    OptError, OptNode, Optimizer,
};

impl Optimizer {
    pub(crate) fn handle_canon_unary(
        &mut self,
        region: &RegionLocation,
        node: NodeRef,
    ) -> Result<(), OptError> {
        let span = self.find_span(node.into()).unwrap_or(Span::empty());
        if let Some(unary_node) = self.try_unwrap_node::<UnaryArith>(node) {
            match &unary_node.op {
                UnaryArithOp::Abs => {
                    if !self.config.autodiff.canonicalize_undiff {
                        return Ok(());
                    }

                    //transform into a smooth abs function
                    // as |f(x)| => sqrt(f(x)^2 + 0.001 )
                    let f_src = self.graph.inport_src(node.input(0)).unwrap();
                    let ty = self.find_type(&f_src.into()).unwrap();
                    let c_splat = self.splat_scalar(
                        *region,
                        ImmScalar::new(self.config.autodiff.smooth_abs_c),
                        ty.clone(),
                    );

                    let new_result = self
                        .graph
                        .on_region(&region, |g| {
                            let (fx_square, _) = g
                                .connect_node(
                                    OptNode::new(
                                        BinaryArith::new(BinaryArithOp::Mul),
                                        span.clone(),
                                    ),
                                    &[f_src, f_src],
                                )
                                .unwrap();
                            let (with_const, _) = g
                                .connect_node(
                                    OptNode::new(
                                        BinaryArith::new(BinaryArithOp::Add),
                                        span.clone(),
                                    ),
                                    &[fx_square.output(0), c_splat],
                                )
                                .unwrap();
                            let (sqrt, _) = g
                                .connect_node(
                                    OptNode::new(Buildin::new(BuildinOp::SquareRoot), span.clone()),
                                    &[with_const.output(0)],
                                )
                                .unwrap();

                            sqrt
                        })
                        .unwrap();

                    self.graph.replace_node_uses(node, new_result)?;
                    Ok(())
                }
                _ => Ok(()),
            }
        } else {
            panic!("Should be unary arith");
        }
    }

    pub(crate) fn handle_canon_buildin(
        &mut self,
        region: &RegionLocation,
        node: NodeRef,
    ) -> Result<(), OptError> {
        let span = self.find_span(node.into()).unwrap_or(Span::empty());
        if let Some(buildin_node) = self.try_unwrap_node::<Buildin>(node) {
            match &buildin_node.op {
                //Transform the buildin op to a square-root and multiplied parts
                BuildinOp::Length => {
                    let src_value = self
                        .graph
                        .inport_src(InportLocation {
                            node,
                            input: InputType::Input(0),
                        })
                        .unwrap();
                    let new_producer = self
                        .graph
                        .on_region(region, |g| {
                            //find the input type to the length node. We use that to
                            //determin how often we need to index (and square)
                            //the vector
                            let index_count = {
                                //sample first edge, must be connected at least once...
                                let edg = g.ctx()[InportLocation {
                                    node,
                                    input: InputType::Input(0),
                                }]
                                .edge
                                .unwrap();
                                let ty = if let Some(t) = g.ctx()[edg].ty.get_type() {
                                    t
                                } else {
                                    //TODO: do that better
                                    panic!("Encountered untyped edge while canonicalizing");
                                };

                                if let Ty::Vector { width } = ty {
                                    *width
                                } else {
                                    panic!("Was no vector, was {ty}");
                                }
                            };

                            //Index into vector n-times
                            let mut indices = SmallColl::new();
                            for idx in 0..index_count {
                                let (new_node, _edges) = g
                                    .connect_node(
                                        OptNode::new(ConstantIndex::new(idx), span.clone()),
                                        &[src_value],
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
                                            OptNode::new(
                                                BinaryArith::new(BinaryArithOp::Mul),
                                                span.clone(),
                                            ),
                                            &[indexed, indexed],
                                        )
                                        .unwrap();

                                    node.output(0)
                                })
                                .collect::<SmallColl<_>>();

                            //Add all squared indices
                            assert!(squared.len() >= 2);
                            let (mut last_add, _) = g
                                .connect_node(
                                    OptNode::new(
                                        BinaryArith::new(BinaryArithOp::Add),
                                        span.clone(),
                                    ),
                                    &[squared[0], squared[1]],
                                )
                                .unwrap();
                            //now build the _staggered_ add chain.
                            for next_idx in 2..squared.len() {
                                let (new_last, _) = g
                                    .connect_node(
                                        OptNode::new(
                                            BinaryArith::new(BinaryArithOp::Add),
                                            span.clone(),
                                        ),
                                        &[last_add.output(0), squared[next_idx]],
                                    )
                                    .unwrap();

                                last_add = new_last;
                            }

                            let (sqrt, _) = g
                                .connect_node(
                                    OptNode::new(Buildin::new(BuildinOp::SquareRoot), span),
                                    &[last_add.output(0)],
                                )
                                .unwrap();

                            sqrt
                        })
                        .unwrap();

                    self.graph.replace_node_uses(node, new_producer)?;
                    Ok(())
                }
                BuildinOp::Min | BuildinOp::Max => {
                    if !self.config.autodiff.canonicalize_undiff {
                        return Ok(());
                    }

                    //Inspired by this: https://auto-differentiation.github.io/ref/math/
                    //implement as:
                    //min: (x + y - abs(x-y)) / 2
                    //max: (x + y + abs(x-y)) / 2

                    let inner_op = if buildin_node.op == BuildinOp::Min {
                        BinaryArithOp::Sub
                    } else {
                        BinaryArithOp::Add
                    };

                    let ty = self
                        .find_type(&node.output(0).into())
                        .expect("Expected type to be set!");
                    let imm_two = self.splat_scalar(*region, ImmScalar::new(2.0), ty.clone());

                    let x_src = self.graph.inport_src(node.input(0)).unwrap();
                    let y_src = self.graph.inport_src(node.input(1)).unwrap();
                    let x_ty = self.find_type(&x_src.into()).unwrap();
                    let y_ty = self.find_type(&y_src.into()).unwrap();
                    assert!(x_ty == y_ty);

                    let (abs_node, output) = self
                        .graph
                        .on_region(region, |g| {
                            let (x_min_y, _) = g
                                .connect_node(
                                    OptNode::new(
                                        BinaryArith::new(BinaryArithOp::Sub),
                                        span.clone(),
                                    ),
                                    &[x_src, y_src],
                                )
                                .unwrap();

                            let (abs, to_abs_edge) = g
                                .connect_node(
                                    OptNode::new(UnaryArith::new(UnaryArithOp::Abs), span.clone()),
                                    &[x_min_y.output(0)],
                                )
                                .unwrap();
                            //pre_set the abs-type, otherwise the shortcut to canonicalize abs won't work later on.
                            assert!(to_abs_edge.len() == 1);
                            g.ctx_mut().edge_mut(to_abs_edge[0]).ty.set_type(x_ty);

                            //add or subtract
                            let (min_max_add_sub, _) = g
                                .connect_node(
                                    OptNode::new(BinaryArith::new(inner_op), span.clone()),
                                    &[y_src, abs.output(0)],
                                )
                                .unwrap();

                            //add
                            let (add, _) = g
                                .connect_node(
                                    OptNode::new(
                                        BinaryArith::new(BinaryArithOp::Add),
                                        span.clone(),
                                    ),
                                    &[x_src, min_max_add_sub.output(0)],
                                )
                                .unwrap();
                            //div with two
                            let (div, _) = g
                                .connect_node(
                                    OptNode::new(
                                        BinaryArith::new(BinaryArithOp::Div),
                                        span.clone(),
                                    ),
                                    &[add.output(0), imm_two],
                                )
                                .unwrap();

                            (abs, div)
                        })
                        .unwrap();

                    //post-canonicalize the abs impl, if it needs to
                    self.handle_canon_node(abs_node)?;
                    //now replace callers of that min/max call with the new output
                    self.graph.replace_node_uses(node, output)?;
                    Ok(())
                }
                //All other buildin nodes are canon
                _ => Ok(()),
            }
        } else {
            //just panic, the handle_canon_node would be buggy in that case
            panic!("Buggy handle canon!")
        }
    }
}
