/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//!Handles the the actual differentiation operation of nodes, independent from the applied algorithm. Conceptually this means transforming a `f(x)` into `f'(x)`.

mod arith_binary;
mod buildin;
mod other;

use rvsdg::{
    edge::OutportLocation,
    region::RegionLocation,
    smallvec::{smallvec, SmallVec},
    NodeRef, SmallColl,
};
use rvsdg_viewer::View;
use vola_common::Span;

use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp},
        buildin::Buildin,
        logical::{BinaryBool, UnaryBool},
        matrix::UnaryMatrix,
        relational::BinaryRel,
        trigonometric::{Trig, TrigOp},
    },
    imm::ImmScalar,
    typelevel::{ConstantIndex, UniformConstruct},
    OptNode, Optimizer,
};

use super::{activity::Activity, AdResponse, AutoDiffError};

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
    pub(crate) fn build_diff_value(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
        activity: &mut Activity,
    ) -> Result<AdResponse, AutoDiffError> {
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

        if self.is_node_type::<UniformConstruct>(node) {
            return self.build_diff_constant_construct(region, node);
        }

        if self.is_node_type::<BinaryArith>(node) {
            return self.build_diff_binary_arith(region, node, activity);
        }

        if self.is_node_type::<UnaryArith>(node) {
            return self.build_diff_unary_arith(region, node);
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

        if self.is_node_type::<BinaryBool>(node) {
            return self.build_diff_binary_logic(region, node);
        }

        if self.is_node_type::<UnaryBool>(node) {
            return self.build_diff_unary_logic(region, node);
        }
        if self.is_node_type::<BinaryRel>(node) {
            return self.build_diff_binary_rel(region, node);
        }

        Err(AutoDiffError::NoAdImpl(format!(
            "No AD implementation for {} | {node}",
            self.graph[node].name()
        )))
    }

    fn build_diff_constant_index(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<AdResponse, AutoDiffError> {
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

        Ok(AdResponse::new(node.output(0), result).with_chained_derivatives(subdiffs))
    }

    fn build_diff_constant_construct(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<AdResponse, AutoDiffError> {
        //Just build a vector of all derivatives
        let span = self.find_span(node.into()).unwrap_or(Span::empty());

        let srcs = self.graph[node].input_srcs(&self.graph);
        let const_width = srcs.len();

        let mut subdiffs = SmallVec::new();
        let result = self
            .graph
            .on_region(&region, |g| {
                let index_diff = g.insert_node(OptNode::new(
                    UniformConstruct::new().with_inputs(const_width),
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

        Ok(AdResponse::new(node.output(0), result.output(0)).with_chained_derivatives(subdiffs))
    }

    fn build_diff_unary_arith(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<AdResponse, AutoDiffError> {
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
                                [src],
                            )
                            .unwrap();

                        let (f_div_abs_div, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Div), span.clone()),
                                [src, abs_f.output(0)],
                            )
                            .unwrap();

                        let (mul, _) = g
                            .connect_node(
                                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), span.clone()),
                                [f_div_abs_div.output(0)],
                            )
                            .unwrap();

                        let subdiff_dst = mul.input(1);

                        (subdiff_dst, mul.output(0))
                    })
                    .unwrap();

                let subdiff = smallvec![(src, smallvec![subdiff_dst])];
                Ok(AdResponse::new(node.output(0), output).with_chained_derivatives(subdiff))
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
                                [negone],
                            )
                            .unwrap();
                        let diff_dst = mul.input(1);

                        (diff_dst, mul)
                    })
                    .unwrap();

                let subdiff = smallvec![(f_src, smallvec![diff_dst])];
                Ok(AdResponse::new(node.output(0), result.output(0))
                    .with_chained_derivatives(subdiff))
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

                Ok(AdResponse::new(node.output(0), zero))
            }
        }
    }

    fn build_diff_binary_logic(
        &mut self,
        _region: RegionLocation,
        node: NodeRef,
    ) -> Result<AdResponse, AutoDiffError> {
        Ok(AdResponse::new(node.output(0), node.output(0)))
        //Err(AutoDiffError::NoAdImpl(self.graph[node].name().to_string()))
    }

    fn build_diff_unary_logic(
        &mut self,
        _region: RegionLocation,
        node: NodeRef,
    ) -> Result<AdResponse, AutoDiffError> {
        Ok(AdResponse::new(node.output(0), node.output(0)))
        //Err(AutoDiffError::NoAdImpl(self.graph[node].name().to_string()))
    }

    fn build_diff_binary_rel(
        &mut self,
        _region: RegionLocation,
        node: NodeRef,
    ) -> Result<AdResponse, AutoDiffError> {
        #[cfg(feature = "log")]
        log::warn!("TODO: implement boolean differential calculus");

        Ok(AdResponse::new(node.output(0), node.output(0)))
        //Err(AutoDiffError::NoAdImpl(self.graph[node].name().to_string()))
    }

    fn build_diff_unary_matrix(
        &mut self,
        _region: RegionLocation,
        node: NodeRef,
    ) -> Result<AdResponse, AutoDiffError> {
        Err(AutoDiffError::NoAdImpl(self.graph[node].name().to_string()))
    }

    fn build_diff_trig(
        &mut self,
        region: RegionLocation,
        node: NodeRef,
    ) -> Result<AdResponse, AutoDiffError> {
        let span = self.find_span(node.into()).unwrap_or(Span::empty());
        let src = self.graph.inport_src(node.input(0)).unwrap();
        let trigop = self.graph[node]
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<Trig>()
            .unwrap()
            .op
            .clone();
        match trigop {
            TrigOp::Sin => {
                //sin' = cos
                let cos_output = self
                    .graph
                    .on_region(&region, |g| {
                        let (f_diff, _) = g
                            .connect_node(OptNode::new(Trig::new(TrigOp::Cos), span), [src])
                            .unwrap();
                        f_diff.output(0)
                    })
                    .unwrap();

                Ok(self.build_chain_rule_for(&region, node.output(0), cos_output, src))
            }
            TrigOp::Cos => {
                //cos' = -sin
                let diffout = self
                    .graph
                    .on_region(&region, |g| {
                        let (sinout, _) = g
                            .connect_node(OptNode::new(Trig::new(TrigOp::Sin), span.clone()), [src])
                            .unwrap();
                        let (negged, _) = g
                            .connect_node(
                                OptNode::new(UnaryArith::new(UnaryArithOp::Neg), span),
                                [sinout.output(0)],
                            )
                            .unwrap();
                        negged.output(0)
                    })
                    .unwrap();
                Ok(self.build_chain_rule_for(&region, node.output(0), diffout, src))
            }
            TrigOp::Tan => {
                //See https://math.stackexchange.com/questions/1108131/what-is-cos%C2%B2x
                todo!("{trigop:?} not implemented")
            }
            _ => todo!("{trigop:?} not implemented"),
        }
    }

    //Small helper that makes chaining derivatives easier (building the chain rule).
    //Basically for a given f(g(x)) you supply `f'(g(x))` (`diffed_output`), and tell it what the source of `g(x)` is (active_sub_src). It'll return the outport of `f'(g'(x)) * g'(x)` where `g'(x)` is enqued in the deferred list.
    fn build_chain_rule_for(
        &mut self,
        region: &RegionLocation,
        //The original value of f,
        f: OutportLocation,
        //f'(g(x))
        diffed_output: OutportLocation,
        //g(x) where we'll need g'(x) for
        active_sub_src: OutportLocation,
    ) -> AdResponse {
        let mut subdiff = SmallColl::new();
        let mul_out = self
            .graph
            .on_region(region, |g| {
                let (mul, _edg) = g
                    .connect_node(
                        OptNode::new(BinaryArith::new(BinaryArithOp::Mul), Span::empty()),
                        [diffed_output],
                    )
                    .unwrap();

                //enque
                subdiff.push((active_sub_src, smallvec![mul.input(1)]));
                mul.output(0)
            })
            .unwrap();

        AdResponse::new(f, mul_out).with_chained_derivatives(subdiff)
    }
}
