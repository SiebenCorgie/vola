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
    builder::RegionBuilder,
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
        buildin::{Buildin, BuildinOp},
        logical::{BinaryBool, UnaryBool},
        matrix::UnaryMatrix,
        relational::BinaryRel,
        trigonometric::{Trig, TrigOp},
    },
    hook_barith, hook_buildin, hook_uarith,
    imm::ImmScalar,
    typelevel::{ConstantIndex, UniformConstruct},
    OptEdge, OptNode, Optimizer,
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
            return self.build_diff_trig(region, node, activity);
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
                        let abs_f = hook_uarith!(g, Abs, span.clone(), src);

                        let f_div_abs_div =
                            hook_barith!(g, Div, span.clone(), [src, abs_f.output(0)]);

                        let mul = hook_barith!(g, Mul, span.clone(), [f_div_abs_div.output(0)]);

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
                        let mul = hook_barith!(g, Mul, span.clone(), [negone]);
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
        activity: &mut Activity,
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
            TrigOp::ASin => {
                //asin(x)' = 1 / sqrt(1.0 - x^2)
                let diffout = self
                    .graph
                    .on_region(&region, |g| {
                        let one = g.insert_node(OptNode::new(ImmScalar::new(1.0), span.clone()));
                        let two = g.insert_node(OptNode::new(ImmScalar::new(2.0), span.clone()));
                        let powout = hook_buildin!(g, Pow, span.clone(), [src, two.output(0)]);
                        let subout =
                            hook_barith!(g, Add, span.clone(), [one.output(0), powout.output(0)]);
                        let sqrtout =
                            hook_buildin!(g, SquareRoot, span.clone(), [subout.output(0)]);
                        let div =
                            hook_barith!(g, Div, span.clone(), [one.output(0), sqrtout.output(0)]);

                        div.output(0)
                    })
                    .unwrap();

                Ok(self.build_chain_rule_for(&region, node.output(0), diffout, src))
            }
            TrigOp::ACos => {
                //acos(x)' = -(1/sqrt(1.0 - x^2))
                let diffout = self
                    .graph
                    .on_region(&region, |g| {
                        let one = g.insert_node(OptNode::new(ImmScalar::new(1.0), span.clone()));
                        let two = g.insert_node(OptNode::new(ImmScalar::new(2.0), span.clone()));
                        let powout = hook_buildin!(g, Pow, span.clone(), [src, two.output(0)]);
                        let subout =
                            hook_barith!(g, Add, span.clone(), [one.output(0), powout.output(0)]);
                        let sqrtout =
                            hook_buildin!(g, SquareRoot, span.clone(), [subout.output(0)]);
                        let div =
                            hook_barith!(g, Div, span.clone(), [one.output(0), sqrtout.output(0)]);
                        let neg = hook_uarith!(g, Neg, span.clone(), div.output(0));
                        neg.output(0)
                    })
                    .unwrap();

                Ok(self.build_chain_rule_for(&region, node.output(0), diffout, src))
            }
            TrigOp::ATan => {
                //atan(x)' = 1/(x^2 + 1)
                let diffout = self
                    .graph
                    .on_region(&region, |g| {
                        let one = g.insert_node(OptNode::new(ImmScalar::new(1.0), span.clone()));
                        let two = g.insert_node(OptNode::new(ImmScalar::new(2.0), span.clone()));
                        let powout = hook_buildin!(g, Pow, span.clone(), [src, two.output(0)]);
                        let addout =
                            hook_barith!(g, Add, span.clone(), [powout.output(0), two.output(0)]);

                        let div =
                            hook_barith!(g, Div, span.clone(), [one.output(0), addout.output(0)]);
                        div.output(0)
                    })
                    .unwrap();

                Ok(self.build_chain_rule_for(&region, node.output(0), diffout, src))
            }
            TrigOp::ATan2 => {
                //See https://de.wikipedia.org/wiki/Arctan2#Ableitungen (german)
                //
                // Depending on the activeness, user either dx, dy version, or the total differential.

                //Left (x)
                let left_src = self.graph.inport_src(node.input(0)).unwrap();
                let right_src = self.graph.inport_src(node.input(1)).unwrap();
                //Right (y)
                let left_active = activity.is_active_port(self, left_src);
                let right_active = activity.is_active_port(self, right_src);

                let original_span = self.find_span(node.into()).unwrap_or(Span::empty());
                //Builds d-left i.e. dx into the region, returns the result port
                // (-y) / (x*x + y*y)
                let build_d_left = |region_builder: &mut RegionBuilder<OptNode, OptEdge>| {
                    //add y to subdiff list

                    let negy = hook_uarith!(region_builder, Neg, original_span.clone(), right_src);

                    //NOTE: inputs will be connected by the post-derivative routine.
                    let xsquare = hook_barith!(
                        region_builder,
                        Mul,
                        original_span.clone(),
                        [left_src, left_src]
                    );

                    //Notify that both x-es need to be subderived
                    //subdiff.push((left_src, smallvec![xsquare.input(0), xsquare.input(1)]));

                    let ysquare = hook_barith!(
                        region_builder,
                        Mul,
                        original_span.clone(),
                        [right_src, right_src]
                    );
                    //(x*x + y*y)
                    let add = hook_barith!(
                        region_builder,
                        Add,
                        original_span.clone(),
                        [xsquare.output(0), ysquare.output(0)]
                    );
                    let result = hook_barith!(
                        region_builder,
                        Div,
                        original_span.clone(),
                        [negy.output(0), add.output(0)]
                    );

                    result.output(0)
                };

                //Builds d-right i.e. dy into the region, returns the result port
                // x / (x*x + y*y)
                let build_d_right = |region_builder: &mut RegionBuilder<OptNode, OptEdge>| {
                    let xsquare = hook_barith!(
                        region_builder,
                        Mul,
                        original_span.clone(),
                        [left_src, left_src]
                    );
                    //NOTE inputs will be connected by the post-derivative routine.

                    let ysquare = hook_barith!(
                        region_builder,
                        Mul,
                        original_span.clone(),
                        [right_src, right_src]
                    );
                    //add y to subdiff list, i.e. build derivative for both y-values
                    //subdiff.push((right_src, smallvec![ysquare.input(0), ysquare.input(1)]));

                    //(x*x + y*y)
                    let add = hook_barith!(
                        region_builder,
                        Add,
                        original_span.clone(),
                        [xsquare.output(0), ysquare.output(0)]
                    );
                    let result = hook_barith!(
                        region_builder,
                        Div,
                        original_span.clone(),
                        [left_src, add.output(0)]
                    );
                    result.output(0)
                };

                match (left_active, right_active) {
                    (false, false) => unreachable!(),
                    (true, false) => {
                        let dx = self.graph.on_region(&region, build_d_left).unwrap();

                        Ok(self.build_chain_rule_for(&region, node.output(0), dx, left_src))
                    }
                    (false, true) => {
                        let dy = self.graph.on_region(&region, build_d_right).unwrap();

                        Ok(self.build_chain_rule_for(&region, node.output(0), dy, right_src))
                    }
                    (true, true) => {
                        unimplemented!("Total Atan2 derivative not implemented ")
                    }
                }
            }
            _ => todo!("differentiation of {trigop:?} not implemented"),
        }
    }

    ///Small helper that makes chaining derivatives easier (building the chain rule).
    ///Basically for a given f(g(x)) you supply `f'(g(x))` (`diffed_output`), and tell it what the source of `g(x)` is (active_sub_src). It'll return the outport of `f'(g'(x)) * g'(x)` where `g'(x)` is enqued in the deferred list.
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
