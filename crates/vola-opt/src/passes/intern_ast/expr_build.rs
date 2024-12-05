/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{edge::OutportLocation, region::RegionLocation, smallvec::SmallVec};

use crate::Optimizer;

use super::{
    block_build::BlockCtx,
    unresolved::{UnresolvedCall, UnresolvedEval},
};

use rvsdg::{region::Input, smallvec::smallvec, SmallColl};
use vola_ast::{
    alge::{Expr, ExprTy},
    common::Branch,
};
use vola_common::{ariadne::Label, error::error_reporter, report};

use crate::{
    alge::{ConstantIndex, Construct, EvalNode},
    imm::{ImmNat, ImmScalar},
    OptError, OptNode,
};

impl Optimizer {
    //Sets up the alge expression, and if successful, return the output port that defines the final value.
    // TODO: Right now we don't have anything stateful, so a simple Outport location is enough. Hover, this might change whenever we introduce
    // buffer and image reads at a later stage.
    pub fn build_expr(
        &mut self,
        expr: Expr,
        region: RegionLocation,
        ctx: &mut BlockCtx,
    ) -> Result<OutportLocation, OptError> {
        let expr_span = expr.op_span();
        match expr.expr_ty {
            ExprTy::Unary { op, operand } => {
                //setup the unary node, than recurse, setup the subexpression and hook it up to our unary expression
                let sub_output = self.build_expr(*operand, region, ctx)?;
                let opnode = self
                    .graph
                    .on_region(&region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(OptNode::from(op).with_span(expr_span), &[sub_output])
                            .unwrap();
                        //NOTE we _know_ that the node has only one output
                        opnode.output(0)
                    })
                    .unwrap();

                Ok(opnode)
            }
            ExprTy::Binary { left, right, op } => {
                //Similar to the unary op, first parse both sub_trees, then hook them up to the
                // inputs and return the output.
                let left_out = self.build_expr(*left, region, ctx)?;
                let right_out = self.build_expr(*right, region, ctx)?;

                let opnode = self
                    .graph
                    .on_region(&region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(
                                OptNode::from(op).with_span(expr_span),
                                &[left_out, right_out],
                            )
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            ExprTy::Call(c) => {
                //Deffer call resolution by building all arg expression, then inserting a "UnresolvedCall"
                //node
                let args: SmallVec<_> = c
                    .args
                    .into_iter()
                    .map(|arg| self.build_expr(arg, region, ctx))
                    .collect::<Result<SmallColl<_>, OptError>>()?;

                let ucall = self
                    .graph
                    .on_region(&region, |reg| {
                        let (usolev, _) = reg
                            .connect_node(
                                OptNode::new(
                                    UnresolvedCall::new_with_args(c.ident.0.clone(), args.len()),
                                    c.span.clone(),
                                ),
                                &args,
                            )
                            .unwrap();
                        usolev.output(0)
                    })
                    .unwrap();
                Ok(ucall)
            }
            ExprTy::EvalExpr(evalexpr) => {
                //for eval, hookup the operand, then all arguments, to a unresolved eval node

                let mut args = SmallColl::new();
                //first arg is, by definition the _hopefull_ defined var
                let operand = ctx.find_variable(&evalexpr.evaluator.0)?;
                args.push(operand);

                for arg in evalexpr.params.into_iter() {
                    args.push(self.build_expr(arg, region, ctx)?);
                }

                //Now setup the eval node
                let opnode = self
                    .graph
                    .on_region(&region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(
                                OptNode::new(
                                    UnresolvedEval::new_with_args(
                                        evalexpr.concept.0.clone(),
                                        args.len(),
                                    ),
                                    expr_span,
                                ),
                                &args,
                            )
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();

                Ok(opnode)
            }
            ExprTy::FieldAccess { src, accessors } => {
                //Try to find the access source, if successful, hook the source up to this and
                // return the value

                let src = ctx.find_variable(&src.0)?;

                //Unwrap the `accessors` list into a chain of `ConstantIndex`, each feeding into its
                //successor.
                let mut src = src;
                for accessor in accessors {
                    let idx = if let Some(idx) = accessor.try_to_index() {
                        idx
                    } else {
                        let err = OptError::Any {
                            text: format!("Could not convert {} to index!", accessor),
                        };
                        report(
                            error_reporter(err.clone(), expr_span.clone())
                                .with_label(Label::new(expr_span.clone()).with_message("here"))
                                .finish(),
                        );
                        return Err(err);
                    };

                    let cinode = OptNode::new(ConstantIndex::new(idx), expr_span.clone());
                    //now connect it to the predecessor.
                    let new_src = self
                        .graph
                        .on_region(&region, |reg| {
                            let (opnode, _) = reg.connect_node(cinode, &[src.clone()]).unwrap();
                            opnode.output(0)
                        })
                        .unwrap();
                    //finally overwrite src
                    src = new_src;
                }

                //last src is the opnode with the _final_ value
                Ok(src)
            }
            ExprTy::Ident(i) => {
                //try to resolve the ident, or throw an error if not possible
                let ident_node = ctx.find_variable(&i.0)?;
                Ok(ident_node)
            }
            ExprTy::List(lst) => {
                //For now we just have a special "list" constructor that connects as many srcs as there are
                // list elements.

                //For it to not panic we first collect all items, then calculate how many
                // list items we need, resize the inputs array and then connect them

                let mut items: SmallColl<OutportLocation> = SmallColl::new();
                for itm in lst {
                    items.push(self.build_expr(itm, region, ctx)?);
                }

                let mut node = Construct::new();
                node.inputs = smallvec![Input::default(); items.len()];

                let opnode = self
                    .graph
                    .on_region(&region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(OptNode::new(node, expr_span), &items)
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            ExprTy::Literal(lit) => {
                let optnode = match lit {
                    vola_ast::common::Literal::IntegerLiteral(i) => {
                        OptNode::new(ImmNat::new(i), expr_span)
                    }
                    vola_ast::common::Literal::FloatLiteral(f) => {
                        OptNode::new(ImmScalar::new(f), expr_span)
                    }
                };

                let opnode = self
                    .graph
                    .on_region(&region, |regbuilder| {
                        let opnode = regbuilder.insert_node(optnode);
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            ExprTy::Tuple(tuple_elements) => {
                todo!("implement tuple")
            }
            ExprTy::ScopedCall(sc) => {
                let err = OptError::Any {
                    text: format!("Unexpected scoped-call in algebraic expression"),
                };
                report(
                    error_reporter(err.clone(), sc.span.clone())
                        .with_label(
                            Label::new(sc.span.clone())
                                .with_message("this patter can only be used in a csg-context"),
                        )
                        .finish(),
                );
                return Err(err);
            }
            ExprTy::SplatExpr { expr, count } => {
                //The splat expression is a shortcut for
                //building vectors / matrixes etc from elements.
                //So thats exactly what we are emitting as well.

                if count < 2 {
                    let err = OptError::Any {
                        text: format!("Splat expression's count cannot be less than 2"),
                    };
                    report(
                        error_reporter(err.clone(), expr.span.clone())
                            .with_label(
                                Label::new(expr.span.clone())
                                    .with_message("Consider removing this splat expression"),
                            )
                            .finish(),
                    );

                    return Err(err);
                }

                let src_expr = self.build_expr(*expr, region, ctx)?;
                let mut items: SmallColl<OutportLocation> = SmallColl::new();
                for _ in 0..count {
                    items.push(src_expr);
                }

                let mut node = Construct::new();
                node.inputs = smallvec![Input::default(); items.len()];

                let opnode = self
                    .graph
                    .on_region(&region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(OptNode::new(node, expr_span), &items)
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            ExprTy::BranchExpr(e) => self.build_branch_expr(*e, region, ctx),
        }
    }

    ///Builds a branch expression that is expected to return a value
    pub fn build_branch_expr(
        &mut self,
        branch: Branch,
        region: RegionLocation,
        ctx: &mut BlockCtx,
    ) -> Result<OutportLocation, OptError> {
        todo!()
    }
}
