use ahash::AHashMap;
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use rvsdg::{
    edge::{InportLocation, InputType, LangEdge, OutportLocation, OutputType},
    region::{Input, RegionLocation},
    smallvec::smallvec,
    NodeRef, SmallColl,
};
use vola_ast::{
    alge::{Expr, ExprTy},
    common::{Block, Call, GammaExpr, ThetaExpr},
};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{
    alge::{CallOp, ConstantIndex, Construct, EvalNode, ImmNat, ImmScalar, WkOp},
    ast::block_builder::BlockBuilderConfig,
    common::{FnImport, LmdContext, VarDef},
    OptEdge, OptError, OptNode, TypeState,
};

use super::BlockBuilder;

impl<'a> BlockBuilder<'a> {
    //Sets up the alge expression, and if successful, return the output port that defines the final value.
    // TODO: Right now we don't have anything stateful, so a simple Outport location is enough. Hover, this might change whenever we introduce
    // buffer and image reads at a later stage.
    pub fn setup_alge_expr(&mut self, expr: Expr) -> Result<OutportLocation, OptError> {
        let expr_span = expr.op_span();
        match expr.expr_ty {
            ExprTy::Unary { op, operand } => {
                //setup the unary node, than recurse, setup the subexpression and hook it up to our unary expression
                let sub_output = self.setup_alge_expr(*operand)?;
                let opnode = self
                    .opt
                    .graph
                    .on_region(&self.region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(
                                OptNode::new(CallOp::new(op.into()), expr_span),
                                &[sub_output],
                            )
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
                let left_out = self.setup_alge_expr(*left)?;
                let right_out = self.setup_alge_expr(*right)?;

                let opnode = self
                    .opt
                    .graph
                    .on_region(&self.region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(
                                OptNode::new(CallOp::new(op.into()), expr_span),
                                &[left_out, right_out],
                            )
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            ExprTy::Call(c) => self.setup_call_expr(*c, expr_span),
            ExprTy::EvalExpr(evalexpr) => {
                if !self.config.allow_eval {
                    let err = OptError::Any {
                        text: "Eval expressions are only allowed in impl-blocks of operations!"
                            .to_owned(),
                    };
                    report(
                        error_reporter(err.clone(), evalexpr.span.clone())
                            .with_label(
                                Label::new(evalexpr.span.clone())
                                    .with_message("Consider removing this eval"),
                            )
                            .finish(),
                    );
                    return Err(err);
                }

                //For the eval expression, first find / insert the cv_import we need.
                // then setup all arguments, and finally add the call.
                let (eval_cv, eval_cv_type) = self.get_cv_for_eval(&evalexpr)?;
                let mut inputs: SmallColl<OutportLocation> = smallvec![eval_cv];

                let argount = evalexpr.params.len();
                for arg in evalexpr.params.into_iter() {
                    inputs.push(self.setup_alge_expr(arg)?);
                }

                //Now setup the eval node
                let opnode = self
                    .opt
                    .graph
                    .on_region(&self.region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(
                                OptNode::new(
                                    EvalNode::new(argount, evalexpr.concept.0.clone()),
                                    expr_span,
                                ),
                                &inputs,
                            )
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();

                //tag output type
                self.opt.typemap.set(opnode.into(), eval_cv_type);

                Ok(opnode)
            }
            ExprTy::FieldAccess { src, accessors } => {
                //Try to find the access source, if successful, hook the source up to this and
                // return the value

                let src = if let Some(access) = self.ref_var(&src.0) {
                    access.port
                } else {
                    let err = OptError::Any {
                        text: format!("could not find {} in scope", src.0),
                    };
                    report(
                        error_reporter(err.clone(), expr_span.clone())
                            .with_label(
                                Label::new(expr_span.clone())
                                    .with_message("Try to define this before using it."),
                            )
                            .finish(),
                    );
                    return Err(err);
                };

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
                        .opt
                        .graph
                        .on_region(&self.region, |reg| {
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
                let ident_node = if let Some(noderef) = self.ref_var(&i.0) {
                    noderef
                } else {
                    let err = OptError::Any {
                        text: format!("could not find {} in scope", i.0),
                    };
                    report(
                        error_reporter(err.clone(), expr_span.clone())
                            .with_label(
                                Label::new(expr_span.clone())
                                    .with_message("Try to define this before using it."),
                            )
                            .finish(),
                    );
                    return Err(err);
                };

                Ok(ident_node.port)
            }
            ExprTy::List(lst) => {
                //For now we just have a special "list" constructor that connects as many srcs as there are
                // list elements.

                //For it to not panic we first collect all items, then calculate how many
                // list items we need, resize the inputs array and then connect them

                let mut items: SmallColl<OutportLocation> = SmallColl::new();
                for itm in lst {
                    items.push(self.setup_alge_expr(itm)?);
                }

                let mut node = Construct::new();
                node.inputs = smallvec![Input::default(); items.len()];

                let opnode = self
                    .opt
                    .graph
                    .on_region(&self.region, |regbuilder| {
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
                    .opt
                    .graph
                    .on_region(&self.region, |regbuilder| {
                        let opnode = regbuilder.insert_node(optnode);
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            ExprTy::AccessExpr(ae) => {
                let err = OptError::Any {
                    text: format!("Unexpected access-expression"),
                };
                report(
                    error_reporter(err.clone(), ae.span.clone())
                        .with_label(
                            Label::new(ae.span.clone())
                                .with_message("this patter can only be used as the last statement of an export-function"),
                        )
                        .finish(),
                );
                return Err(err);
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
            ExprTy::GammaExpr(e) => self.setup_gamma_expr(*e, expr_span),
            ExprTy::ThetaExpr(t) => self.setup_theta_expr(*t, expr_span),
        }
    }

    fn setup_gamma_expr(
        &mut self,
        mut gamma: GammaExpr,
        #[allow(unused)] expr_span: Span,
    ) -> Result<OutportLocation, OptError> {
        //there are two things to take care of
        //1. If we are assigning to a already existing value, we have to
        //   build a _identity_-else-branch, which basically passes through
        //   the _old-value_ in the _else_ case.
        //
        //2. We have to build the branch mapping.
        //   This works by taking the predicate expressions of all branches _in-order_ and mapping them
        //   to a value of 1..n (n=conditionals.len()).
        //   we do that in a nested way, so the precedence is ensured.
        //   so
        //   if a < 10{
        //     1.0
        //   }else if a < 15{
        //     2.0
        //   } else {
        //     3.0
        //   }
        //   is 1.0 for a ==  9
        //   is 2.0 for a == 14
        //   is 3.0 for a >= 15
        //
        //   this'll play in fact out as a rewriting it as this:
        //   if a < 10{
        //     1.0
        //   }else{
        //     if a < 15{
        //       2.0
        //     } else{
        //       3.0
        //     }
        //   }

        //TODO: find out, if this is an assignment. If so, we could infer the else-
        //      branch by passing through the old value.

        //In practice we make our live easy, by factoring out by popping of the first
        //conditional, and then recursing on the rest.
        //
        // only edge case is, that there is only one conditional, and one _else_,
        // in that case we use those.
        //
        // if without else is currently not supported, see the TODO above.

        if gamma.unconditional.is_none() {
            let err = OptError::Any {
                text: format!("γ-Expression without an \"else\" branch are not (yet) allowed"),
            };
            report(
                error_reporter(err.clone(), gamma.span.clone())
                    .with_label(
                        Label::new(gamma.span.clone())
                            .with_message("here"),
                    ).with_help("Consider adding a else branch, that just returns already existing variable, or a constant!")
                    .finish(),
            );
            return Err(err);
        }

        assert!(gamma.conditionals.len() >= 1);
        //initiate the recursion
        let (condition, ifblock) = gamma.conditionals.remove(0);
        self.gamma_splitoff(condition, ifblock, gamma)
    }

    fn gamma_splitoff(
        &mut self,
        condition: Expr,
        if_block: Block,
        mut rest_gamma: GammaExpr,
    ) -> Result<OutportLocation, OptError> {
        //build the gamma node, then enter both branches. For the if-block just emit the block into that region,
        //for the else block, recurse the gamma_splitoff if needed, or
        //emit the else branch, if only that is left.
        let gamma_node = self
            .opt
            .graph
            .on_region(&self.region, |reg| {
                let (gamma_node, _) = reg.new_decission(|gb| {
                    //always add 1 output to the gamma
                    let (_, _outport) = gb.add_exit_variable();

                    //now add both branches
                    let (idx0, _) = gb.new_branch(|_, _| {});
                    let (idx1, _) = gb.new_branch(|_, _| {});

                    assert!(idx0 == 0);
                    assert!(idx1 == 1);
                });

                gamma_node
            })
            .unwrap();

        let condition_span = condition.span.clone();
        //allrighty, lets build the condition and hook that up.
        let condition = self.setup_alge_expr(condition)?;
        self.opt
            .graph
            .connect(
                condition,
                InportLocation {
                    node: gamma_node,
                    input: InputType::GammaPredicate,
                },
                OptEdge::value_edge(),
            )
            .unwrap();

        //Tag the condition port and the node with a nice span
        self.opt
            .span_tags
            .set(condition.into(), condition_span.clone());
        self.opt
            .span_tags
            .set(condition.node.into(), condition_span.clone());

        let if_lmd_ctx = self.lmd_ctx.clone();
        let mut if_block_builder = BlockBuilder {
            span: if_block.span.clone(),
            config: self.config.clone(),
            csg_operands: self.csg_operands.clone(),
            return_type: self.return_type.clone(),
            lmd_ctx: if_lmd_ctx,
            region: RegionLocation {
                node: gamma_node,
                region_index: 0,
            },
            opt: &mut self.opt,
        };
        let result_port = if_block_builder.build_block(if_block)?;
        assert!(result_port.len() == 1);
        //now hook up the result of the block to the region's exit variable
        self.opt
            .graph
            .connect(
                result_port[0].1,
                InportLocation {
                    node: gamma_node,
                    input: InputType::ExitVariableResult {
                        branch: 0,
                        exit_variable: 0,
                    },
                },
                OptEdge::value_edge(),
            )
            .unwrap();

        //for the else branch we have two possibilities. One is, that there is still an
        // if-else and else branch left, in that case, just recures in the gamma-region-1
        //
        // the second case is, that only the else branch is left, in that case,
        // append the else block the same way we did with the if branch.
        //
        // either way, setup the new block builder the same way we did for the if-block

        let else_lmd_ctx = self.lmd_ctx.clone();
        let mut else_block_builder = BlockBuilder {
            span: self.span.clone(), //FIXME: not really the correct span, but have no better one atm :/
            config: self.config.clone(),
            csg_operands: self.csg_operands.clone(),
            return_type: self.return_type.clone(),
            lmd_ctx: else_lmd_ctx,
            region: RegionLocation {
                node: gamma_node,
                region_index: 1,
            },
            opt: &mut self.opt,
        };

        if rest_gamma.conditionals.len() > 0 {
            //recurse with the else block
            let (conditional, new_if_block) = rest_gamma.conditionals.remove(0);

            let result_port =
                else_block_builder.gamma_splitoff(conditional, new_if_block, rest_gamma)?;
            //connect result to outport
            self.opt
                .graph
                .connect(
                    result_port,
                    InportLocation {
                        node: gamma_node,
                        input: InputType::ExitVariableResult {
                            branch: 1,
                            exit_variable: 0,
                        },
                    },
                    OptEdge::value_edge(),
                )
                .unwrap();
        } else {
            //this is the case, that we don't have any more if-conditions, in that case just append
            let result_port =
                else_block_builder.build_block(rest_gamma.unconditional.take().unwrap())?;
            assert!(result_port.len() == 1);
            self.opt
                .graph
                .connect(
                    result_port[0].1,
                    InportLocation {
                        node: gamma_node,
                        input: InputType::ExitVariableResult {
                            branch: 1,
                            exit_variable: 0,
                        },
                    },
                    OptEdge::value_edge(),
                )
                .unwrap();
        }

        //tag the gamma with a span
        self.opt.span_tags.set(gamma_node.into(), self.span.clone());

        //finally return the
        Ok(OutportLocation {
            node: gamma_node,
            output: OutputType::ExitVariableOutput(0),
        })
    }

    fn setup_theta_expr(
        &mut self,
        theta: ThetaExpr,
        expr_span: Span,
    ) -> Result<OutportLocation, OptError> {
        //Conceptually we build a gamma node, that wraps the theta node and checks the
        //head exprssion. This is needed, since the RVSDG defines theta nodes as tail controlled.
        //makes sense, since it always needs to produces _at least something_, so needs to run once.
        //
        //So the _ran 0-time_ case is done via the gamma-node.

        //first things first, setup the initial value of the loop, as well as the two bounds.

        struct PortPackage {
            //The arugmentport in the theta node, that routes the _last_ value of
            //the loop-value
            lv_arg_value: OutportLocation,
            //the loop_-value result port _after executing the loop_
            lv_res_value: InportLocation,

            g_initial_input_port: InportLocation,
            g_inp_bound_lower: InportLocation,
            g_inp_bound_higher: InportLocation,
            g_result_value_out_port: OutportLocation,
        }

        let initial_assignment_src = self.setup_alge_expr(theta.initial_assignment.expr)?;

        let bound_lower = self.setup_alge_expr(theta.bound_lower)?;
        let bound_upper = self.setup_alge_expr(theta.bound_upper)?;

        //we use the implicit LessThan for checks... always :D
        let initial_lt = self
            .opt
            .graph
            .on_region(&self.region, |reg| {
                let (node, _edges) = reg
                    .connect_node(
                        OptNode {
                            span: Span::empty(),
                            node: Box::new(CallOp::new(WkOp::Lt)),
                        },
                        &[bound_lower, bound_upper],
                    )
                    .unwrap();
                node
            })
            .unwrap();

        let (gamma_node, (loop_node, ports)) =
            self.opt
                .graph
                .on_region(&self.region, |reg| {
                    let (gamma_node, (loop_node, port_package)) = reg.new_decission(|gb| {
                        let (g_inp_bound_lower, lower_bound_idx) = gb.add_entry_variable();
                        let g_arg_bound_lower = OutportLocation {
                            node: g_inp_bound_lower.node,
                            output: g_inp_bound_lower.input.map_to_in_region(0).unwrap(),
                        };
                        let (g_inp_bound_higher, upper_bound_idx) = gb.add_entry_variable();
                        let g_arg_bound_higher = OutportLocation {
                            node: g_inp_bound_higher.node,
                            output: g_inp_bound_higher.input.map_to_in_region(0).unwrap(),
                        };
                        let (g_initial_input_port, _) = gb.add_entry_variable();
                        let g_initial_arg_port = OutportLocation {
                            node: g_initial_input_port.node,
                            output: g_initial_input_port.input.map_to_in_region(0).unwrap(),
                        };
                        let (_, g_result_value_out_port) = gb.add_exit_variable();
                        let g_result_value_res_port = InportLocation {
                            node: g_result_value_out_port.node,
                            input: g_result_value_out_port.output.map_to_in_region(0).unwrap(),
                        };

                        //This i an assumption the type-derive pass uses later on
                        assert!(lower_bound_idx == 0);
                        assert!(upper_bound_idx == 1);

                        //now add both branches, add the theta
                        //node immediatly, and use the same mapping methode, to map all variables, and the
                        let (idx0, (loop_node, (lv_arg_value, lv_res_value))) =
                            gb.new_branch(|rg, _| {
                                let (loop_node, ports) = rg.new_loop(|lb| {
                                    //NOTE: we have an informal convention, that the first
                                    //      three arguments are (lower_bound, higher_bound, loop-value)
                                    let (
                                        lv_inp_bound_lower,
                                        lv_arg_bound_lower,
                                        lv_res_bound_lower,
                                        _,
                                    ) = lb.add_loop_variable();
                                    let (
                                        lv_inp_bound_higher,
                                        lv_arg_bound_higher,
                                        lv_res_bound_higher,
                                        _,
                                    ) = lb.add_loop_variable();

                                    let (lv_inp_value, lv_arg_value, lv_res_value, lv_out_value) =
                                        lb.add_loop_variable();

                                    let _ = lb.on_loop(|rg| {
                                        //setup the control structure within the loop. This are two parts
                                        //1. the Lt controll part that is routed to the theta-predicate
                                        //2. the idx+1 part that is routed to lv_res_bound_lower
                                        //3. routing of the upper bound
                                        let (ltcheck, _) = rg
                                            .connect_node(
                                                OptNode::new(CallOp::new(WkOp::Lt), Span::empty()),
                                                &[lv_arg_bound_lower, lv_arg_bound_higher],
                                            )
                                            .unwrap();
                                        //connect the check to the predicate
                                        rg.ctx_mut()
                                            .connect(
                                                ltcheck.output(0),
                                                InportLocation {
                                                    node: lv_inp_bound_higher.node,
                                                    input: InputType::ThetaPredicate,
                                                },
                                                OptEdge::value_edge(),
                                            )
                                            .unwrap();

                                        //now build the idx + 1 and route that to the lv_res_bound_lower
                                        let litone = rg.insert_node(OptNode::new(
                                            ImmNat::new(1),
                                            Span::empty(),
                                        ));
                                        let (subone, _) = rg
                                            .connect_node(
                                                OptNode::new(CallOp::new(WkOp::Add), Span::empty()),
                                                &[lv_arg_bound_lower, litone.output(0)],
                                            )
                                            .unwrap();

                                        rg.ctx_mut()
                                            .connect(
                                                subone.output(0),
                                                lv_res_bound_lower,
                                                OptEdge::value_edge(),
                                            )
                                            .unwrap();

                                        //now route the upper bound directly
                                        rg.ctx_mut()
                                            .connect(
                                                lv_arg_bound_higher,
                                                lv_res_bound_higher,
                                                OptEdge::value_edge(),
                                            )
                                            .unwrap();

                                        //finally rout the initial value, lower and upper bound from the
                                        //gamma-node args to the lv_inputs
                                        rg.ctx_mut()
                                            .connect(
                                                g_arg_bound_lower,
                                                lv_inp_bound_lower,
                                                OptEdge::value_edge(),
                                            )
                                            .unwrap();
                                        rg.ctx_mut()
                                            .connect(
                                                g_arg_bound_higher,
                                                lv_inp_bound_higher,
                                                OptEdge::value_edge(),
                                            )
                                            .unwrap();
                                        rg.ctx_mut()
                                            .connect(
                                                g_initial_arg_port,
                                                lv_inp_value,
                                                OptEdge::value_edge(),
                                            )
                                            .unwrap();
                                        rg.ctx_mut()
                                            .connect(
                                                lv_out_value,
                                                g_result_value_res_port,
                                                OptEdge::value_edge(),
                                            )
                                            .unwrap();
                                    });
                                    (lv_arg_value, lv_res_value)
                                });
                                (loop_node, ports)
                            });
                        let (idx1, _) = gb.new_branch(|bb, _| {
                            //for the else branch, just connect the initial value to the result
                            let initial_argport = OutportLocation {
                                node: g_initial_arg_port.node,
                                output: g_initial_input_port.input.map_to_in_region(1).unwrap(),
                            };
                            let initial_res_port = InportLocation {
                                node: g_initial_arg_port.node,
                                input: g_result_value_out_port.output.map_to_in_region(1).unwrap(),
                            };
                            bb.ctx_mut()
                                .connect(initial_argport, initial_res_port, OptEdge::value_edge())
                                .unwrap();
                        });

                        assert!(idx0 == 0);
                        assert!(idx1 == 1);

                        (
                            loop_node,
                            PortPackage {
                                lv_arg_value,
                                lv_res_value,
                                g_initial_input_port,
                                g_inp_bound_lower,
                                g_inp_bound_higher,
                                g_result_value_out_port,
                            },
                        )
                    });

                    (gamma_node, (loop_node, port_package))
                })
                .unwrap();

        //do the static rounting which is
        //1. initila check to gamma-predicate
        //2. lower_bound to pass-through
        //3. upper_bound to pass-through,
        //4. initial_value to pass-through
        self.opt
            .graph
            .connect(
                initial_lt.output(0),
                InportLocation {
                    node: gamma_node,
                    input: InputType::GammaPredicate,
                },
                OptEdge::value_edge(),
            )
            .unwrap();

        self.opt
            .graph
            .connect(bound_lower, ports.g_inp_bound_lower, OptEdge::value_edge())
            .unwrap();

        self.opt
            .graph
            .connect(bound_upper, ports.g_inp_bound_higher, OptEdge::value_edge())
            .unwrap();
        self.opt
            .graph
            .connect(
                initial_assignment_src,
                ports.g_initial_input_port,
                OptEdge::value_edge(),
            )
            .unwrap();

        //now build the theta_node in the gamma-node's _if_ branch
        //we rely on the auto-import of used variables in the _setup_alge_expr_
        //routine to import any needed variables
        //add the predicate port
        let mut block = self.inherite_ctx(
            theta.span.clone(),
            BlockBuilderConfig {
                expect_return: crate::ast::block_builder::RetType::None,
                allow_eval: false,
                allow_csg_stmt: false,
            },
            self.return_type.clone(),
        );
        //move to the theta node
        let theta_region = RegionLocation {
            node: loop_node,
            region_index: 0,
        };
        block.region = theta_region;
        block.lmd_ctx.defined_vars.insert(
            theta.initial_assignment.dst.0.clone(),
            VarDef {
                port: ports.lv_arg_value,
                span: theta.initial_assignment.span.clone(),
            },
        );

        let _ret = block.build_block(theta.body)?;
        let post_block_ctx = block.destroy_inherited();
        //finally, after stopping, connect the initial_assignment var to the theta_node's result port
        let current_theta_res_port = post_block_ctx
            .defined_vars
            .get(&theta.initial_assignment.dst.0)
            .unwrap()
            .port;
        self.opt
            .graph
            .connect(
                current_theta_res_port,
                ports.lv_res_value,
                OptEdge::value_edge(),
            )
            .unwrap();

        self.route_theta_var_use(loop_node, post_block_ctx);

        //sprinkel some debug info all over the place
        self.opt.span_tags.set(gamma_node.into(), expr_span.clone());
        self.opt.span_tags.set(loop_node.into(), expr_span.clone());
        self.opt.span_tags.set(
            RegionLocation {
                node: gamma_node,
                region_index: 0,
            }
            .into(),
            expr_span.clone(),
        );
        self.opt.span_tags.set(
            RegionLocation {
                node: gamma_node,
                region_index: 1,
            }
            .into(),
            expr_span.clone(),
        );
        self.opt.span_tags.set(loop_node.into(), expr_span.clone());
        self.opt.span_tags.set(
            RegionLocation {
                node: loop_node,
                region_index: 0,
            }
            .into(),
            expr_span.clone(),
        );
        self.opt.names.set(
            gamma_node.into(),
            format!(
                "loop<{}> (first iteration head-ctrl)",
                theta.initial_assignment.dst.0
            ),
        );
        self.opt.names.set(
            loop_node.into(),
            format!("loop<{}> (tail-ctrl)", theta.initial_assignment.dst.0),
        );

        //use the gamma-nodes's exit variable as loop_exit_src
        Ok(ports.g_result_value_out_port)
    }

    fn route_theta_var_use(&mut self, theta: NodeRef, post_block_building_ctx: LmdContext) {
        //for each used variable, check if it was redefined in the context of the just build block.
        //if so, route that new definition to the loop-variable-result,
        //if not, just route the loop-variable argument to its own result.
        for lvidx in 0..self
            .opt
            .graph
            .node(theta)
            .node_type
            .unwrap_theta_ref()
            .loop_variable_count()
        {
            let src_port = OutportLocation {
                node: theta,
                output: OutputType::Argument(lvidx),
            };
            if let Some(prodname) = self.opt.var_producer.get(&src_port.into()) {
                let dst_port = InportLocation {
                    node: theta,
                    input: InputType::Result(lvidx),
                };
                if let Some(current_port) = post_block_building_ctx.defined_vars.get(prodname) {
                    //was redefined, so route to the new port
                    self.opt
                        .graph
                        .connect(current_port.port, dst_port, OptEdge::value_edge())
                        .unwrap();
                } else {
                    //was imported, but not reused, so route to the same outport
                    self.opt
                        .graph
                        .connect(src_port, dst_port, OptEdge::value_edge())
                        .unwrap();
                }
            }
        }
    }

    fn setup_call_expr(
        &mut self,
        call: Call,
        expr_span: Span,
    ) -> Result<OutportLocation, OptError> {
        //For the call node we try to parse the well known ops.
        //
        //If that doesn't work, we lookup the parsed functions.
        //if one of those has the identifier, import it as cv and
        //call that instead.

        //Build the call node with its sub args
        let mut subargs: SmallColl<OutportLocation> = SmallColl::new();
        for arg in call.args {
            let arg = self.setup_alge_expr(arg)?;
            subargs.push(arg);
        }
        if let Some(wknode) = WkOp::try_parse(&call.ident.0) {
            let opnode = self
                .opt
                .graph
                .on_region(&self.region, |regbuilder| {
                    let (opnode, _) = regbuilder
                        .connect_node(OptNode::new(CallOp::new(wknode), expr_span), &subargs)
                        .unwrap();
                    opnode.output(0)
                })
                .unwrap();

            Ok(opnode)
        } else {
            if let Some(algefn) = self.opt.alge_fn.get(&call.ident.0) {
                let alge_import =
                    if let Some(imported) = self.lmd_ctx.known_function(&algefn.name.0) {
                        imported
                    } else {
                        //import this function newly, add to context,
                        //then use it

                        let algeimportcv = self
                            .opt
                            .graph
                            .import_context(
                                algefn
                                    .lambda
                                    .as_outport_location(OutputType::LambdaDeclaration),
                                self.region,
                            )
                            .unwrap();

                        assert!(self
                            .lmd_ctx
                            .imported_functions
                            .insert(
                                algefn.name.0.clone(),
                                FnImport {
                                    port: algeimportcv,
                                    span: algefn.span.clone(),
                                    args: algefn.args.iter().map(|a| a.1.clone()).collect(),
                                    ret: algefn.retty.clone()
                                }
                            )
                            .is_none());

                        self.lmd_ctx.known_function(&algefn.name.0).unwrap()
                    };
                //import the algefn into scope, then add an apply node
                //and hookup the arguments. Bail if the argument count does not
                //fit

                //import algefn

                let applynode_output = self
                    .opt
                    .graph
                    .on_region(&self.region, |regbuilder| {
                        let (applynode, input_edges) =
                            regbuilder.call(alge_import.port.clone(), &subargs).unwrap();
                        //Set edge types already, since we know them
                        //from the imported function
                        //NOTE: that the first input is always just a callable
                        for (argidx, edg) in input_edges.iter().skip(1).enumerate() {
                            assert!(
                                regbuilder
                                    .ctx_mut()
                                    .edge_mut(*edg)
                                    .ty
                                    .set_type(alge_import.args[argidx].clone())
                                    == Some(TypeState::Unset)
                            );
                        }

                        applynode.output(0)
                    })
                    .unwrap();
                //tag the output with the result type
                self.opt
                    .typemap
                    .set(applynode_output.clone().into(), alge_import.ret.clone());
                Ok(applynode_output)
            } else {
                let err = OptError::Any {
                    text: format!("\"{}\" does not name a build-in function, or declared algebraic function in this scope!", call.ident.0),
                };
                report(
                    error_reporter(err.clone(), expr_span.clone())
                        .with_label(Label::new(expr_span.clone()).with_message("here"))
                        .finish(),
                );
                Err(err)
            }
        }
    }
}
