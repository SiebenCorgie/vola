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
    SmallColl,
};
use vola_ast::{
    alge::{Expr, ExprTy},
    common::{Block, Call, GammaExpr},
};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{
    alge::{CallOp, ConstantIndex, Construct, EvalNode, ImmNat, ImmScalar, WkOp},
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

                let src = if let Some(access) = self.lmd_ctx.defined_vars.get(&src.0) {
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
                let ident_node = if let Some(noderef) = self.lmd_ctx.defined_vars.get(&i.0) {
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
            ExprTy::ThetaExpr => todo!("implement theta expr"),
        }
    }

    fn setup_gamma_expr(
        &mut self,
        mut gamma: GammaExpr,
        expr_span: Span,
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
        let (gamma_node, inport_mapping, fn_mapping, result_port) = self
            .opt
            .graph
            .on_region(&self.region, |reg| {
                let (gamma_node, (inport_mapping, fn_mapping, result_port)) =
                    reg.new_decission(|gb| {
                        //create a entry-variable for each defined var.
                        //and add the inport to the gamma_node
                        let mut inport_mapping = AHashMap::default();
                        for (var_name, _def_port) in self.lmd_ctx.defined_vars.iter() {
                            let (port, _idx) = gb.add_entry_variable();
                            inport_mapping.insert(var_name.clone(), port);
                        }
                        let mut fn_mapping = AHashMap::default();
                        for (fn_name, _def_port) in self.lmd_ctx.imported_functions.iter() {
                            let (port, _idx) = gb.add_entry_variable();
                            fn_mapping.insert(fn_name.clone(), port);
                        }

                        //always add 1 output to the gamma
                        let (_, outport) = gb.add_exit_variable();

                        //now add both branches
                        let (idx0, _) = gb.new_branch(|_, _| {});
                        let (idx1, _) = gb.new_branch(|_, _| {});

                        assert!(idx0 == 0);
                        assert!(idx1 == 1);

                        (inport_mapping, fn_mapping, outport)
                    });

                //hook up all the variables to the entry-node ports
                for (var_name, var_entry_port) in &inport_mapping {
                    let def_port = self
                        .lmd_ctx
                        .defined_vars
                        .get(var_name)
                        .expect("Var should be defined!");
                    reg.ctx_mut()
                        .connect(def_port.port, var_entry_port.clone(), OptEdge::value_edge())
                        .expect("Expected to be connected without problems!");
                }

                (gamma_node, inport_mapping, fn_mapping, result_port)
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

        //for the if-branch, always build a new block-builder which is based on the new var-mapping
        let if_lmd_ctx = LmdContext {
            defined_vars: inport_mapping
                .iter()
                .map(|(varname, gamma_inport)| {
                    let old_var = self.lmd_ctx.defined_vars.get(varname).unwrap().clone();
                    let def = VarDef {
                        port: OutportLocation {
                            node: gamma_inport.node,
                            output: gamma_inport.input.map_to_in_region(0).unwrap(),
                        },
                        span: old_var.span,
                    };

                    //if we already know a type for the variable, for instance if the source is a
                    // typed argument, pass that through.
                    if let Some(ty) = self.opt.find_type(&old_var.port.into()) {
                        self.opt.typemap.set(def.port.clone().into(), ty);
                    }

                    (varname.clone(), def)
                })
                .collect(),
            imported_functions: fn_mapping
                .iter()
                .map(|(fnname, inport)| {
                    let old_def = self.lmd_ctx.imported_functions.get(fnname).unwrap();
                    let def = FnImport {
                        port: OutportLocation {
                            node: inport.node,
                            output: inport.input.map_to_in_region(0).unwrap(),
                        },
                        span: old_def.span.clone(),
                        args: old_def.args.clone(),
                        ret: old_def.ret.clone(),
                    };
                    (fnname.clone(), def)
                })
                .collect(),
        };

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

        let else_lmd_ctx = LmdContext {
            defined_vars: inport_mapping
                .iter()
                .map(|(varname, gamma_inport)| {
                    let old_var = self.lmd_ctx.defined_vars.get(varname).unwrap().clone();
                    let def = VarDef {
                        port: OutportLocation {
                            node: gamma_inport.node,
                            output: gamma_inport.input.map_to_in_region(1).unwrap(),
                        },
                        span: old_var.span,
                    };

                    //if we already know a type for the variable, for instance if the source is a
                    // typed argument, pass that through.
                    if let Some(ty) = self.opt.find_type(&old_var.port.into()) {
                        self.opt.typemap.set(def.port.clone().into(), ty);
                    }

                    (varname.clone(), def)
                })
                .collect(),
            imported_functions: fn_mapping
                .iter()
                .map(|(fnname, inport)| {
                    let old_def = self.lmd_ctx.imported_functions.get(fnname).unwrap();
                    let def = FnImport {
                        port: OutportLocation {
                            node: inport.node,
                            output: inport.input.map_to_in_region(1).unwrap(),
                        },
                        span: old_def.span.clone(),
                        args: old_def.args.clone(),
                        ret: old_def.ret.clone(),
                    };
                    (fnname.clone(), def)
                })
                .collect(),
        };

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
