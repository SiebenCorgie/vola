/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use rvsdg::{
    edge::{OutportLocation, OutputType},
    region::Input,
    smallvec::smallvec,
    SmallColl,
};
use vola_ast::alge::{AlgeExpr, AlgeExprTy};
use vola_common::{ariadne::Label, error::error_reporter, report};

use crate::{
    alge::{CallOp, ConstantIndex, Construct, EvalNode, ImmNat, ImmScalar, WkOp},
    common::FnImport,
    OptError, OptNode, TypeState,
};

use super::BlockBuilder;

impl<'a> BlockBuilder<'a> {
    //Sets up the alge expression, and if successful, return the output port that defines the final value.
    // TODO: Right now we don't have anything stateful, so a simple Outport location is enough. Hover, this might change whenever we introduce
    // buffer and image reads at a later stage.
    pub fn setup_alge_expr(&mut self, expr: AlgeExpr) -> Result<OutportLocation, OptError> {
        let expr_span = expr.op_span();
        match expr.expr_ty {
            AlgeExprTy::Unary { op, operand } => {
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
            AlgeExprTy::Binary { left, right, op } => {
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
            AlgeExprTy::Call(c) => {
                //For the call node we try to parse the well known ops.
                //
                //If that doesn't work, we lookup the parsed functions.
                //if one of those has the identifier, import it as cv and
                //call that instead.

                //Build the call node with its sub args
                let mut subargs: SmallColl<OutportLocation> = SmallColl::new();
                for arg in c.args {
                    let arg = self.setup_alge_expr(arg)?;
                    subargs.push(arg);
                }
                if let Some(wknode) = WkOp::try_parse(&c.ident.0) {
                    let opnode = self
                        .opt
                        .graph
                        .on_region(&self.region, |regbuilder| {
                            let (opnode, _) = regbuilder
                                .connect_node(
                                    OptNode::new(CallOp::new(wknode), expr_span),
                                    &subargs,
                                )
                                .unwrap();
                            opnode.output(0)
                        })
                        .unwrap();

                    Ok(opnode)
                } else {
                    if let Some(algefn) = self.opt.alge_fn.get(&c.ident.0) {
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
                            text: format!("\"{}\" does not name a build-in function, or declared algebraic function in this scope!", c.ident.0),
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
            AlgeExprTy::EvalExpr(evalexpr) => {
                if !self.is_eval_allowed {
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
            AlgeExprTy::FieldAccess { src, accessors } => {
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
            AlgeExprTy::Ident(i) => {
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
            AlgeExprTy::List(lst) => {
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
            AlgeExprTy::Literal(lit) => {
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
        }
    }
}
