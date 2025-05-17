/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    edge::{InputType, OutportLocation, OutputType},
    region::RegionLocation,
};

use crate::{
    csg::CsgOp,
    imm::ImmBool,
    passes::lower_ast::block_build::VarDef,
    typelevel::{NonUniformConstruct, TypeCast},
    OptEdge, Optimizer,
};

use super::block_build::BlockCtx;

use rvsdg::{region::Input, smallvec::smallvec, SmallColl};
use vola_ast::{
    alge::{Expr, ExprTy},
    common::Branch,
    csg::CsgTy,
};
use vola_common::{Span, VolaError};

use crate::{
    alge::EvalNode,
    imm::{ImmNat, ImmScalar},
    typelevel::{ConstantIndex, UniformConstruct},
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
    ) -> Result<OutportLocation, VolaError<OptError>> {
        let expr_span = expr.op_span();
        match expr.expr_ty {
            ExprTy::Unary { op, operand } => {
                //setup the unary node, than recurse, setup the subexpression and hook it up to our unary expression
                let sub_output = self.build_expr(*operand, region, ctx)?;
                let opnode = self
                    .graph
                    .on_region(&region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(OptNode::from(op).with_span(expr_span), [sub_output])
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
                                [left_out, right_out],
                            )
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            ExprTy::Call(c) => {
                //for calls we frist try to build a entity or csg-operation, if the name exist for one of those.
                //If not, we build a unresolved call
                enum CallResult {
                    Node(OptNode),
                    Call {
                        import: OutportLocation,
                        lambda: OutportLocation,
                    },
                }
                let args: SmallColl<_> = c
                    .args
                    .into_iter()
                    .map(|arg| self.build_expr(arg, region, ctx))
                    .collect::<Result<SmallColl<_>, VolaError<OptError>>>()?;

                let res = if let Some(ooe) = self.csg_node_defs.get(&c.ident.0) {
                    //found one, use that as a CSG op
                    let node =
                        OptNode::new(CsgOp::new(ooe.name.clone(), 0, args.len()), c.span.clone());
                    CallResult::Node(node)
                } else {
                    //Either a building _special_ call, or an actual call to a λ.
                    //The latter is resolved as a simple _call_
                    if let Some(intr) = OptNode::try_parse(&c.ident.0) {
                        CallResult::Node(intr)
                    } else {
                        //must be some kind of function, try to import it, and place a call
                        let call_output = ctx
                            .find_variable(&mut self.graph, &c.ident.0)
                            .map_err(|e| e.with_label(expr_span.clone(), "here"))?;
                        //make sure this is actually a callable
                        if let Some(callable) = self.graph.find_callabel_def(call_output) {
                            CallResult::Call {
                                import: call_output,
                                lambda: callable,
                            }
                        } else {
                            let err = VolaError::error_here(
                                OptError::Any {
                                    text: format!(
                                        "Could not find a function '{}' in scope",
                                        &c.ident.0
                                    ),
                                },
                                expr_span,
                                "trying to call this",
                            );
                            if let Some(outspan) = self.find_span(call_output.into()) {
                                return Err(err.with_label(outspan, "found this"));
                            } else {
                                return Err(err);
                            }
                        }
                    }
                };

                let call_result = match res {
                    //Serialize as simple node
                    CallResult::Node(n) => {
                        //check that the arg-count matches
                        if n.node.inputs().len() != args.len() {
                            let err = VolaError::error_here(
                                OptError::Any {
                                    text: format!(
                                        "{} expects {} arguments, got {}",
                                        n.node.name(),
                                        n.node.inputs().len(),
                                        args.len()
                                    ),
                                },
                                expr_span,
                                "here",
                            );
                            return Err(err);
                        }

                        self.graph
                            .on_region(&region, |reg| {
                                let (result, _) = reg.connect_node(n, args).unwrap();
                                result.output(0)
                            })
                            .unwrap()
                    }
                    //As apply node
                    CallResult::Call { import, lambda } => {
                        //check that the arg-count matches
                        let argcount = self.graph[lambda.node]
                            .node_type
                            .unwrap_lambda_ref()
                            .argument_count();
                        if argcount != args.len() {
                            let lmd_name = self.node_name(lambda.node);
                            let lmd_span = self.find_span(lambda.node.into());
                            let err = VolaError::error_here(
                                OptError::Any {
                                    text: format!(
                                        "{} expects {} arguments, got {}",
                                        lmd_name,
                                        argcount,
                                        args.len()
                                    ),
                                },
                                expr_span,
                                "here",
                            );
                            if let Some(lmdspan) = lmd_span {
                                return Err(err.with_label(lmdspan, "function defined here"));
                            } else {
                                return Err(err);
                            }
                        }

                        self.graph
                            .on_region(&region, |reg| {
                                let (call, _edges) = reg.call(import, &args).unwrap();
                                assert!(
                                    reg.ctx()[call].outputs().len() == 1,
                                    "No multi, or none-result functions supported atm."
                                );
                                call.output(0)
                            })
                            .unwrap()
                    }
                };

                Ok(call_result)
            }
            ExprTy::Eval(evalexpr) => {
                //for eval, hookup the operand, then all arguments, to a unresolved eval node

                let mut args = SmallColl::new();
                //first arg is, by definition the _hopefull_ defined var
                let operand = ctx
                    .find_variable(&mut self.graph, &evalexpr.evaluator.0)
                    .map_err(|e| e.with_label(expr_span.clone(), "here"))?;
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
                                    EvalNode::new(args.len(), evalexpr.concept.0.clone()),
                                    expr_span,
                                ),
                                args,
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

                let mut src = ctx
                    .find_variable(&mut self.graph, &src.0)
                    .map_err(|e| e.with_label(expr_span.clone(), "here"))?;

                //Unwrap the `accessors` list into a chain of `ConstantIndex`, each feeding into its
                //successor.
                for accessor in accessors {
                    let idx = if let Some(idx) = accessor.try_to_index() {
                        idx
                    } else {
                        let err = OptError::Any {
                            text: format!("Could not convert {} to index!", accessor),
                        };
                        return Err(VolaError::error_here(err, expr_span, "here"));
                    };

                    let cinode = OptNode::new(ConstantIndex::new(idx), expr_span.clone());
                    //now connect it to the predecessor.
                    let new_src = self
                        .graph
                        .on_region(&region, |reg| {
                            let (opnode, _) = reg.connect_node(cinode, [src.clone()]).unwrap();
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
                let ident_node = ctx
                    .find_variable(&mut self.graph, &i.0)
                    .map_err(|e| e.with_label(expr_span.clone(), "here"))?;
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

                let mut node = UniformConstruct::new();
                node.inputs = smallvec![Input::default(); items.len()];

                let opnode = self
                    .graph
                    .on_region(&region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(OptNode::new(node, expr_span), items)
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
                    vola_ast::common::Literal::BoolLiteral(b) => {
                        OptNode::new(ImmBool::new(b), expr_span)
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
                let first_span = tuple_elements.first().map(|f| f.span.clone()).unwrap();
                let last_span = tuple_elements.last().map(|f| f.span.clone()).unwrap();
                let span = Span {
                    file: first_span.file.clone(),
                    from: first_span.from,
                    to: last_span.to,
                    byte_start: first_span.byte_start,
                    byte_end: last_span.byte_end,
                };
                let mut args = SmallColl::default();
                for arg in tuple_elements.into_iter() {
                    args.push(self.build_expr(arg, region, ctx)?);
                }

                //For tuples we just emit a NonUniform constructor
                let tuple_construct = self
                    .graph
                    .on_region(&region, |reg| {
                        let (node, _edg) = reg
                            .connect_node(
                                OptNode::new(NonUniformConstruct::new(args.len()), span),
                                args,
                            )
                            .unwrap();
                        node.output(0)
                    })
                    .unwrap();

                Ok(tuple_construct)
            }
            ExprTy::ScopedCall(sc) => {
                if let Some(operation) = self.csg_node_defs.get(&sc.call.ident.0) {
                    if operation.ty == CsgTy::Entity {
                        let err = OptError::Any { text: format!("Using Entity with sub-trees. Only CSG-Operations can use subtrees!") };
                        return Err(VolaError::error_here(err, sc.call.span, "called here")
                            .with_label(operation.span.clone(), "Entity defined here"));
                    }
                } else {
                    //No such entity or operation
                    let err = OptError::Any {
                        text: format!(
                            "No CSG-Entity or CSG-Operation named \"{}\" found",
                            sc.call.ident.0
                        ),
                    };
                    return Err(VolaError::error_here(
                        err,
                        sc.call.span.clone(),
                        "called here",
                    ));
                }

                //setup the subtree-results
                let mut csg_node_args = SmallColl::new();
                let block_count = sc.blocks.len();
                for subtree in sc.blocks {
                    //open new scope, serialize the bloch, then read out the return value.
                    //if there is no return value, bail

                    //NOTE: we stay in the same region, only open a AST scope.
                    //      CSG-Scopes can only caputer values, but not modify them, so this is false
                    ctx.open_new_scope(region, false);
                    //build the block
                    let subtree_span = subtree.span.clone();
                    self.build_block(region, subtree, ctx)?;
                    let after_finish_scope = ctx.close_scope();
                    //now try to feed the result back
                    if let Some(result) = after_finish_scope.result {
                        csg_node_args.push(result);
                    } else {
                        //No such entity or operation
                        let err = OptError::Any {
                            text: format!("The call's subtree must have a (csg) result"),
                        };
                        return Err(VolaError::error_here(
                            err,
                            sc.call.span.clone(),
                            "called here",
                        )
                        .with_label(subtree_span.clone(), "This region"));
                    }
                }

                //now append all regular arguments
                let argcount = sc.call.args.len();
                for arg in sc.call.args {
                    let expr = self.build_expr(arg, region, ctx)?;
                    csg_node_args.push(expr);
                }
                let n = OptNode::new(
                    CsgOp::new(sc.call.ident.clone(), block_count, argcount + block_count),
                    sc.span,
                );
                //finally setup the CSGOp
                let output = self
                    .graph
                    .on_region(&region, |reg| {
                        let (usolev, _) = reg.connect_node(n, csg_node_args).unwrap();
                        usolev.output(0)
                    })
                    .unwrap();
                Ok(output)
            }
            ExprTy::Splat { expr, count } => {
                //The splat expression is a shortcut for
                //building vectors / matrixes etc from elements.
                //So thats exactly what we are emitting as well.

                if count < 2 {
                    let err = OptError::Any {
                        text: format!("Splat expression's count cannot be less than 2"),
                    };
                    return Err(VolaError::error_here(
                        err,
                        expr.span.clone(),
                        "Consider removing this splat expression",
                    ));
                }

                let src_expr = self.build_expr(*expr, region, ctx)?;
                let mut items: SmallColl<OutportLocation> = SmallColl::new();
                for _ in 0..count {
                    items.push(src_expr);
                }

                let mut node = UniformConstruct::new();
                node.inputs = smallvec![Input::default(); items.len()];

                let opnode = self
                    .graph
                    .on_region(&region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(OptNode::new(node, expr_span), items)
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            ExprTy::Cast { span: _, expr, ty } => {
                let src = self.build_expr(*expr, region, ctx)?;
                let node = TypeCast::new(ty.into());
                let opnode = self
                    .graph
                    .on_region(&region, |reg| {
                        let (opnode, _) = reg
                            .connect_node(OptNode::new(node, expr_span), [src])
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            ExprTy::Branch(e) => self.build_branch_expr(*e, region, ctx),
        }
    }

    ///Builds a branch expression that is expected to return a value
    pub fn build_branch_expr(
        &mut self,
        branch: Branch,
        region: RegionLocation,
        ctx: &mut BlockCtx,
    ) -> Result<OutportLocation, VolaError<OptError>> {
        let (condition, if_branch) = branch.conditional;
        //First build the condition in this region
        let condition = self.build_expr(condition, region, ctx)?;
        //now build the condition block.
        let (gamma, if_idx, else_idx) = self
            .graph
            .on_region(&region, |reg| {
                let (g, (if_idx, else_idx)) = reg.new_decission(|g| {
                    let (bidx_if, _) = g.new_branch(|_b, _idx| {});
                    let (bidx_else, _) = g.new_branch(|_b, _idx| {});
                    (bidx_if, bidx_else)
                });
                //connect the criterion
                let _ = reg
                    .ctx_mut()
                    .connect(
                        condition,
                        g.as_inport_location(InputType::GammaPredicate),
                        OptEdge::value_edge_unset(),
                    )
                    .unwrap();
                (g, if_idx, else_idx)
            })
            .unwrap();

        //setup the _if_ branch
        let if_region = RegionLocation {
            node: gamma,
            region_index: if_idx,
        };
        ctx.open_new_scope(
            RegionLocation {
                node: gamma,
                region_index: if_idx,
            },
            true,
        );
        self.build_block(if_region, *if_branch, ctx)?;
        let post_if_scope = ctx.close_scope();

        //After building the if-branch, build the _else_ branch. We initialize the else branch with all imported
        //context of the _if-branch_.
        let else_region = RegionLocation {
            node: gamma,
            region_index: else_idx,
        };
        ctx.open_new_scope(else_region, true);
        for (varkey, var) in post_if_scope.vars.into_iter() {
            if let VarDef::Imported {
                mut import_port,
                last_use: _,
            } = var
            {
                let _ = import_port.output.change_region_index(else_idx).unwrap();
                let in_region_insert = VarDef::Imported {
                    import_port,
                    last_use: import_port,
                };
                ctx.active_scope.vars.insert(varkey, in_region_insert);
            }
        }
        if let Some(else_block) = branch.unconditional {
            self.build_block(else_region, *else_block, ctx)?;
        }
        let post_else_scope = ctx.close_scope();
        //Since this is the _expr_ flavour of a branch, we only need to feed bach the results of both branches.

        let result_idx = self.graph[gamma]
            .node_type
            .unwrap_gamma_mut()
            .add_exit_var();

        for (region_index, result_port) in [
            (if_idx, post_if_scope.result),
            (else_idx, post_else_scope.result),
        ] {
            let result_ev = gamma.as_inport_location(InputType::ExitVariableResult {
                branch: region_index,
                exit_variable: result_idx,
            });
            if let Some(value_src) = result_port {
                //connect the result
                self.graph
                    .connect(value_src, result_ev, OptEdge::value_edge_unset())
                    .unwrap();
            } else {
                let err = OptError::Any {
                    text: format!("All branches must have an result if used as an expression!"),
                };
                return Err(VolaError::error_here(
                    err,
                    branch.span.clone(),
                    "on this expression",
                ));
            }
        }

        self.span_tags.set(gamma.into(), branch.span.clone());

        //successfully connected results, return the value_src port
        Ok(gamma.as_outport_location(OutputType::ExitVariableOutput(result_idx)))
    }
}
