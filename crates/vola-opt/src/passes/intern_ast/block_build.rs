/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::{
    alge::{
        arithmetic::{BinaryArith, BinaryArithOp},
        relational::{BinaryRel, BinaryRelOp},
    },
    imm::ImmNat,
    OptEdge, OptError, OptGraph, OptNode, Optimizer,
};
use ahash::AHashMap;
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    region::RegionLocation,
    smallvec::SmallVec,
    SmallColl,
};
use rvsdg_viewer::View;
use vola_ast::{
    alge::Expr,
    common::{Block, Branch, Loop, Stmt},
};
use vola_common::{ariadne::Label, error_reporter, report, warning_reporter, Span, VolaError};

#[derive(Debug, Clone)]
pub enum VarDef {
    ///Variable that was frist defined in this scope
    FirstDef {
        first_def: OutportLocation,
        last_use: OutportLocation,
    },
    ///Variable that was imported into this scope at `import_port`,
    Imported {
        import_port: OutportLocation,
        last_use: OutportLocation,
    },
}

pub struct Scope {
    ///True, if this scope mutates any parent-captured
    /// variables
    pub mutate_on_close: bool,
    ///The region this scope is in. Helps us detect region change between scopes.
    pub region: RegionLocation,
    pub vars: AHashMap<String, VarDef>,
    //If the scope produces a result, this is its source
    pub result: Option<OutportLocation>,
}

impl Scope {
    pub fn empty(region: RegionLocation, mutate_on_close: bool) -> Self {
        Self {
            mutate_on_close,
            region,
            vars: AHashMap::default(),
            result: None,
        }
    }

    pub fn get_var_use(&self, var: &str) -> Option<OutportLocation> {
        if let Some(var) = self.vars.get(var) {
            match var {
                VarDef::FirstDef {
                    first_def: _,
                    last_use,
                } => Some(*last_use),
                VarDef::Imported {
                    import_port: _,
                    last_use,
                } => Some(*last_use),
            }
        } else {
            None
        }
    }
}

pub struct BlockCtx {
    pub block_span: Span,
    pub active_scope: Scope,
    pub parent_scopes: SmallColl<Scope>,
}

impl BlockCtx {
    pub fn empty(region: RegionLocation) -> Self {
        BlockCtx {
            block_span: Span::empty(),
            active_scope: Scope::empty(region, false),
            parent_scopes: SmallVec::default(),
        }
    }

    pub fn define_var(&mut self, name: String, at: OutportLocation) -> Result<(), OptError> {
        if self.active_scope.vars.contains_key(&name) {
            let err = OptError::Any {
                text: format!(
                    "Cannot create variable \"{}\", because such a variable was defined before in this scope!",
                    name
                ),
            };
            return Err(err);
        }

        self.active_scope.vars.insert(
            name,
            VarDef::FirstDef {
                first_def: at,
                last_use: at,
            },
        );
        Ok(())
    }

    pub fn write_var(&mut self, name: &str, new_origin: OutportLocation) -> Result<(), OptError> {
        if let Some(lastdef) = self.active_scope.vars.get_mut(name) {
            match lastdef {
                VarDef::FirstDef {
                    first_def: _,
                    last_use,
                } => *last_use = new_origin,
                VarDef::Imported {
                    import_port: _,
                    last_use,
                } => *last_use = new_origin,
            }
            Ok(())
        } else {
            let err = OptError::Any {
                text: format!(
                    "Cannot write variable \"{}\", because such a variable was not defined in this scope!",
                    name
                ),
            };
            return Err(err);
        }
    }

    pub fn write_or_define_var(
        &mut self,
        graph: &mut OptGraph,
        name: &str,
        new_origin: OutportLocation,
    ) -> Result<(), OptError> {
        //try to write to an existing
        if self.write_var(name, new_origin).is_ok() {
            return Ok(());
        }

        //otherwise import (since this must have existed before),
        //and immediatly redefine last use at this port
        let _ = self.try_import(graph, name)?;
        self.write_var(name, new_origin)
            .expect("Writing must work after importing");
        Ok(())
    }

    ///Tries to find the variable in any of the parent scopes, if successful, imports it into this scope
    fn try_import(
        &mut self,
        graph: &mut OptGraph,
        name: &str,
    ) -> Result<OutportLocation, OptError> {
        //This is a two pass process. Frist we reverse-iterate all parent scopes (which is a AST concept),
        //till we find `name`. If this ends, without finding the name, we can exit, if it finds the name, we can
        //then import the variable.
        //
        // The magic sauce of this process is, that we implicitly shadow (since a newly _used_ variable is not written back)
        // for shaddowing scopes, and we also define, if the scope is not shadowing.
        // If a variable was imported by a parent scope, it is also overwritten, if needed, in this scope.
        //
        //NOTE: this works with any value, including funtions. Which is how we resolve function-calls as well.

        let mut found_in = None;
        for parent_scope in self.parent_scopes.iter().rev() {
            if let Some(var_port) = parent_scope.vars.get(name) {
                let port = match var_port {
                    VarDef::FirstDef {
                        first_def: _,
                        last_use,
                    } => *last_use,
                    VarDef::Imported {
                        import_port: _,
                        last_use,
                    } => *last_use,
                };
                //found one
                found_in = Some(port);
                break;
            }
        }

        //bail
        let port = if let Some(fi) = found_in {
            fi
        } else {
            let err = OptError::Any {
                text: format!("Could not find \"{name}\" in any parent scope!"),
            };
            return Err(err);
        };

        //we _silent-import_, if the port is already in the correct region. This happens if there is only a scope distinction on a AST
        //level, but not the graph anymore. For instance when _using_ stuff in a ScopedCall, without control-flow.
        let imported_at = if graph[port.node].parent == Some(self.active_scope.region) {
            port
        } else {
            //NOTE: we decide the _import_ path, based on the producer _type_. λs are imported as context, everything else as
            //      as arguments
            let producer_port = if let Some(prod) = graph.find_producer_out(port) {
                prod
            } else {
                //If we can't find the producer, check if it is a context variable. In that case, we are likely trying to import
                //unset context (for instance when building an impl block).
                //for those its okay to _just use_ that port, without having a producer
                if let OutputType::ContextVariableArgument(_i) = port.output {
                    port
                } else {
                    let err = OptError::Internal(format!(
                        "Could not route \"{name}\" into region. This is a bug!"
                    ));
                    report(error_reporter(err.clone(), Span::empty()).finish());
                    return Err(err);
                }
            };

            //Note: We import Lambda-Declerations as context, everything else as arguments
            if producer_port.output == OutputType::LambdaDeclaration {
                if graph.is_in_parent(self.active_scope.region.node, producer_port.node) {
                    //this would create a recursive call, which is not allowed.
                    //
                    //bail in that case
                    let err = OptError::Any {
                        text: format!(
                            "Cannot use \"{}\", as this would create a recursivecall",
                            name
                        ),
                    };
                    report(
                        error_reporter(err.clone(), self.block_span.clone())
                            .with_label(
                                Label::new(self.block_span.clone()).with_message("In this region"),
                            )
                            .finish(),
                    );
                    return Err(err);
                }

                let (port, _path) = graph.import_context(port, self.active_scope.region)?;
                port
            } else {
                let (port, _path) = graph.import_argument(port, self.active_scope.region)?;
                port
            }
        };

        //define and return
        let old = self.active_scope.vars.insert(
            name.to_owned(),
            VarDef::Imported {
                import_port: imported_at,
                last_use: imported_at,
            },
        );
        //should not overwrite anything, but just be sure
        assert!(old.is_none());
        //Reuse the existing port
        Ok(imported_at)
    }

    ///Finds, or imports the variable into scope. Fails, if no such variable was defined
    ///in this, or any of the parent scopes.
    pub fn find_variable(
        &mut self,
        graph: &mut OptGraph,
        name: &str,
    ) -> Result<OutportLocation, OptError> {
        if let Some(src) = self.active_scope.get_var_use(name) {
            //Found on this scope
            Ok(src)
        } else {
            //Did not find, try to import from parents
            match self.try_import(graph, name) {
                Ok(port) => Ok(port),
                Err(err) => Err(err),
            }
        }
    }

    ///Opens a new, empty scope in `region`.
    pub fn open_new_scope(&mut self, region: RegionLocation, mutate_on_close: bool) {
        let mut new_scope = Scope::empty(region, mutate_on_close);
        //switch active and _to_be_paren_
        std::mem::swap(&mut new_scope, &mut self.active_scope);
        //now push the _old_ active scope to the parents
        self.parent_scopes.push(new_scope);
    }
    ///Closes the scope, and pops back the last parent. Returns the active scope at _closing_ time.
    pub fn close_scope(&mut self) -> Scope {
        //pop last parent, then switch it back to active and return the _old_ scope
        let mut old = self.parent_scopes.pop().expect("tried to close root scope");
        std::mem::swap(&mut self.active_scope, &mut old);
        old
    }
}

impl Optimizer {
    pub fn build_block(
        &mut self,
        region: RegionLocation,
        mut block: Block,
        ctx: &mut BlockCtx,
    ) -> Result<(), VolaError<OptError>> {
        //HACK:
        //if the last stmt in a block is a branch with
        //results, and no return_expression is given, assume that this _is_
        //the return expression instead. This is a shortcoming of the current tree-sitter parser.
        // if any parse _does this righ_, it shouldn't happen anyways.

        //always mark the region's span
        self.span_tags.set(region.into(), block.span);

        if block.retexpr.is_none() {
            if let Some(Stmt::Branch(b)) = block.stmts.last().cloned() {
                #[cfg(feature = "log")]
                log::info!("Using branch-return hack for {}", b.span);
                //move the branch to the block's result, instead
                block.retexpr = Some(Expr {
                    span: b.span.clone(),
                    expr_ty: vola_ast::alge::ExprTy::Branch(Box::new(b)),
                });
                //and remove the branch
                block.stmts.remove(block.stmts.len() - 1);
            }
        }

        for stmt in block.stmts {
            self.build_stmt(stmt, region, ctx)?;
        }
        //Build the return expression, if there is any
        if let Some(retexpr) = block.retexpr {
            let outport = self.build_expr(retexpr, region, ctx)?;
            ctx.active_scope.result = Some(outport);
        }

        Ok(())
    }

    pub fn build_stmt(
        &mut self,
        stmt: Stmt,
        region: RegionLocation,
        ctx: &mut BlockCtx,
    ) -> Result<(), VolaError<OptError>> {
        match stmt {
            Stmt::Assign(assign) => match ctx.find_variable(&mut self.graph, &assign.dst.0) {
                Ok(_old_src) => {
                    let expr_src = self.build_expr(assign.expr, region, ctx)?;
                    //Rewrite expression-src to new assignment
                    if let Err(err) = ctx.write_var(&assign.dst.0, expr_src) {
                        return Err(VolaError::error_here(err, assign.span.clone(), "here"));
                    }
                    Ok(())
                }
                Err(err) => {
                    return Err(VolaError::error_here(err, assign.span.clone(), "here"));
                }
            },
            Stmt::Csg(csg_binding) => {
                let expr_src = self.build_expr(csg_binding.expr, region, ctx)?;
                //NOTE: We make sure that the produced value's src is a CSG node, otherwise we fail, because thats
                //the semantic of the node
                let producer = self.graph.find_producer_simple(expr_src);
                for prod in producer {
                    //can only test for simple-node producers, otherwise the type-derive will catch it later on.
                    if self.graph[prod.node].node_type.is_simple() {
                        if self.graph[prod.node]
                            .node_type
                            .unwrap_simple_ref()
                            .node
                            .dialect()
                            != "csg"
                        {
                            let node_name = self.graph[prod.node].name();
                            let node_span = self.find_span(prod.node.into());
                            let err = OptError::Any {
                                text: format!("Expected a CSG node to be bound to a CSG variable, but \"{}\" is not a CSG operation", node_name),
                            };
                            if let Some(span) = node_span {
                                return Err(VolaError::error_here(
                                    err,
                                    csg_binding.span.clone(),
                                    "here",
                                )
                                .with_label(span, "this should be a CSG-value"));
                            } else {
                                return Err(VolaError::error_here(
                                    err,
                                    csg_binding.span.clone(),
                                    "here",
                                ));
                            };
                        }
                    }
                }

                if let Err(error) = ctx.define_var(csg_binding.decl_name.0, expr_src) {
                    return Err(VolaError::error_here(error, csg_binding.span, "here"));
                }
                Ok(())
            }
            Stmt::Let(let_binding) => {
                let expr_src = self.build_expr(let_binding.expr, region, ctx)?;
                //NOTE: We make sure that the produced value's src is a CSG node, otherwise we fail, because thats
                //the semantic of the node
                let producer = self.graph.find_producer_simple(expr_src);
                for prod in producer {
                    //note we can only check nodes that are created _here_.
                    if self.graph[prod.node].node_type.is_simple() {
                        if self.graph[prod.node]
                            .node_type
                            .unwrap_simple_ref()
                            .node
                            .dialect()
                            == "csg"
                        {
                            let node_name = self.graph[prod.node].name();
                            let node_span = self.find_span(prod.node.into());
                            let err = OptError::Any {
                                text: format!(
                                    "Expected a no CSG node to be bound to a variable, but {} is CSG",
                                    node_name
                                ),
                            };
                            let labeled_err = if let Some(span) = node_span {
                                return Err(VolaError::error_here(
                                    err,
                                    let_binding.span.clone(),
                                    "here",
                                )
                                .with_label(span, "this should not be a CSG"));
                            } else {
                                return Err(VolaError::error_here(
                                    err,
                                    let_binding.span.clone(),
                                    "here",
                                ));
                            };
                        }
                    }
                }

                if let Err(error) = ctx.define_var(let_binding.decl_name.0, expr_src) {
                    return Err(VolaError::error_here(
                        error,
                        let_binding.span.clone(),
                        "here",
                    ));
                }

                Ok(())
            }
            Stmt::Branch(b) => self.build_branch_stmt(b, region, ctx),
            Stmt::Loop(loopstmt) => self.build_loop_stmt(loopstmt, region, ctx),
            Stmt::Block(b) => {
                let current_region = region;
                ctx.open_new_scope(current_region, false);
                self.build_block(region, *b, ctx)?;
                let closed = ctx.close_scope();
                if let Some(return_value) = closed.result {
                    return Err(VolaError::error_here(
                        OptError::Any {
                            text: "block has unexpected result".to_owned(),
                        },
                        self.find_span(return_value.into()).unwrap_or(Span::empty()),
                        "Block statement should not have a return-value, consider binding the value to a result",
                    ));
                }
                Ok(())
            }
            //Ignoring comments
            Stmt::Comment(_) => Ok(()),
        }
    }

    pub fn build_branch_stmt(
        &mut self,
        branch: Branch,
        region: RegionLocation,
        ctx: &mut BlockCtx,
    ) -> Result<(), VolaError<OptError>> {
        //TODO: make sure both blocks don't have a result, maybe warn on _unused-result?_
        //      then make sure to overwrite new var-defs, and feed-through any unused, but captured
        //      variables

        if branch.conditional.1.retexpr.is_some() {
            let warn = "Branch has result, but is part of a statement. Return value won't be used!"
                .to_owned();
            report(
                warning_reporter(warn, branch.span.clone())
                    .with_label(
                        Label::new(branch.conditional.1.retexpr.as_ref().unwrap().span.clone())
                            .with_message("This result"),
                    )
                    .finish(),
            );
        }
        if let Some(unconditional) = &branch.unconditional {
            if unconditional.retexpr.is_some() {
                let warn =
                    "Branch has result, but is part of a statement. Return value won't be used!"
                        .to_owned();
                report(
                    warning_reporter(warn, branch.span.clone())
                        .with_label(
                            Label::new(unconditional.retexpr.as_ref().unwrap().span.clone())
                                .with_message("This result"),
                        )
                        .finish(),
                );
            }
        }

        //Since this is the stmt-flavour, record any used variable, and exit it from the loop/mark as used after executing any
        let mut used_vars_import = AHashMap::default();
        //setup the gamma-node

        let condition_src = self.build_expr(branch.conditional.0, region, ctx)?;
        let (gamma, if_index, else_index) = self
            .graph
            .on_region(&region, |reg| {
                let (g, (if_bidx, else_bidx)) = reg.new_decission(|g| {
                    let (if_branch, _) = g.new_branch(|_, _| {});
                    let (else_branch, _) = g.new_branch(|_, _| {});
                    (if_branch, else_branch)
                });

                //connect condition
                reg.ctx_mut()
                    .connect(
                        condition_src,
                        g.as_inport_location(InputType::GammaPredicate),
                        OptEdge::value_edge_unset(),
                    )
                    .unwrap();
                (g, if_bidx, else_bidx)
            })
            .unwrap();

        //Serialize If-branch
        let if_region = RegionLocation {
            node: gamma,
            region_index: if_index,
        };
        ctx.open_new_scope(if_region, true);
        self.build_block(if_region, *branch.conditional.1, ctx)?;
        let post_if_scope = ctx.close_scope();

        //pre-setup the else branch and scope
        let else_branch = RegionLocation {
            node: gamma,
            region_index: else_index,
        };
        ctx.open_new_scope(else_branch, true);

        //register all used, imported variables. This makes sure we don't export
        // local variables.
        for (var_name, vdef) in post_if_scope.vars.iter() {
            if let VarDef::Imported {
                import_port,
                last_use: _,
            } = vdef
            {
                //register as used...
                let import_port_type = import_port.output.map_out_of_region().unwrap();
                let old = used_vars_import.insert(
                    var_name.to_owned(),
                    gamma.as_inport_location(import_port_type),
                );
                assert!(old.is_none());
                //..and forward the definition to the else-branch's scope
                let in_else_branch = gamma
                    .as_outport_location(import_port_type.map_to_in_region(else_index).unwrap());
                ctx.active_scope.vars.insert(
                    var_name.to_owned(),
                    VarDef::Imported {
                        import_port: in_else_branch,
                        last_use: in_else_branch,
                    },
                );
            }
        }

        //if there is another block, serialize it
        if let Some(unconditional) = branch.unconditional {
            self.build_block(else_branch, *unconditional, ctx)?;
        }
        let post_else_block = ctx.close_scope();
        //push all _newly_ registered variables
        for (varkey, var) in post_else_block.vars.iter() {
            //ignore known ones
            if used_vars_import.contains_key(varkey) {
                continue;
            }
            //otherwise add, if its an import
            if let VarDef::Imported {
                import_port,
                last_use: _,
            } = var
            {
                let import_port_type = import_port.output.map_out_of_region().unwrap();
                let old = used_vars_import.insert(
                    varkey.to_owned(),
                    gamma.as_inport_location(import_port_type),
                );
                assert!(old.is_none());
            }
        }

        //finally feed-through all _used_ variables, using the closed scopes's last-use
        //information.
        //by definition the stmt-flavour can not have a result. So we can just allocate exit-vars as we please
        //
        //We do a little optimization already. We only export the var, if it was not just used (in any of both branches), but
        //written to as well. We know this, if the last-use port is not the import port, of the registered variable.
        //
        //otherwise we don't have to export it, since the old _out-of-gamma_ value is still valid.
        for (used_var, _ev) in used_vars_import {
            if Self::is_read_write_branch(&post_if_scope, &post_else_block, used_var.as_str()) {
                let exit_var_idx = self.graph[gamma]
                    .node_type
                    .unwrap_gamma_mut()
                    .add_exit_var();
                //connect both results to the exit_var_port
                for (reg_idx, src) in [
                    (if_index, post_if_scope.get_var_use(&used_var).unwrap()),
                    (else_index, post_else_block.get_var_use(&used_var).unwrap()),
                ] {
                    let in_region_result =
                        gamma.as_inport_location(InputType::ExitVariableResult {
                            branch: reg_idx,
                            exit_variable: exit_var_idx,
                        });
                    self.graph
                        .connect(src, in_region_result, OptEdge::value_edge_unset())
                        .unwrap();
                }

                //As the last action, register the just fed-through variable in the parent context as _used_
                let new_use_port =
                    gamma.as_outport_location(OutputType::ExitVariableOutput(exit_var_idx));
                ctx.write_var(&used_var, new_use_port).unwrap();
            }
        }

        Ok(())
    }

    fn is_read_write_branch(if_scope: &Scope, else_scope: &Scope, name: &str) -> bool {
        let is_read_write = |scope: &Scope, name: &str| {
            if let VarDef::Imported {
                import_port,
                last_use,
            } = scope.vars.get(name).expect("must be present if called")
            {
                import_port != last_use
            } else {
                panic!("{name} should be imported into scope, but wasn't");
            }
        };

        is_read_write(if_scope, name) || is_read_write(else_scope, name)
    }

    pub fn build_loop_stmt(
        &mut self,
        loopstmt: Loop,
        region: RegionLocation,
        ctx: &mut BlockCtx,
    ) -> Result<(), VolaError<OptError>> {
        //We have to write the loop down as a Gamma-Node, where the _no-iteration_ case is handeled by a branch, while the iteration itself
        //is handeled by the theta node.
        //
        //we do this constructing an artificial Branch AST node. The condition is the lower-bound of the iteration.
        //so given "for a in x..y" the "must-run" condition is x<y.
        //in that case the loop woud run _at-all_ once.
        //in all other cases, x>=y holds, so we take the _is-not-running_ branch.

        let rangespan = loopstmt.range_expr_span();
        let lower_bound = self.build_expr(loopstmt.bound_lower, region, ctx)?;
        let upper_bound = self.build_expr(loopstmt.bound_upper, region, ctx)?;

        let run_condition = self
            .graph
            .on_region(&region, |reg| {
                let condition = OptNode::new(BinaryRel::new(BinaryRelOp::Lt), Span::empty());
                let (node, _edgs) = reg
                    .connect_node(condition, [lower_bound, upper_bound])
                    .unwrap();

                self.span_tags.set(node.into(), rangespan.clone());
                node
            })
            .unwrap();

        let (gamma, (loop_branch, norun_branch)) = self
            .graph
            .on_region(&region, |reg| {
                reg.new_decission(|g| {
                    let (in_loop, _) = g.new_branch(|_, _| {});
                    let (no_run, _) = g.new_branch(|_, _| {});

                    (in_loop, no_run)
                })
            })
            .unwrap();

        //connect gamma-condition to should-run-or-not gamma
        self.graph
            .connect(
                run_condition.output(0),
                gamma.as_inport_location(InputType::GammaPredicate),
                OptEdge::value_edge_unset(),
            )
            .unwrap();

        //For the gamma-node, the idea is similar to how we build a gamma-stmt,
        //but we just _know_ that we don't use anything in the else case. So feeding through is easier.

        //First, build the loop-branch by entering a new scope, then emitting the loop-node into that branch.
        //
        //By definition the lower and upper bound are imported first as loop-variables.
        let loop_region = RegionLocation {
            node: gamma,
            region_index: loop_branch,
        };
        let (theta, lower, upper) = self
            .graph
            .on_region(&loop_region, |reg| {
                let (theta_node, (lower, upper)) = reg.new_loop(|t| {
                    //add the walking bound, and the upper bound as loop-variables
                    let (lower_in, lower_arg, lower_res, lower_out) = t.add_loop_variable();
                    let (upper_in, upper_arg, upper_res, upper_out) = t.add_loop_variable();

                    //build the index-increaser and feed out the new-value
                    t.on_loop(|reg| {
                        //NOTE: we have to increase the lower bound, before testing,
                        //      since the condition decides if we take
                        //      the "next" loop.

                        //increase the lower bound and connect it to its loop-variable
                        let one = reg.insert_node(OptNode::new(ImmNat::new(1), rangespan.clone()));
                        let (added, _) = reg
                            .connect_node(
                                OptNode::new(
                                    BinaryArith::new(BinaryArithOp::Add),
                                    rangespan.clone(),
                                ),
                                [lower_arg, one.output(0)],
                            )
                            .unwrap();

                        let (condition, _) = reg
                            .connect_node(
                                OptNode::new(BinaryRel::new(BinaryRelOp::Lt), rangespan.clone()),
                                [added.output(0), upper_arg],
                            )
                            .unwrap();
                        self.span_tags.set(condition.into(), rangespan.clone());
                        //connect the condition to the theta's condition port
                        reg.connect_to_result(condition.output(0), InputType::ThetaPredicate)
                            .unwrap();

                        //Connect increased lower bound
                        reg.connect_to_result(added.output(0), lower_res.input)
                            .unwrap();
                        //connect _static_ upper bound back to itself
                        reg.connect_to_result(upper_arg, upper_res.input).unwrap();
                    });
                    //return back the connections of the loop bounds
                    ((lower_in, lower_out), (upper_in, upper_out))
                });
                (theta_node, lower, upper)
            })
            .unwrap();

        //anonyme-connect the lower and upper bounds in the loop-branch
        let anonym_lower = self.graph[gamma]
            .node_type
            .unwrap_gamma_mut()
            .add_entry_var();
        let anonym_upper = self.graph[gamma]
            .node_type
            .unwrap_gamma_mut()
            .add_entry_var();
        //connect bound-expr to the anony-import
        self.graph
            .connect(
                lower_bound,
                gamma.as_inport_location(InputType::EntryVariableInput(anonym_lower)),
                OptEdge::value_edge_unset(),
            )
            .unwrap();
        self.graph
            .connect(
                upper_bound,
                gamma.as_inport_location(InputType::EntryVariableInput(anonym_upper)),
                OptEdge::value_edge_unset(),
            )
            .unwrap();
        //connect bound-expr in loop-branch to bound-loop-variables
        self.graph
            .connect(
                gamma.as_outport_location(OutputType::EntryVariableArgument {
                    branch: loop_branch,
                    entry_variable: anonym_lower,
                }),
                lower.0,
                OptEdge::value_edge_unset(),
            )
            .unwrap();
        self.graph
            .connect(
                gamma.as_outport_location(OutputType::EntryVariableArgument {
                    branch: loop_branch,
                    entry_variable: anonym_upper,
                }),
                upper.0,
                OptEdge::value_edge_unset(),
            )
            .unwrap();

        //at this point we have setup the gamma-theta nesting. Now double-open the loop-branch, build the loop-block,
        //and handle feeding out all used and unused variables.

        let loop_body = RegionLocation {
            node: theta,
            region_index: 0,
        };
        ctx.open_new_scope(loop_region, true);
        ctx.open_new_scope(loop_body, true);
        //define the lower_anonym loop bound as the loop-variable that is referenced by the ident
        let loop_variable_port = theta.as_outport_location(OutputType::Argument(anonym_lower));
        self.names.set(
            loop_variable_port.into(),
            loopstmt.iteration_variable_ident.0.clone(),
        );
        self.names.set(lower.0.into(), "lower-bound".to_owned());
        self.names.set(lower.1.into(), "lower-bound".to_owned());
        self.names.set(upper.0.into(), "upper-bound".to_owned());
        self.names.set(upper.1.into(), "upper-bound".to_owned());
        self.span_tags.set(theta.into(), loopstmt.span.clone());
        self.span_tags.set(gamma.into(), loopstmt.span.clone());
        let old = ctx.active_scope.vars.insert(
            loopstmt.iteration_variable_ident.0,
            VarDef::FirstDef {
                first_def: loop_variable_port,
                last_use: loop_variable_port,
            },
        );
        assert!(old.is_none());
        self.build_block(loop_body, *loopstmt.body, ctx)?;
        let post_loop = ctx.close_scope();
        let _post_loopbranch = ctx.close_scope();

        struct RWVar {
            import_argument: OutportLocation,
            result_connection: InportLocation,
        }
        //handle all loop variables. There are two cases (again)
        //
        // 1. a variable is imported, and read, but not written. In that case for
        //    a LV-n, map the argument-port to same result port.
        // 2. a variable is imported, and written. In that case, map the new-value to the result port,
        //    and register it as _written_ in the use-table
        let mut readwrite_vars = AHashMap::default();
        for (var, vardef) in post_loop.vars.iter() {
            if let VarDef::Imported {
                import_port,
                last_use,
            } = vardef
            {
                assert_eq!(import_port.node, theta);
                let result_lv = if let OutputType::Argument(idx) = import_port.output {
                    //Lower and upper bound are already self-connected accordingly in the theta-building procedure, so ignore those
                    //we also don't want to redefine the lower-bound in the loop
                    if idx < 2 {
                        if import_port != last_use {
                            let err = OptError::Any { text: format!("Tried to redefine loop bound \"{var}\" in loop,  which is not allowed!") };
                            return Err(VolaError::error_here(err, rangespan, "for this bound"));
                        }
                        continue;
                    }

                    theta.as_inport_location(InputType::Result(idx))
                } else {
                    panic!("assumed argument port!");
                };

                if import_port == last_use {
                    //is used, but not redefined, map back to its own lv
                    self.graph
                        .connect(*import_port, result_lv, OptEdge::value_edge_unset())
                        .unwrap();
                } else {
                    //is read-write, register, and map last-use
                    self.graph
                        .connect(*last_use, result_lv, OptEdge::value_edge_unset())
                        .unwrap();
                    readwrite_vars.insert(
                        var.clone(),
                        RWVar {
                            import_argument: *import_port,
                            result_connection: result_lv,
                        },
                    );
                }
            }
        }

        //Now, after mapping everything that happened in the theta node, we know modified variables in the loop-stmt, so we can use
        //a similar approach to the gamma-builder.
        // Any variable that is read-write in theta: export the LV-N to EX-M, and register as used.
        // then find the entry-variable we got that variable from in the first place (in the loop-case), and route that straight through in the
        // no-loop branch.
        for (ident, var) in readwrite_vars {
            let export_ex = self.graph[gamma]
                .node_type
                .unwrap_gamma_mut()
                .add_exit_var();
            let var_output = var
                .result_connection
                .node
                .as_outport_location(var.result_connection.input.map_out_of_region().unwrap());
            //connect var_src to the just created exit-var
            self.graph
                .connect(
                    var_output,
                    gamma.as_inport_location(InputType::ExitVariableResult {
                        branch: loop_branch,
                        exit_variable: export_ex,
                    }),
                    OptEdge::value_edge_unset(),
                )
                .unwrap();
            //now connect the import_src (in the no-loop branch) to the exit-var
            let no_run_src = {
                //This should be the input to the theta-node
                let theta_input = var
                    .import_argument
                    .node
                    .as_inport_location(var.import_argument.output.map_out_of_region().unwrap());
                //now find the connected EV, which _should_ be directly connected (by the import).
                let mut ev_arg = self.graph.inport_src(theta_input).unwrap();
                assert_eq!(ev_arg.node, gamma);
                //is gamma-ev, so we can _just_ change the region to the
                //no-run branch
                let _ = ev_arg.output.change_region_index(norun_branch);
                ev_arg
            };
            self.graph
                .connect(
                    no_run_src,
                    gamma.as_inport_location(InputType::ExitVariableResult {
                        branch: norun_branch,
                        exit_variable: export_ex,
                    }),
                    OptEdge::value_edge_unset(),
                )
                .unwrap();

            self.names
                .set(var.import_argument.into(), format!("Run value: {ident}"));
            self.names
                .set(no_run_src.into(), format!("No-Run value: {ident}"));

            //finally redefine the variable in the theta-parent (AST) scope
            ctx.write_or_define_var(
                &mut self.graph,
                &ident,
                gamma.as_outport_location(OutputType::ExitVariableOutput(export_ex)),
            )
            .unwrap();
        }

        Ok(())
    }
}
