/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::{OptEdge, OptError, Optimizer};
use ahash::AHashMap;
use rvsdg::{
    edge::{InportLocation, LangEdge, OutportLocation},
    region::RegionLocation,
};
use rvsdg_viewer::View;
use vola_ast::{
    alge::Expr,
    common::{Block, Branch, Loop, Stmt},
};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

pub struct BlockCtx {
    pub block_span: Span,
    pub defined_vars: AHashMap<String, OutportLocation>,
    pub result_connector: Option<InportLocation>,
    ///tracks how externally used, and named variables are used. Lets a recursive call
    //map back _changed_ variables. Mostly used for LoopVars/ EntryExit pairs.
    pub external_used_vars: AHashMap<String, (InportLocation, OutportLocation)>,
}

impl BlockCtx {
    pub fn empty() -> Self {
        BlockCtx {
            block_span: Span::empty(),
            defined_vars: AHashMap::default(),
            result_connector: None,
            external_used_vars: AHashMap::default(),
        }
    }

    pub fn find_variable(&self, name: &str) -> Result<OutportLocation, OptError> {
        if let Some(src) = self.defined_vars.get(name).cloned() {
            Ok(src)
        } else {
            let err = OptError::Any {
                text: format!("Could not find variable \"{name}\""),
            };
            report(
                error_reporter(err.clone(), self.block_span.clone().into())
                    .with_label(
                        Label::new(self.block_span.clone().into())
                            .with_message("In this region".to_owned()),
                    )
                    .finish(),
            );
            Err(err)
        }
    }
}

impl Optimizer {
    pub fn build_block(
        &mut self,
        region: RegionLocation,
        block: Block,
        mut ctx: BlockCtx,
    ) -> Result<(), OptError> {
        for stmt in block.stmts {
            self.build_stmt(stmt, region, &mut ctx)?;
        }

        match (block.retexpr, ctx.result_connector) {
            (Some(retexpr), Some(retconn)) => {
                let outport = self.build_expr(retexpr, region, &mut ctx)?;
                self.graph
                    .connect(outport, retconn, OptEdge::value_edge())?;
            }
            (None, Some(_retcon)) => {
                let err = OptError::Any {
                    text: format!("Expected returned value, but had no return expression"),
                };
                report(error_reporter(err.clone(), ctx.block_span.clone().into()).finish());
                return Err(err);
            }
            (Some(_retexpr), None) => {
                let err = OptError::Any {
                    text: format!("Expected no return value, but had a return expression"),
                };
                report(error_reporter(err.clone(), ctx.block_span.clone().into()).finish());
                return Err(err);
            }
            (None, None) => {}
        }

        Ok(())
    }

    pub fn build_stmt(
        &mut self,
        stmt: Stmt,
        region: RegionLocation,
        ctx: &mut BlockCtx,
    ) -> Result<(), OptError> {
        match stmt {
            Stmt::Assign(assign) => match ctx.defined_vars.get(&assign.dst.0).cloned() {
                Some(old_src) => {
                    let expr_src = self.build_expr(assign.expr, region, ctx)?;
                    //Rewrite expression-src to new assignment
                    let second_old = ctx.defined_vars.insert(assign.dst.0, expr_src);

                    assert_eq!(
                        second_old,
                        Some(old_src),
                        "assignment changed while serializing assignment"
                    );
                    Ok(())
                }
                None => {
                    let err = OptError::Any {
                        text: format!(
                            "Cannot assign \"{}\", because no such variable was defined before!",
                            assign.dst.0
                        ),
                    };

                    report(
                        error_reporter(err.clone(), assign.span.clone().into())
                            .with_label(Label::new(assign.span.clone()).with_message("here"))
                            .finish(),
                    );
                    Err(err)
                }
            },
            Stmt::Csg(csg_binding) => {
                if ctx.defined_vars.contains_key(&csg_binding.decl_name.0) {
                    let err = OptError::Any {
                        text: format!(
                            "Cannot create csg variable \"{}\", because such a variable was defined before!",
                            csg_binding.decl_name.0
                        ),
                    };

                    report(
                        error_reporter(err.clone(), csg_binding.span.clone().into())
                            .with_label(Label::new(csg_binding.span.clone()).with_message("here"))
                            .finish(),
                    );
                    return Err(err);
                }
                let expr_src = self.build_expr(csg_binding.expr, region, ctx)?;
                //NOTE: We make sure that the produced value's src is a CSG node, otherwise we fail, because thats
                //the semantic of the node
                let producer = self.graph.find_producer_out(expr_src).unwrap();
                if self.graph[producer.node]
                    .node_type
                    .unwrap_simple_ref()
                    .node
                    .dialect()
                    == "csg"
                {
                    let node_name = self.graph[producer.node].name();
                    let node_span = self.find_span(producer.node.into());
                    let err = OptError::Any {
                        text: format!("Expected a CSG node to be bound to a CSG variable, but \"{}\" is not a CSG operation", node_name),
                    };
                    let errreport = error_reporter(err.clone(), csg_binding.span.clone().into());
                    let labeled_err = if let Some(span) = node_span {
                        errreport.with_label(
                            Label::new(span.into()).with_message("this should be a CSG"),
                        )
                    } else {
                        errreport
                    };
                    report(labeled_err.finish());
                    return Err(err);
                }
                ctx.defined_vars.insert(csg_binding.decl_name.0, expr_src);
                Ok(())
            }
            Stmt::Let(let_binding) => {
                if ctx.defined_vars.contains_key(&let_binding.decl_name.0) {
                    let err = OptError::Any {
                            text: format!(
                                "Cannot create variable \"{}\", because such a variable was defined before!",
                                let_binding.decl_name.0
                            ),
                        };

                    report(
                        error_reporter(err.clone(), let_binding.span.clone().into())
                            .with_label(Label::new(let_binding.span.clone()).with_message("here"))
                            .finish(),
                    );
                    return Err(err);
                }
                let expr_src = self.build_expr(let_binding.expr, region, ctx)?;
                //NOTE: We make sure that the produced value's src is a CSG node, otherwise we fail, because thats
                //the semantic of the node
                let producer = self.graph.find_producer_out(expr_src).unwrap();
                if self.graph[producer.node]
                    .node_type
                    .unwrap_simple_ref()
                    .node
                    .dialect()
                    != "csg"
                {
                    let node_name = self.graph[producer.node].name();
                    let node_span = self.find_span(producer.node.into());
                    let err = OptError::Any {
                        text: format!(
                            "Expected a no CSG node to be bound to a variable, but {} is CSG",
                            node_name
                        ),
                    };
                    let errreport = error_reporter(err.clone(), let_binding.span.clone().into());
                    let labeled_err = if let Some(span) = node_span {
                        errreport.with_label(
                            Label::new(span.into()).with_message("this should not be a CSG"),
                        )
                    } else {
                        errreport
                    };
                    report(labeled_err.finish());
                    return Err(err);
                }
                ctx.defined_vars.insert(let_binding.decl_name.0, expr_src);
                Ok(())
            }
            Stmt::Branch(b) => self.build_branch_stmt(b, region, ctx),
            Stmt::Loop(loopstmt) => self.build_loop_stmt(loopstmt, region, ctx),
        }
    }

    pub fn build_branch_stmt(
        &mut self,
        branch: Branch,
        region: RegionLocation,
        ctx: &mut BlockCtx,
    ) -> Result<(), OptError> {
        todo!("implement branch statements")
    }

    pub fn build_loop_stmt(
        &mut self,
        loopstmt: Loop,
        region: RegionLocation,
        ctx: &mut BlockCtx,
    ) -> Result<(), OptError> {
        todo!("implement_ loop stmt")
    }
}
