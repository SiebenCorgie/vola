/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use ahash::AHashMap;
use rvsdg::{edge::{OutportLocation, OutputType}, nodes::NodeType, region::RegionLocation, smallvec::{smallvec, SmallVec}, NodeRef, SmallColl};
use vola_ast::{alge::{EvalExpr, Expr, ExprTy}, common::Stmt, csg::{AccessDesc, CSGNodeTy}};
use vola_common::{ariadne::{Color, Fmt, Label}, error::error_reporter, report, Span};

use crate::{common::{LmdContext, Ty}, csg::TreeAccess, OptError, OptNode, Optimizer};


pub(crate) enum ReturnExpr{
    AccessDescriptors(AccessDesc),
    Expr(Expr),
    CsgOp(Expr)
}

impl ReturnExpr{
    fn describe(&self) -> &str{
        match self{
            Self::AccessDescriptors(_) => "access-descriptor",
            Self::Expr(_) => "algebraic-expression",
            Self::CsgOp(_) => "csg-expression"
        }
    }
}

mod algeexpr;
mod stmts;
mod csgtree;

#[derive(Debug, Clone, PartialEq)]
pub enum RetType{
    AlgebraicExpr,
    CsgTree,
    AccessDesc,
    None
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockBuilderConfig{
    //True only for impl-blocks and their 
    //internal sub-blocks
    pub allow_eval: bool,
    pub expect_return: RetType,
    pub allow_csg_stmt: bool,
}

impl BlockBuilderConfig{
    pub fn impl_block(csgnode_ty: CSGNodeTy) -> Self{
        BlockBuilderConfig { allow_eval: csgnode_ty == CSGNodeTy::Operation, expect_return: RetType::AlgebraicExpr, allow_csg_stmt: false }
    }

    pub fn alge_fn() -> Self{
        BlockBuilderConfig { allow_eval: false, expect_return: RetType::AlgebraicExpr, allow_csg_stmt: false }
    }

    pub fn field_def() -> Self{
        BlockBuilderConfig { allow_eval: false, expect_return: RetType::CsgTree, allow_csg_stmt: true }
    }

    pub fn export_fn() -> Self{
        BlockBuilderConfig { allow_eval: false, expect_return: RetType::AccessDesc, allow_csg_stmt: true }
    }
}

///Handels the _block_ node of the AST.
///
///Mostly takes care of dispatchning the assignments
///as well as doing error-detection in the AST for the first set of
///semantic errors.
pub struct BlockBuilder<'a> {
    pub span: Span,
    pub config: BlockBuilderConfig,
    ///Named CSG operands, if there are any. Those cannot be shadowed.
    pub csg_operands: AHashMap<String, usize>,

    ///If known, the return type(s) of the block
    pub return_type: SmallColl<Ty>,
    ///Current _variable_ context for this block.
    ///FIX: name differently. Since this is more of a _region_context_ at this point.
    pub lmd_ctx: LmdContext,

    ///The region we are building the block in.
    pub region: RegionLocation,
    pub opt: &'a mut Optimizer
}

impl<'a> BlockBuilder<'a> {

    pub fn inherite_ctx<'b>(&'b mut self, span: Span, config: BlockBuilderConfig, return_type: SmallColl<Ty>) -> BlockBuilder<'b>{
        BlockBuilder { 
            span, 
            config, 
            csg_operands: self.csg_operands.clone(), 
            return_type, 
            lmd_ctx: self.lmd_ctx.clone(), 
            region: self.region.clone(), 
            opt: self.opt  
        }
    }

    pub fn destroy_inherited(self) -> LmdContext{
        self.lmd_ctx
    }
    
    fn parent_node(&self) -> NodeRef{
        self.region.node
    }

    pub fn get_cv_for_eval(
        &mut self, 
        eval_expr: &EvalExpr
    ) -> Result<(OutportLocation, Ty), OptError>{
        if !self.config.allow_eval{
            let err = OptError::Any { 
                text: format!("Cannot use eval expression in this context.\nEval expressions are only allowed in concept implementations!"), 
            };
            report(
                error_reporter(err.clone(), eval_expr.span.clone())
                    .with_label(
                        Label::new(eval_expr.span.clone())
                            .with_message("consider moving this eval into a concept implementation."),
                    )
                    .finish(),
            );
            return Err(err);
        }

        
        //Before doing anything, check that the concept exists and has the right ammount of
        // arguments
        let concept = match self.opt.concepts.get(&eval_expr.concept.0) {
            Some(c) => c,
            None => {
                return Err(OptError::report_no_concept(
                    &eval_expr.span,
                    &eval_expr.concept.0,
                ));
            }
        };

        if concept.src_ty.len() != eval_expr.params.len() {
            return Err(OptError::report_argument_missmatch(
                &concept.span,
                concept.src_ty.len(),
                &eval_expr.span,
                eval_expr.params.len(),
            ));
        }

        let concept_ret_type = concept
            .dst_ty
            .clone()
            .try_into()
            .expect("Could not convert ast-type to opt-type");

        //Assumes that the operand exists
        if !self.csg_operands.contains_key(&eval_expr.evaluator.0) {
            let err = OptError::Any {
                text: format!("CSG-Operand {} does not exist", eval_expr.evaluator.0),
            };

            report(
                error_reporter(err.clone(), eval_expr.span.clone())
                    .with_label(
                        Label::new(eval_expr.span.clone())
                            .with_color(Color::Red)
                            .with_message("here"),
                    )
                    .with_label(
                        Label::new(self.span.clone()).with_message(
                            &format!(
                        "Consider adding a CSG-Operand named \"{}\" for this implementation.",
                        eval_expr.evaluator.0
                    )
                            .fg(Color::Blue),
                        ),
                    )
                    .finish(),
            );

            return Err(err);
        }
        let cv = *self.csg_operands.get(&eval_expr.evaluator.0).unwrap();

        let port = self.get_cv_argument_port(cv);

        Ok((port, concept_ret_type))
    }

    fn get_cv_argument_port(&self, cv: usize) -> OutportLocation{
        match &self.opt.graph.node(self.parent_node()).node_type{
            NodeType::Gamma(_) => todo!(),
            NodeType::Theta(_) => todo!(),
            NodeType::Phi(_) => todo!(),
            NodeType::Lambda(_) => OutportLocation { node: self.parent_node(), output: OutputType::ContextVariableArgument(cv) },
            _ => panic!("Unexpected node type!")
        }
    }



    ///Builds the block based on `stmts` and `ret`, and emits it into the given `region`.
    ///Returns the return_expression's ports, if there is any.
    pub(crate) fn build_block(
        &mut self,
        block: vola_ast::common::Block,
    ) -> Result<SmallColl<(Option<Ty>, OutportLocation)>, OptError>{

        
        for stmt in block.stmts{
            match stmt{
                Stmt::Csg(csgbind) => {
                    if !self.config.allow_csg_stmt{
                        let err = OptError::Any { text: format!("Csg Statement not allowed in this context") };
                        report(error_reporter(err.clone(), csgbind.span.clone()).with_label(Label::new(csgbind.span.clone()).with_message("csg binding is not allowed in this context!")).finish());
                        return Err(err);
                    }
                    self.setup_csg_binding(csgbind)?
                },
                Stmt::Let(letstmt) => self.setup_let(letstmt)?,
                Stmt::Assign(assign) => self.setup_assign(assign)?,
                Stmt::ThetaExpr => todo!("setup thetaexpr"),
            }
        }

        let return_expression = if let Some(expr) = block.retexpr{
            match self.config.expect_return{
                RetType::AlgebraicExpr => Some(ReturnExpr::Expr(expr)),
                RetType::CsgTree => Some(ReturnExpr::CsgOp(expr)),
                RetType::AccessDesc => {
                    match expr.expr_ty{
                        ExprTy::AccessExpr(expr) => Some(ReturnExpr::AccessDescriptors(expr)),
                        ExprTy::EvalExpr(expr) => Some(ReturnExpr::AccessDescriptors(AccessDesc { span: expr.span.clone(), evals: smallvec![expr] })),
                        _ => {
                            let err = OptError::Any { text: format!("Expected a access-description at the end") };
                            report(
                                error_reporter(err.clone(), block.span.clone())
                                    .with_label(
                                        Label::new(block.span.clone())
                                            .with_message("consider adding it at the end of this block")
                                    )
                                    .finish()
                            );
                            return Err(err);
                        }
                    }
                },
                RetType::None => {
                    let err = OptError::Any { text: format!("Expected no return value") };
                    report(
                        error_reporter(err.clone(), block.span.clone())
                            .with_label(
                                Label::new(block.span.clone())
                                    .with_message("consider removing the last expression from the end of this block")
                            )
                            .finish()
                    );
                    return Err(err);
                }
            }
        }else{
            if self.config.expect_return != RetType::None{
                let err = OptError::Any { text: format!("Expected no return value") };
                report(
                    error_reporter(err.clone(), block.span.clone())
                        .with_label(
                            Label::new(block.span.clone())
                                .with_message("consider removing the last expression from the end of this block")
                        )
                        .finish()
                );
                return Err(err);
            }else{
                None
            }
        };


        match (self.config.expect_return.clone(), return_expression){
            (RetType::AlgebraicExpr, Some(ReturnExpr::Expr(e))) => {
                let expr = self.setup_alge_expr(e)?;

                //Note we don't know the type of the expression yet, so just using _none_
                Ok(smallvec![(None, expr)])
            },
            (RetType::AlgebraicExpr, None) => {
                let err = OptError::Any { text: format!("Expected a algebraic return statment at the end") };
                report(
                    error_reporter(err.clone(), block.span.clone())
                        .with_label(
                            Label::new(block.span.clone())
                                .with_message("consider adding a algebraic return expression at the end of this block")
                        )
                        .finish()
                );
                Err(err)
            }
            (RetType::CsgTree, Some(ReturnExpr::CsgOp(op))) => Ok(smallvec![(Some(Ty::CSGTree), self.setup_csg_tree(op)?)]),
            (RetType::CsgTree, None) => {
                let err = OptError::Any { text: format!("Expected a csg return statment at the end") };
                report(
                    error_reporter(err.clone(), block.span.clone())
                        .with_label(
                            Label::new(block.span.clone())
                                .with_message("consider adding a csg-expression at the end of this block")
                        )
                        .finish()
                );
                Err(err)
            }
            (RetType::AccessDesc, Some(ReturnExpr::AccessDescriptors(ad))) => {
                let access = self.build_access(ad)?.into_iter().map(|(ty, port)| (Some(ty), port)).collect();
                Ok(access)
            },
            (RetType::AccessDesc, None) => {
                let err = OptError::Any { text: format!("Expected a access-description at the end of this block") };
                report(
                    error_reporter(err.clone(), block.span.clone())
                        .with_label(
                            Label::new(block.span.clone())
                                .with_message("consider adding a one, or a tuple of multiple eval expressions")
                        )
                        .finish()
                );
                Err(err)
            }
            (RetType::None, None) => Ok(SmallColl::new()),
            (RetType::None, Some(any)) => {
                let span = match &any{
                    ReturnExpr::AccessDescriptors(ad) => ad.span.clone(),
                    ReturnExpr::CsgOp(op) => op.op_span(),
                    ReturnExpr::Expr(e) => e.span.clone(),
                };
                let err = OptError::Any { text: format!("Expected no return statment, but had a {}", any.describe()) };
                report(
                    error_reporter(err.clone(), span.clone())
                        .with_label(
                            Label::new(span.clone())
                                .with_message("consider removing this, or making it a statement")
                        ).finish()
                );
                Err(err)
            },
            (expected, any) => {
                let err = OptError::Any { text: format!("Expected {:?} return statement, but got {}", expected, any.map(|t| t.describe().to_owned()).unwrap_or("none".to_owned())) };
                report(
                    error_reporter(err.clone(), block.span.clone())
                        .with_label(
                            Label::new(block.span.clone())
                                .with_message("this block needs to be fixed")
                        ).finish()
                );
                Err(err)
            }
        }
    }


    //Small subroutine that sets up the tree_access node and return the result output 
    fn build_access(&mut self, access: AccessDesc) -> Result<SmallColl<(Ty, OutportLocation)>, OptError>{
        let mut outports = SmallColl::new();
        for acc in access.evals{
            outports.push(self.build_sub_access(acc)?);
        }
        Ok(outports)
    }
    fn build_sub_access(&mut self, access_eval: EvalExpr) -> Result<(Ty, OutportLocation), OptError>{
        
        //check if we can find the tree.
        let field_src = match self.lmd_ctx.defined_vars.get(&access_eval.evaluator.0) {
            Some(field_src) => field_src,
            None => {
                return Err(OptError::report_variable_not_found(
                    &access_eval.span,
                    &access_eval.evaluator.0,
                ));
            }
        };

        let concept = match self.opt.concepts.get(&access_eval.concept.0) {
            None => {
                return Err(OptError::report_no_concept(
                    &access_eval.span,
                    &access_eval.concept.0,
                ));
            }
            Some(con) => con,
        };

        //While at it, make sure the call-parameter-count matches
        if concept.src_ty.len() != access_eval.params.len() {
            return Err(OptError::report_argument_missmatch(
                &concept.span,
                concept.src_ty.len(),
                &access_eval.span,
                access_eval.params.len(),
            ));
        }

        let concept_name = concept.name.clone();

        //We use the concept's return type to infer the output signature.
        let return_type: Ty = concept.dst_ty.clone().into();

        //register in output signature
        self.return_type.push(return_type.clone());
        
        //At this point we can be sure that the concept exists, and is at least called with the right amount
        // of arguments.
        // We can also be sure that a variable with the given name exists. Though we are not sure its a field
        // (yet?).
        // TODO: We could already tag the csg output with a CSG type, so we can be sure already.
        //      Or we just do that at the type-derive / type_check stage.

        let mut wires: SmallColl<OutportLocation> = SmallColl::new();
        wires.push(field_src.port.clone());

        let mut signature = SmallVec::new();
        //First argument must alwoas be the tree thats called.
        signature.push(Ty::CSGTree);
        //all following args are the callargs
        for arg in concept.src_ty.iter() {
            signature.push(
                arg.clone()
                    .try_into()
                    .expect("Could not convert tree call arg to opttype"),
            );
        }
        for arg in access_eval.params {
            let arg_port = self.setup_alge_expr(arg)?;
            wires.push(arg_port);
        }

        
        let access_output = self
            .opt
            .graph
            .on_region(&self.region, |reg| {
                let (node, _) = reg
                    .connect_node(
                        OptNode::new(
                            TreeAccess::new(concept_name, signature, return_type.clone()),
                            access_eval.span.clone(),
                        ),
                        &wires,
                    )
                    .unwrap();
                node.output(0)
            })
            .unwrap();

        Ok((return_type, access_output))
    }
    
}
