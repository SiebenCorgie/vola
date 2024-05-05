/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use ahash::AHashMap;
use rvsdg::{edge::{InputType, OutportLocation, OutputType}, nodes::NodeType, region::RegionLocation, smallvec::SmallVec, NodeRef, SmallColl};
use vola_ast::{alge::{AlgeExpr, AssignStmt, EvalExpr, LetStmt}, csg::{AccessDesc, CSGBinding}};
use vola_common::{ariadne::{Color, Fmt, Label}, error::error_reporter, report, Span};

use crate::{common::{LmdContext, Ty}, csg::TreeAccess, OptError, OptNode, Optimizer};

///Types of fetched statments for a block
pub(crate) enum FetchStmt{
    CSGBind(CSGBinding),
    Let(LetStmt),
    Assign(AssignStmt)
}

pub(crate) enum ReturnExpr{
    AccessDescriptors(SmallColl<AccessDesc>),
    Expr(AlgeExpr),
}

mod algeexpr;
mod stmts;
mod csgtree;
///Handels the _block_ node of the AST.
///
///Mostly takes care of dispatchning the assignments
///as well as doing error-detection in the AST for the first set of
///semantic errors.
pub struct BlockBuilder<'a> {
    pub span: Span,
    ///Named CSG operands, if there are any. Those cannot be shadowed.
    pub csg_operands: AHashMap<String, usize>,
    pub is_eval_allowed: bool,

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

    fn parent_node(&self) -> NodeRef{
        self.region.node
    }

    pub fn get_cv_for_eval(
        &mut self, 
        eval_expr: &EvalExpr
    ) -> Result<(OutportLocation, Ty), OptError>{
        if !self.is_eval_allowed{
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
    pub(crate) fn build_block(
        mut self,
        stmts: Vec<FetchStmt>,
        ret: ReturnExpr,
    ) -> Result<(), OptError>{
        for stmt in stmts{
            match stmt{
                FetchStmt::CSGBind(csgbind) => self.setup_csg_binding(csgbind)?,
                FetchStmt::Let(letstmt) => self.setup_let(letstmt)?,
                FetchStmt::Assign(assign) => self.setup_assign(assign)?,
            }
        }

        match ret{
            ReturnExpr::AccessDescriptors(ad) => {
                for access in ad{
                    self.wire_access(access)?
                }
            },
            ReturnExpr::Expr(expr) => {
                self.wire_return_expr(expr)?;
            }
        }

        Ok(())
    }

    fn wire_return_expr(&mut self, expr: AlgeExpr) -> Result<(), OptError>{
        //at the end we simply encue the last expression and hook that up to our result
        let last_output = self.setup_alge_expr(expr)?;
        let result_index = self
            .opt
            .graph
            .node_mut(self.parent_node())
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        let result_edge = self
            .opt
            .graph
            .on_region(&self.region, |reg| {
                reg.connect_to_result(last_output, rvsdg::edge::InputType::Result(result_index))
                    .unwrap()
            })
            .unwrap();

        //finally, set the result edge with the known result_type of the alge_fn
        assert!(self.return_type.len() == 1, "Expected single return type to be set already!");
        self
            .opt
            .graph
            .edge_mut(result_edge)
            .ty
            .set_type(self.return_type[0].clone());
        Ok(())
    }

    fn wire_access(&mut self, access: AccessDesc) -> Result<(), OptError>{
        
        //check if we can find the tree.
        let field_src = match self.lmd_ctx.defined_vars.get(&access.tree_ref.0) {
            Some(field_src) => field_src,
            None => {
                return Err(OptError::report_variable_not_found(
                    &access.span,
                    &access.tree_ref.0,
                ));
            }
        };

        let concept = match self.opt.concepts.get(&access.call.ident.0) {
            None => {
                return Err(OptError::report_no_concept(
                    &access.span,
                    &access.call.ident.0,
                ));
            }
            Some(con) => con,
        };

        //While at it, make sure the call-parameter-count matches
        if concept.src_ty.len() != access.call.args.len() {
            return Err(OptError::report_argument_missmatch(
                &concept.span,
                concept.src_ty.len(),
                &access.span,
                access.call.args.len(),
            ));
        }

        let concept_name = concept.name.clone();

        //We use the concept's return type to infer the output signature.
        let return_type: Ty = concept.dst_ty.clone().into();

        //register in output signature
        let expected_output_idx = self.return_type.len();
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
        for arg in access.call.args {
            let arg_port = self.setup_alge_expr(arg)?;
            wires.push(arg_port);
        }

        //add an result port to the lambda node
        let resultidx = self
            .opt
            .graph
            .node_mut(self.parent_node())
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        assert!(resultidx == expected_output_idx);
        let _access_output = self
            .opt
            .graph
            .on_region(&self.region, |reg| {
                let (node, _) = reg
                    .connect_node(
                        OptNode::new(
                            TreeAccess::new(concept_name, signature, return_type),
                            access.span.clone(),
                        ),
                        &wires,
                    )
                    .unwrap();

                let _ = reg
                    .connect_to_result(node.output(0), InputType::Result(resultidx))
                    .unwrap();
                node.output(0)
            })
            .unwrap();

        Ok(())
    }
    
}
