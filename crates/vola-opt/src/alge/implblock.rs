use std::borrow::BorrowMut;

/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::{
    ast::{AstLambdaBuilder, LambdaBuilderCtx},
    common::{LmdContext, Ty, VarDef},
    error::OptError,
    Optimizer,
};
use ahash::AHashMap;
use rvsdg::{
    edge::{InputType, OutportLocation, OutputType},
    region::RegionLocation,
    NodeRef, SmallColl,
};
use vola_ast::{
    alge::{AlgeStmt, AssignStmt, EvalExpr, ImplBlock, LetStmt},
    common::Ident,
    csg::CSGNodeTy,
};
use vola_common::{report, Span};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ConceptImplKey {
    pub concept_name: String,
    ///Name of either a entity or operation definition.
    pub node_name: String,
}

///Contains the meta data of a concept implementation
pub struct ConceptImpl {
    pub span: Span,
    pub concept: Ident,
    #[allow(unused)]
    pub node_type: CSGNodeTy,

    ///All named operands an their index.
    pub operands: AHashMap<String, usize>,

    ///The λ-Node of this concept
    pub lambda: NodeRef,
    ///Shortcut to the λ-Node's inner region.
    pub lambda_region: RegionLocation,
}

impl LambdaBuilderCtx for ConceptImpl {
    fn get_cv_for_eval(
        &mut self,
        builder: &mut AstLambdaBuilder,
        eval_expr: &EvalExpr,
    ) -> Result<(OutportLocation, Ty), OptError> {
        //Before doing anything, check that the concept exists and has the right ammount of
        // arguments
        let concept = match builder.opt.concepts.get(&eval_expr.concept.0) {
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
        if !self.operands.contains_key(&eval_expr.evaluator.0) {
            return Err(OptError::Any {
                text: format!("Operand {} does not exist", eval_expr.evaluator.0),
            });
        }
        let cv = *self.operands.get(&eval_expr.evaluator.0).unwrap();

        let port = OutportLocation {
            node: builder.lambda,
            output: OutputType::ContextVariableArgument(cv),
        };

        Ok((port, concept_ret_type))
    }
}

impl ConceptImpl {
    fn build_block(
        mut self,
        mut builder: AstLambdaBuilder,
        block: ImplBlock,
    ) -> Result<Self, OptError> {
        let ImplBlock {
            span: _,
            dst: _,
            operands: _,
            concept: _,
            concept_arg_naming: _,
            stmts,
            return_expr,
        } = block;

        for stmt in stmts {
            match stmt {
                AlgeStmt::Assign(assign) => self.setup_assign(&mut builder, assign)?,
                AlgeStmt::Let(letstmt) => self.setup_let(&mut builder, letstmt)?,
            }
        }

        //after setting up stmts in order, build the port of the final expr and connect that to the output of our
        // concept impl
        let return_expr_port = builder.setup_alge_expr(return_expr, &mut self)?;
        //add the output port and connect
        let result_port = builder
            .opt
            .graph
            .node_mut(builder.lambda)
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        assert!(
            result_port == 0,
            "Result port index of concept impl should be 0 (only one result possible)"
        );

        //now connect to it
        builder.opt.graph.on_region(&builder.lambda_region, |reg| {
            reg.connect_to_result(return_expr_port, InputType::Result(result_port))
                .expect("Could not connect to result!")
        });

        Ok(self)
    }

    fn setup_assign(
        &mut self,
        builder: &mut AstLambdaBuilder,
        assignstmt: AssignStmt,
    ) -> Result<(), OptError> {
        let AssignStmt { span, dst, expr } = assignstmt;

        //Assign stmt, similar to the let stmt works, by setting up the expr on the left hand site, but
        // then overwriting the last known definition of dst.

        if !builder.lmd_context.var_exists(&dst.0) {
            let err = OptError::AnySpanned {
                span: span.clone().into(),
                text: format!(
                    "
Cannot assign to an undefined variable {}.
Consider using `let {} = ...;` instead, or using an defined variable.
",
                    dst.0, dst.0
                ),
                span_text: "Unknown variable".to_owned(),
            };

            report(err.clone(), span.get_file());
            return Err(err);
        }

        //build the sub tree and overwrite the last_def output

        let sub_tree_output = builder.setup_alge_expr(expr, self)?;
        let last_def = builder.lmd_context.defined_vars.get_mut(&dst.0).unwrap();
        last_def.port = sub_tree_output;
        Ok(())
    }

    fn setup_let(
        &mut self,
        builder: &mut AstLambdaBuilder,
        let_stmt: LetStmt,
    ) -> Result<(), OptError> {
        //for a let stmt we have to define the new variable _after_ we parsed the rhs expression.

        let LetStmt {
            span,
            decl_name,
            expr,
        } = let_stmt;

        if builder.lmd_context.var_exists(&decl_name.0) {
            let existing = builder.lmd_context.defined_vars.get(&decl_name.0).unwrap();
            let err = OptError::AnySpannedWithSource {
                source_span: existing.span.clone().into(),
                source_text: "first defined here".to_owned(),
                text: format!("
cannot redefine variable with name \"{}\".
Note that vola does not support shadowing. If you just want to change the value of that variable, consider doing it like this:
`{} = ...;`",
                              decl_name.0, decl_name.0),
                span: span.clone().into(),
                span_text: "tried to redefine here".to_owned() };
            report(err.clone(), span.get_file());
            return Err(err);
        }

        let def_port = builder.setup_alge_expr(expr, self)?;

        //register in the lmd context
        builder.lmd_context.add_define(
            decl_name.0,
            VarDef {
                port: def_port,
                span,
            },
        );

        Ok(())
    }
}

impl Optimizer {
    ///Adds this implementation block to the optimizer. Takes care of all the initial legalizing as well.
    pub fn add_impl_block(&mut self, implblock: ImplBlock) -> Result<NodeRef, OptError> {
        //First, check if an implementation for that concept and node already exists
        let key = ConceptImplKey {
            concept_name: implblock.concept.0.clone(),
            node_name: implblock.dst.0.clone(),
        };

        //Check that the impl block doesn't yet exist
        if let Some(existing_key) = self.concept_impl.get(&key) {
            let err = OptError::AnySpannedWithSource {
                source_span: existing_key.span.clone().into(),
                source_text: format!(
                    "first impl of concept {} for node {}",
                    implblock.concept.0, implblock.dst.0
                ),
                text: format!(
                    "Tried to re-implement the concept {} for the entity or operation {}.",
                    implblock.concept.0, implblock.dst.0
                ),
                span: implblock.span.into(),
                span_text: format!("Second implementation here"),
            };

            report(err.clone(), existing_key.span.get_file());
            return Err(err);
        }

        //Now try to retrieve the actual definition of the concept, and check that we are abiding to its definition
        let src_concept = if let Some(src_concept) = self.concepts.get(&implblock.concept.0) {
            src_concept
        } else {
            //Could not find the source concept, bail!
            let err = OptError::AnySpanned {
                span: implblock.span.clone().into(),
                text: format!("Concept {} is undefined!", implblock.concept.0),
                span_text: format!("Consider defining the concept \"{}\"", implblock.concept.0),
            };

            report(err.clone(), implblock.span.get_file());
            return Err(err);
        };

        let src_csg_def = if let Some(src_csg_def) = self.csg_node_defs.get(&implblock.dst.0) {
            src_csg_def.clone()
        } else {
            let err = OptError::AnySpanned {
                span: implblock.span.clone().into(),
                text: format!(
                    "csg-node-type (entity or operation) {} is undefined!",
                    implblock.concept.0
                ),
                span_text: format!(
                    "Consider defining an entity or operation named \"{}\"",
                    implblock.concept.0
                ),
            };

            report(err.clone(), implblock.span.get_file());
            return Err(err);
        };

        //Check that the amount of arguments mirror the concept's argument count.
        if src_concept.src_ty.len() != implblock.concept_arg_naming.len() {
            let err = OptError::AnySpannedWithSource {
                source_span: src_concept.span.clone().into(),
                source_text: format!("Src concept defines {} arguments", src_concept.src_ty.len()),
                text: format!(
                    "argument definition has {} arguments",
                    src_concept.src_ty.len(),
                ),
                span: implblock.span.into(),
                span_text: format!(
                    "this should take {} arguments, not {}",
                    src_concept.src_ty.len(),
                    implblock.concept_arg_naming.len()
                ),
            };
            report(err.clone(), src_concept.span.get_file());
            return Err(err);
        }

        //Make sure that an entity has no sub operands
        if src_csg_def.ty == CSGNodeTy::Entity && implblock.operands.len() > 0 {
            let err = OptError::AnySpanned {
                span: implblock.span.clone().into(),
                text: format!(
                    "entity {} cannot have CSG operands. Only operations can have CSG operands!",
                    implblock.dst.0
                ),
                span_text: format!(
                    "Consider removing the operands, or implementing an \"operation\" instead"
                ),
            };
            report(err.clone(), implblock.span.get_file());
            return Err(err);
        }

        //prebuild the concept_key so we can drop our src_concept references early.
        let concept_key = ConceptImplKey {
            concept_name: src_concept.name.0.clone(),
            node_name: src_csg_def.name.0.clone(),
        };

        //Build the operand->index map
        let operands = implblock
            .operands
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.0.clone(), idx))
            .collect();

        //At this point we should have verified the overall signature. We therefore start building the actual λ-Node.
        // We do this by building the function-context helper that contains all concept-args as defined variables, as well as the
        // operation variables.

        //Create the lambda node. NOTE that we currently alway export. For debug purposes.
        let (lmd, lmd_region) = self.graph.on_omega_node(|omg| {
            let export_impl_block = std::env::var("VOLA_EXPORT_ALL").is_ok();
            omg.new_function(export_impl_block, |lmd_builder| {
                lmd_builder.on_region(|reg| reg.parent_location())
            })
        });

        //register the context ports in the same order as the operands map was build
        for (idx, name) in implblock.operands.iter().enumerate() {
            let lmdcvidx = self
                .graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_context_variable();
            assert!(idx == lmdcvidx);
            //Preset the CVs with the CSGTree type. Since we don't known the exact node at the moment
            self.typemap.set(
                OutportLocation {
                    node: lmd,
                    output: OutputType::ContextVariableArgument(lmdcvidx),
                }
                .into(),
                Ty::CSGTree,
            );
        }

        let lmd_context = LmdContext::new_for_impl_block(
            &mut self.graph,
            &mut self.typemap,
            lmd.clone(),
            &implblock,
            &src_csg_def,
            &src_concept,
        );

        //This effectively builds us the type-signature of the impl block based on the calling-convention
        // that is realised in LmdContext::new_for_impl_block.
        // this means that first all csg-def args are supplied, followed by the args to the concept that is implemented.
        let impl_block_arg_signature = src_csg_def
            .args
            .iter()
            .map(|t| t.ty.clone())
            .chain(
                //NOTE: now chain the concept-args. This is we do that by indexing into the src_concept's definition
                implblock
                    .concept_arg_naming
                    .iter()
                    .enumerate()
                    .map(|(idx, _)| src_concept.src_ty[idx].clone()),
            )
            .collect::<SmallColl<_>>();
        let result_sig = src_concept.dst_ty.clone().into();
        //Temporary builder that tracks things like the defined variables etc.
        // Is dropped within the concept_impl.build_block()
        let lmd_builder = AstLambdaBuilder {
            opt: self,
            lmd_context,
            lambda: lmd,
            lambda_region: lmd_region,
        };

        //Now reverse the game by building the initial ConceptImpl and then letting it handle itself.
        let concept_impl = ConceptImpl {
            span: implblock.span.clone(),
            concept: implblock.concept.clone(),
            node_type: src_csg_def.ty.clone(),
            operands,

            lambda: lmd,
            lambda_region: lmd_region,
        };

        let build_concept_impl = concept_impl.build_block(lmd_builder, implblock)?;

        //After finishing (successfully) add the impl description to the Optimizer so we can
        // find that later on if needed and return
        //NOTE: _should be the same, but could also be changed at some point I guess_
        let lmd = build_concept_impl.lambda.clone();

        //now append types to edges based on impl-block signature, as well add some general debug info for later.
        self.names
            .set(lmd.into(), build_concept_impl.concept.0.clone());
        self.span_tags
            .set(lmd.into(), build_concept_impl.span.clone());

        //This iterator maps us the concept's argument types in order

        //now we chain first the csg-def's arg-types, followed by the concept args (which is our definition), to
        // setup the edge types accordingly
        for (inpidx, inputty) in impl_block_arg_signature.into_iter().enumerate() {
            for edg in self
                .graph
                .node(lmd)
                .node_type
                .unwrap_lambda_ref()
                .argument(inpidx)
                .unwrap()
                .edges
                .clone()
            {
                self.graph.edge_mut(edg).ty.set_type(inputty.clone().into());
            }
        }

        //now do the output. Thats a little easier, since we have, by definition just one output
        assert!(
            self.graph
                .node(lmd)
                .node_type
                .unwrap_lambda_ref()
                .result_count()
                == 1
        );
        self.graph
            .edge_mut(
                self.graph
                    .node(lmd)
                    .node_type
                    .unwrap_lambda_ref()
                    .result(0)
                    .unwrap()
                    .edge
                    .unwrap(),
            )
            .ty
            .set_type(result_sig);

        let old = self.concept_impl.insert(concept_key, build_concept_impl);
        assert!(old.is_none(), "Had an old concept + node combination already, should have been caught before adding it!");

        Ok(lmd)
    }
}
