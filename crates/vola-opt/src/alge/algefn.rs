/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{
    edge::{OutportLocation, OutputType},
    region::RegionLocation,
    smallvec::SmallVec,
    NodeRef,
};
use vola_ast::{
    alge::{AlgeStmt, AssignStmt, LetStmt},
    common::Ident,
};
use vola_common::{report, Span};

use crate::{
    alge::WkOp,
    ast::{AstLambdaBuilder, LambdaBuilderCtx},
    common::{LmdContext, Ty, VarDef},
    OptError, Optimizer,
};

pub struct AlgeFn {
    pub span: Span,
    pub name: Ident,
    pub args: SmallVec<[(String, Ty); 3]>,
    pub retty: Ty,

    pub lambda: NodeRef,
    pub lambda_region: RegionLocation,
}

impl LambdaBuilderCtx for AlgeFn {}

impl AlgeFn {
    fn build_block(
        mut self,
        mut lmd_builder: AstLambdaBuilder,
        alge_fn: vola_ast::alge::AlgeFunc,
    ) -> Result<Self, OptError> {
        for stmt in alge_fn.stmts {
            match stmt {
                AlgeStmt::Assign(assignstmt) => self.setup_assign(&mut lmd_builder, assignstmt)?,
                AlgeStmt::Let(letstmt) => self.setup_let(&mut lmd_builder, letstmt)?,
            }
        }

        //at the end we simply encue the last expression and hook that up to our result
        let last_output = lmd_builder.setup_alge_expr(alge_fn.return_expr, &mut self)?;
        let result_index = lmd_builder
            .opt
            .graph
            .node_mut(self.lambda)
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        let result_edge = lmd_builder
            .opt
            .graph
            .on_region(&self.lambda_region, |reg| {
                reg.connect_to_result(last_output, rvsdg::edge::InputType::Result(result_index))
                    .unwrap()
            })
            .unwrap();

        //finally, set the result edge with the known result_type of the alge_fn
        lmd_builder
            .opt
            .graph
            .edge_mut(result_edge)
            .ty
            .set_type(alge_fn.return_type.into());
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
    pub fn add_alge_fn(&mut self, alge_fn: vola_ast::alge::AlgeFunc) -> Result<NodeRef, OptError> {
        //The alge fn is conceptually similar to the field def.
        //however, we only allow alge expressions and statements. No Eval can be used
        //and the return expression must statify the return-type constrains (at some point ^^).

        if let Some(_special_node) = WkOp::try_parse(&alge_fn.name.0) {
            let err = OptError::AnySpanned { span: alge_fn.span.clone().into(), text: format!("Cannot use name \"{}\" for algebraic function, since a that would shadow a build-in function with the same name!", alge_fn.name.0), span_text: format!("Consider renaming this function.") };

            report(err.clone(), alge_fn.span.get_file());
            return Err(err);
        }

        if let Some(existing_key) = self.alge_fn.get(&alge_fn.name.0) {
            let err = OptError::AnySpannedWithSource {
                source_span: existing_key.span.clone().into(),
                source_text: format!("First implementation of \"{}\" here", alge_fn.name.0),
                text: format!("Cannot define \"{}\" multiple times!", alge_fn.name.0),
                span: alge_fn.span.into(),
                span_text: format!("Tried to redefine here"),
            };
            report(err.clone(), existing_key.span.get_file());
            return Err(err);
        }

        let (lambda, lambda_region) = self.graph.on_omega_node(|omg| {
            let export_fn = std::env::var("VOLA_EXPORT_ALL").is_ok();
            omg.new_function(export_fn, |lmd_buider| {
                lmd_buider.on_region(|reg| reg.parent_location())
            })
        });

        let args = alge_fn
            .args
            .iter()
            .map(|arg| (arg.ident.0.clone(), Ty::from(arg.ty.clone())))
            .collect();

        let lmd_context =
            LmdContext::new_for_alge_fn(&mut self.graph, &mut self.typemap, lambda, &alge_fn);

        let lmd_builder = AstLambdaBuilder {
            opt: self,
            lmd_context,
            lambda,
            lambda_region,
            is_eval_allowed: false,
        };

        let algedef = AlgeFn {
            span: alge_fn.span.clone(),
            name: alge_fn.name.clone().into(),
            args,
            retty: alge_fn.return_type.clone().into(),
            lambda,
            lambda_region,
        };

        let algedef = algedef.build_block(lmd_builder, alge_fn)?;
        let algename = algedef.name.0.clone();
        self.names.set(algedef.lambda.into(), algename.clone());
        self.span_tags
            .set(algedef.lambda.into(), algedef.span.clone());

        //TODO: set the out-edge type?
        assert!(self.alge_fn.insert(algename, algedef).is_none());
        Ok(lambda)
    }
}
