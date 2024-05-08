/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use vola_ast::csg::CsgStmt;
use vola_common::{ariadne::Label, error::error_reporter, report};

use crate::{
    common::{Ty, VarDef},
    OptError,
};

use super::BlockBuilder;

impl<'a> BlockBuilder<'a> {
    pub(crate) fn setup_let(&mut self, let_stmt: vola_ast::alge::LetStmt) -> Result<(), OptError> {
        //for a let stmt we have to define the new variable _after_ we parsed the rhs expression.

        let vola_ast::alge::LetStmt {
            span,
            decl_name,
            expr,
        } = let_stmt;

        if self.lmd_ctx.var_exists(&decl_name.0) {
            let existing = self.lmd_ctx.defined_vars.get(&decl_name.0).unwrap();
            let err = OptError::Any {
                text: format!("
cannot redefine variable with name \"{}\".
Note that vola does not support shadowing. If you just want to change the value of that variable, consider doing it like this:
`{} = ...;`",
                              decl_name.0, decl_name.0),
            };
            report(
                error_reporter(err.clone(), span.clone())
                    .with_label(
                        Label::new(existing.span.clone()).with_message("first defined here"),
                    )
                    .with_label(Label::new(span.clone()).with_message("redefined here"))
                    .finish(),
            );
            return Err(err);
        }

        let def_port = self.setup_alge_expr(expr)?;

        //register in the lmd context
        self.lmd_ctx.add_define(
            decl_name.0,
            VarDef {
                port: def_port,
                span,
            },
        );

        Ok(())
    }

    pub(crate) fn setup_assign(
        &mut self,
        assignstmt: vola_ast::alge::AssignStmt,
    ) -> Result<(), OptError> {
        let vola_ast::alge::AssignStmt { span, dst, expr } = assignstmt;

        //Assign stmt, similar to the let stmt works, by setting up the expr on the left hand site, but
        // then overwriting the last known definition of dst.

        if !self.lmd_ctx.var_exists(&dst.0) {
            let err = OptError::Any {
                text: format!(
                    "
Cannot assign to an undefined variable {}.
Consider using `let {} = ...;` instead, or using an defined variable.
",
                    dst.0, dst.0
                ),
            };

            report(
                error_reporter(err.clone(), span.clone())
                    .with_label(Label::new(span.clone()).with_message("Unknown variable"))
                    .finish(),
            );
            return Err(err);
        }

        log::warn!("Reassignment does not check for correctly typed reassignment yet!");

        //build the sub tree and overwrite the last_def output
        let sub_tree_output = self.setup_alge_expr(expr)?;
        let last_def = self.lmd_ctx.defined_vars.get_mut(&dst.0).unwrap();
        last_def.port = sub_tree_output;
        Ok(())
    }

    pub(crate) fn setup_csg_binding(&mut self, binding: CsgStmt) -> Result<(), OptError> {
        let CsgStmt {
            span,
            decl_name,
            expr,
        } = binding;

        //Similar to let statements, make sure that no variable exists with the given name.
        // If so, build the csg tree

        if self.lmd_ctx.var_exists(&decl_name.0) {
            let existing = self.lmd_ctx.defined_vars.get(&decl_name.0).unwrap();
            let err = OptError::Any {
                text: format!("
cannot redefine variable with name \"{}\".
Note that vola does not support shadowing. If you just want to change the value of that variable, consider doing it like this:
`{} = ...;`",
                              decl_name.0, decl_name.0),
                      };
            report(
                error_reporter(err.clone(), span.clone())
                    .with_label(
                        Label::new(existing.span.clone()).with_message("first defined here"),
                    )
                    .with_label(Label::new(span.clone()).with_message("tried to redefined here"))
                    .finish(),
            );
            return Err(err);
        }

        let def_port = self.setup_csg_tree(expr)?;

        //register in the lmd context
        self.lmd_ctx.add_define(
            decl_name.0,
            VarDef {
                port: def_port,
                span,
            },
        );

        //register type for port
        self.opt.typemap.set(def_port.into(), Ty::CSGTree);

        Ok(())
    }
}
