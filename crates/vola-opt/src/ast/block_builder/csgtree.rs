/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{edge::OutportLocation, SmallColl};
use vola_ast::csg::CSGNodeTy;
use vola_common::{ariadne::Label, error::error_reporter, report};

use crate::{common::Ty, csg::CsgOp, OptError, OptNode};

use super::BlockBuilder;

impl<'a> BlockBuilder<'a> {
    pub fn setup_csg_tree(
        &mut self,
        tree: vola_ast::csg::CSGOp,
    ) -> Result<OutportLocation, OptError> {
        //Based on the number of sub trees, decide which CSG Node to build.
        // There are largely three categories.
        //
        // 1. an op with 1..n sub trees
        // 2. an entity with no subtree
        // 3. a csg reference with no subtree. (set within the scope)
        // 4. a call to a field definition

        let (opnode, span) = match tree.sub_trees.len() {
            0 => {
                //Uses a local csg reference. Therefore check if we have that, and if so, inline
                // it here
                if tree.is_local_reference {
                    if let Some(localcsg) = self.lmd_ctx.defined_vars.get(&tree.op.0) {
                        //Make sure the local variable is a csg variable
                        let err = OptError::Any {
                            text: format!(
                                "Using local variable in CSG tree, but was not a csg-tree variable"
                            ),
                        };
                        if let Some(localcsgty) = self.opt.typemap.get(&localcsg.port.into()) {
                            if localcsgty != &Ty::CSGTree {
                                report(
                                    error_reporter(err.clone(), tree.span.clone())
                                        .with_label(
                                            Label::new(localcsg.span.clone())
                                                .with_message("defined here"),
                                        )
                                        .with_label(
                                            Label::new(tree.head_span().clone())
                                                .with_message("used here"),
                                        )
                                        .finish(),
                                );
                                return Err(err);
                            }
                        } else {
                            report(
                                error_reporter(err.clone(), tree.span.clone())
                                    .with_label(
                                        Label::new(localcsg.span.clone())
                                            .with_message("defined here"),
                                    )
                                    .with_label(
                                        Label::new(tree.head_span().clone())
                                            .with_message("used here"),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }

                        //If found, return that port
                        return Ok(localcsg.port);
                    } else {
                        //Throw an error
                        return Err(OptError::report_variable_not_found(&tree.span, &tree.op.0));
                    }
                } else {
                    //Must be an entity call, or a field call. Try to find an entity with that name, and make
                    // sure the parameter count matches the entity

                    if let Some(csgdef) = self.opt.csg_node_defs.get(&tree.op.0) {
                        if csgdef.ty != CSGNodeTy::Entity {
                            //Is not an entity definition
                            let err = OptError::Any {
                                text: format!("{} is not an entity", tree.op.0),
                            };

                            report(
                                error_reporter(err.clone(), tree.span.clone())
                                    .with_label(
                                        Label::new(csgdef.span.clone())
                                            .with_message("Defined as operation here"),
                                    )
                                    .with_label(Label::new(tree.head_span().clone()).with_message(
                                        "Used in the place of an entity, or local field def here",
                                    ))
                                    .finish(),
                            );
                            return Err(err);
                        }

                        if csgdef.args.len() != tree.args.len() {
                            return Err(OptError::report_argument_missmatch(
                                &csgdef.span,
                                csgdef.args.len(),
                                &tree.head_span(),
                                tree.args.len(),
                            ));
                        }

                        //looks good, construct the CSGOp
                        (
                            CsgOp::new(csgdef.name.clone(), 0, tree.args.len()),
                            tree.span.clone(),
                        )
                    } else {
                        //Is not defined as csg_op, so try to find a field def with that name

                        if let Some(fdef) = self.opt.field_def.get(&tree.op.0).cloned() {
                            //In the case that we have an field definition, import the field definition as
                            //context variable, and call it

                            if fdef.input_signature.len() != tree.args.len() {
                                return Err(OptError::report_argument_missmatch(
                                    &fdef.span,
                                    fdef.input_signature.len(),
                                    &tree.head_span(),
                                    tree.args.len(),
                                ));
                            }

                            //build the apply node and early return
                            let mut args: SmallColl<OutportLocation> = SmallColl::new();
                            for arg in tree.args {
                                args.push(self.setup_alge_expr(arg)?);
                            }
                            let (call_node, _) = self
                                .opt
                                .graph
                                .on_region(&self.region, |reg| {
                                    let imported = reg
                                        .import_context(OutportLocation {
                                            node: fdef.lambda,
                                            output: rvsdg::edge::OutputType::LambdaDeclaration,
                                        })
                                        .unwrap();
                                    reg.call(imported, &args).unwrap()
                                })
                                .unwrap();
                            //NOTE: We tag the apply node with a span, since the apply-node itself has no span
                            //associated, but in this case we have an actual source-level location.
                            self.opt.span_tags.set(call_node.into(), tree.span.clone());
                            return Ok(call_node.output(0));
                        } else {
                            let err = OptError::Any {
                                text: format!(
                                    "{} does not name an entity, local csg variable or field def",
                                    tree.op.0
                                ),
                            };
                            report(
                                error_reporter(err.clone(), tree.span.clone())
                                    .with_label(
                                        Label::new(tree.head_span().clone())
                                            .with_message(
                                                "try defining a local variable, an entity or a field with that name"
                                            ),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    }
                }
            }
            subcount => {
                if let Some(opdef) = self.opt.csg_node_defs.get(&tree.op.0) {
                    if opdef.ty == CSGNodeTy::Operation {
                        //found an operation with that name. So make sure the parameter count matches
                        if opdef.args.len() != tree.args.len() {
                            return Err(OptError::report_argument_missmatch(
                                &opdef.span,
                                opdef.args.len(),
                                &tree.head_span(),
                                tree.args.len(),
                            ));
                        }

                        //Aight, looks good, using that.
                        (
                            CsgOp::new(opdef.name.clone(), subcount, tree.args.len() + subcount),
                            tree.span.clone(),
                        )
                    } else {
                        let err = OptError::Any {
                            text: format!("Could not find operation named \"{}\"", tree.op.0),
                        };
                        report(
                            error_reporter(err.clone(), tree.span.clone())
                                .with_label(
                                    Label::new(tree.head_span().clone())
                                        .with_message("Consider adding such an operation"),
                                )
                                .finish(),
                        );
                        return Err(err);
                    }
                } else {
                    return Err(OptError::report_variable_not_found(
                        &tree.head_span(),
                        &tree.op.0,
                    ));
                }
            }
        };

        let mut args: SmallColl<OutportLocation> = SmallColl::new();
        for subtree in tree.sub_trees {
            args.push(self.setup_csg_tree(subtree)?);
        }

        //then build all argument expressions
        for arg in tree.args {
            args.push(self.setup_alge_expr(arg)?);
        }

        //finally setup the node an connect all sub-trees and argumets to it
        let opnode = self
            .opt
            .graph
            .on_region(&self.region, |regbuilder| {
                let (opnode, _) = regbuilder
                    .connect_node(OptNode::new(opnode, span), &args)
                    .unwrap();
                opnode.output(0)
            })
            .unwrap();
        Ok(opnode)
    }
}
