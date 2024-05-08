/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::{edge::OutportLocation, smallvec::smallvec, SmallColl};
use vola_ast::{
    alge::{Expr, ExprTy},
    common::Block,
    csg::CSGNodeTy,
};
use vola_common::{ariadne::Label, error::error_reporter, report};

use crate::{common::Ty, csg::CsgOp, OptError, OptNode};

use super::{BlockBuilder, BlockBuilderConfig};

impl<'a> BlockBuilder<'a> {
    ///Builds a block, which lifes _within_ `self`, so same region, same, lambda context etc.
    ///
    ///but regardless is expected to return a CSGTree.
    fn build_csg_block(&mut self, block: Block) -> Result<OutportLocation, OptError> {
        //Always expect a CSG
        let config = BlockBuilderConfig {
            expect_return: super::RetType::CsgTree,
            ..self.config.clone()
        };
        let mut local_block_builder =
            self.inherite_ctx(block.span.clone(), config, smallvec![Ty::CSGTree]);

        //now build the block and safe the outport of the created CSGTree
        let out = local_block_builder.build_block(block)?;
        assert!(out.len() == 1);

        //before returning, destroy the local block builder, and merge the lmd_context
        //this allows `self` to register if the inner-block imported new functions
        let destroyed_ctx = local_block_builder.destroy_inherited();
        self.lmd_ctx.merge_inner_ctx(destroyed_ctx);

        Ok(out[0].1)
    }

    pub fn setup_csg_tree(&mut self, tree: Expr) -> Result<OutportLocation, OptError> {
        //There are three variants we have to expect.
        //
        //1. A simple call, this will just add add a CsgOp without any subtrees.
        //2. scoped-call:
        //   In that case we setup a operation, defined by the call's identifier and supplied with the
        //   calls arguments.
        //   we then start a new block for each scope, that is expected to return a
        //   csg-op. The new scope inherits all defined variables from the current lambda builder, but is free
        //   to add their own within the scope. Those won't be exported afterwards.

        match tree.expr_ty {
            ExprTy::Ident(i) => {
                //check if we have a csg-tree with that name, if so, inline it at this point.
                //by returning the port
                if let Some(localcsg) = self.lmd_ctx.defined_vars.get(&i.0) {
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
                                        Label::new(localcsg.span.clone()).with_message(&format!(
                                            "defined here as {:?}",
                                            localcsgty
                                        )),
                                    )
                                    .with_label(
                                        Label::new(tree.span.clone()).with_message("used here"),
                                    )
                                    .finish(),
                            );
                            return Err(err);
                        }
                    } else {
                        report(
                            error_reporter(err.clone(), tree.span.clone())
                                .with_label(
                                    Label::new(localcsg.span.clone()).with_message("defined here"),
                                )
                                .with_label(Label::new(tree.span.clone()).with_message("used here"))
                                .finish(),
                        );
                        return Err(err);
                    }

                    //If found, return that port
                    Ok(localcsg.port)
                } else {
                    //Throw an error
                    Err(OptError::report_variable_not_found(&tree.span, &i.0))
                }
            }
            ExprTy::Call(c) => {
                //Must be an entity call, or a field call. Try to find an entity with that name, and make
                // sure the parameter count matches the entity
                if let Some(csgdef) = self.opt.csg_node_defs.get(&c.ident.0) {
                    if csgdef.ty != CSGNodeTy::Entity {
                        //Is not an entity definition
                        let err = OptError::Any {
                            text: format!("{} is not an entity", c.ident.0),
                        };

                        report(
                            error_reporter(err.clone(), c.span.clone())
                                .with_label(
                                    Label::new(csgdef.span.clone())
                                        .with_message("Defined as operation here"),
                                )
                                .with_label(Label::new(c.span.clone()).with_message(
                                    "Used in the place of an entity, or local field def here",
                                ))
                                .finish(),
                        );
                        return Err(err);
                    }

                    if csgdef.args.len() != c.args.len() {
                        return Err(OptError::report_argument_missmatch(
                            &csgdef.span,
                            csgdef.args.len(),
                            &c.span,
                            c.args.len(),
                        ));
                    }

                    let opnode = CsgOp::new(csgdef.name.clone(), 0, c.args.len());

                    let mut args: SmallColl<OutportLocation> = SmallColl::new();
                    //then build all argument expressions
                    for arg in c.args {
                        args.push(self.setup_alge_expr(arg)?);
                    }

                    //finally setup the node an connect all sub-trees and argumets to it
                    let opnode = self
                        .opt
                        .graph
                        .on_region(&self.region, |regbuilder| {
                            let (opnode, _) = regbuilder
                                .connect_node(OptNode::new(opnode, c.span.clone()), &args)
                                .unwrap();
                            opnode.output(0)
                        })
                        .unwrap();
                    Ok(opnode)
                } else {
                    //Is not defined as csg_op, so try to find a field def with that name

                    if let Some(fdef) = self.opt.field_def.get(&c.ident.0).cloned() {
                        //In the case that we have an field definition, import the field definition as
                        //context variable, and call it

                        if fdef.input_signature.len() != c.args.len() {
                            return Err(OptError::report_argument_missmatch(
                                &fdef.span,
                                fdef.input_signature.len(),
                                &c.span,
                                c.args.len(),
                            ));
                        }

                        //build the apply node and early return
                        let mut args: SmallColl<OutportLocation> = SmallColl::new();
                        for arg in c.args {
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
                        Ok(call_node.output(0))
                    } else {
                        let err = OptError::Any {
                            text: format!(
                                "Expression does not name an entity, local csg variable or field def",
                            ),
                        };
                        report(
                                error_reporter(err.clone(), tree.span.clone())
                                    .with_label(
                                        Label::new(tree.span.clone())
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
            ExprTy::ScopedCall(sc) => {
                let subcount = sc.blocks.len();

                if let Some(opdef) = self.opt.csg_node_defs.get(&sc.call.ident.0) {
                    if opdef.ty == CSGNodeTy::Operation {
                        //found an operation with that name. So make sure the parameter count matches
                        if opdef.args.len() != sc.call.args.len() {
                            return Err(OptError::report_argument_missmatch(
                                &opdef.span,
                                opdef.args.len(),
                                &sc.span,
                                sc.call.args.len(),
                            ));
                        }

                        let opnode =
                            CsgOp::new(opdef.name.clone(), subcount, sc.call.args.len() + subcount);

                        let mut args: SmallColl<OutportLocation> = SmallColl::new();
                        for subtree in sc.blocks {
                            //setup the new tree by _opening_ a new scope that inherits
                            //the current lambda and build that fully
                            let sub_build = self.build_csg_block(subtree)?;
                            args.push(sub_build);
                        }

                        //then build all argument expressions
                        for arg in sc.call.args {
                            args.push(self.setup_alge_expr(arg)?);
                        }

                        //finally setup the node an connect all sub-trees and argumets to it
                        let opnode = self
                            .opt
                            .graph
                            .on_region(&self.region, |regbuilder| {
                                let (opnode, _) = regbuilder
                                    .connect_node(OptNode::new(opnode, sc.call.span.clone()), &args)
                                    .unwrap();
                                opnode.output(0)
                            })
                            .unwrap();
                        Ok(opnode)
                    } else {
                        let err = OptError::Any {
                            text: format!("Could not find operation named \"{}\"", sc.call.ident.0),
                        };
                        report(
                            error_reporter(err.clone(), tree.span.clone())
                                .with_label(
                                    Label::new(sc.span.clone())
                                        .with_message("Consider adding such an operation"),
                                )
                                .finish(),
                        );
                        return Err(err);
                    }
                } else {
                    return Err(OptError::report_variable_not_found(
                        &sc.span.clone(),
                        &sc.call.ident.0,
                    ));
                }
            }
            _ => {
                //all others are _not expected_
                let err = OptError::Any {
                    text: format!(
                        "Unexpected expression type, does not name an entity, local csg variable or field def",
                    ),
                };
                report(
                    error_reporter(err.clone(), tree.span.clone())
                        .with_label(Label::new(tree.span.clone()).with_message(
                            "try defining a local variable, an entity or a field with that name",
                        ))
                        .finish(),
                );
                return Err(err);
            }
        }
    }
}
