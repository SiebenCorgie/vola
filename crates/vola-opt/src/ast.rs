/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Module that handles the opt-graph building based on [AST](vola-ast) nodes.

use rvsdg::{
    edge::{OutportLocation, OutputType},
    region::{Input, RegionLocation},
    smallvec::{smallvec, SmallVec},
    NodeRef,
};
use vola_ast::{
    alge::{AlgeExpr, AlgeExprTy, EvalExpr},
    csg::CSGNodeTy,
    AstEntry, TopLevelNode,
};
use vola_common::{ariadne::Label, error::error_reporter, report};

use crate::{
    alge::{CallOp, ConstantIndex, Construct, EvalNode, ImmNat, ImmScalar, WkOp},
    common::{FnImport, LmdContext, Ty},
    csg::CsgOp,
    error::OptError,
    OptNode, Optimizer, TypeState,
};

mod block_builder;

impl Optimizer {
    ///Adds the top-level node to the optimizer graph. If it applies, it returns a reference to the created node.
    pub fn add_tl_node(&mut self, tlnode: TopLevelNode) -> Result<Option<NodeRef>, OptError> {
        match tlnode.entry {
            //We ignore those atm
            AstEntry::Comment(_) => Ok(None),
            AstEntry::Module(m) => {
                let err = OptError::Any {
                    text: "Encountered \"module\" while transforming AST to RVSDG. Modules should be resolved beforehand.".to_owned()
                };
                report(
                    error_reporter(err.clone(), m.span.clone())
                        .with_label(
                            Label::new(m.span.clone()).with_message("This module was not resolved"),
                        )
                        .finish(),
                );
                Err(err)
            }
            AstEntry::Concept(csgcon) => {
                if let Some(existing_concept) = self.concepts.get(&csgcon.name.0) {
                    let err = OptError::Any {
                        text: format!("Concept {} was already defined", existing_concept.name.0),
                    };

                    report(
                        error_reporter(err.clone(), csgcon.span.clone())
                            .with_label(
                                Label::new(existing_concept.span.clone())
                                    .with_message("first defined here"),
                            )
                            .with_label(
                                Label::new(csgcon.span.clone()).with_message("redefined here"),
                            )
                            .finish(),
                    );

                    Err(err)
                } else {
                    //No yet in collection, therefore push
                    self.concepts.insert(csgcon.name.0.clone(), csgcon);
                    Ok(None)
                }
            }
            AstEntry::CSGNodeDef(csgnd) => {
                //similar to the concept case, test if there is already one, if not, push
                if let Some(existing_csg) = self.csg_node_defs.get(&csgnd.name.0) {
                    let err = OptError::Any {
                        text: format!("Operation or Entity {} was already defined. \nNote that operations and entities share one name space.", existing_csg.name.0),
                    };

                    report(
                        error_reporter(err.clone(), csgnd.span.clone())
                            .with_label(
                                Label::new(existing_csg.span.clone())
                                    .with_message("first defined here"),
                            )
                            .with_label(
                                Label::new(csgnd.span.clone()).with_message("redefined here"),
                            )
                            .finish(),
                    );

                    Err(err)
                } else {
                    //No yet in collection, therefore push
                    self.csg_node_defs.insert(csgnd.name.0.clone(), csgnd);
                    Ok(None)
                }
            }
            AstEntry::ImplBlock(implblock) => self.add_impl_block(implblock).map(|t| Some(t)),
            AstEntry::FieldDefine(fdef) => self.add_field_def(fdef).map(|t| Some(t)),
            AstEntry::ExportFn(expfn) => self.add_export_fn(expfn).map(|t| Some(t)),
            AstEntry::AlgeFunc(algefn) => self.add_alge_fn(algefn).map(|t| Some(t)),
        }
    }
}

///Helper that can take an AST statement or expression, and build the corresponding node setup in the RVSDG-Graph.
///
/// NOTE: If we'd have a non-structured language this would be the structurizer. But we are structured, we can
/// build the graph directly. See [the RVSDG paper](https://arxiv.org/pdf/1912.05036.pdf) 5.1 _Construction_.
pub struct AstLambdaBuilder<'a> {
    pub opt: &'a mut Optimizer,
    pub lambda: NodeRef,
    pub lambda_region: RegionLocation,
    pub lmd_context: LmdContext,
    pub is_eval_allowed: bool,
}

pub trait LambdaBuilderCtx {
    ///Registers the `eval_expr` in the lambda's context_variable set, and returns the Outport of said cv and the return type of the eval call.
    fn get_cv_for_eval(
        &mut self,
        _builder: &mut AstLambdaBuilder,
        eval_expr: &EvalExpr,
    ) -> Result<(OutportLocation, Ty), OptError> {
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
        Err(err)
    }
}

impl<'a> AstLambdaBuilder<'a> {
    //Sets up the alge expression, and if successful, return the output port that defines the final value.
    // TODO: Right now we don't have anything stateful, so a simple Outport location is enough. Hover, this might change whenever we introduce
    // buffer and image reads at a later stage.
    pub fn setup_alge_expr(
        &mut self,
        expr: AlgeExpr,
        parent: &mut impl LambdaBuilderCtx,
    ) -> Result<OutportLocation, OptError> {
        let expr_span = expr.op_span();
        match expr.expr_ty {
            AlgeExprTy::Unary { op, operand } => {
                //setup the unary node, than recurse, setup the subexpression and hook it up to our unary expression
                let sub_output = self.setup_alge_expr(*operand, parent)?;
                let opnode = self
                    .opt
                    .graph
                    .on_region(&self.lambda_region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(
                                OptNode::new(CallOp::new(op.into()), expr_span),
                                &[sub_output],
                            )
                            .unwrap();
                        //NOTE we _know_ that the node has only one output
                        opnode.output(0)
                    })
                    .unwrap();

                Ok(opnode)
            }
            AlgeExprTy::Binary { left, right, op } => {
                //Similar to the unary op, first parse both sub_trees, then hook them up to the
                // inputs and return the output.
                let left_out = self.setup_alge_expr(*left, parent)?;
                let right_out = self.setup_alge_expr(*right, parent)?;

                let opnode = self
                    .opt
                    .graph
                    .on_region(&self.lambda_region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(
                                OptNode::new(CallOp::new(op.into()), expr_span),
                                &[left_out, right_out],
                            )
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            AlgeExprTy::Call(c) => {
                //For the call node we try to parse the well known ops.
                //
                //If that doesn't work, we lookup the parsed functions.
                //if one of those has the identifier, import it as cv and
                //call that instead.

                //Build the call node with its sub args
                let mut subargs: SmallVec<[OutportLocation; 3]> = SmallVec::new();
                for arg in c.args {
                    let arg = self.setup_alge_expr(arg, parent)?;
                    subargs.push(arg);
                }
                if let Some(wknode) = WkOp::try_parse(&c.ident.0) {
                    let opnode = self
                        .opt
                        .graph
                        .on_region(&self.lambda_region, |regbuilder| {
                            let (opnode, _) = regbuilder
                                .connect_node(
                                    OptNode::new(CallOp::new(wknode), expr_span),
                                    &subargs,
                                )
                                .unwrap();
                            opnode.output(0)
                        })
                        .unwrap();

                    Ok(opnode)
                } else {
                    if let Some(algefn) = self.opt.alge_fn.get(&c.ident.0) {
                        let alge_import = if let Some(imported) =
                            self.lmd_context.known_function(&algefn.name.0)
                        {
                            imported
                        } else {
                            //import this function newly, add to context,
                            //then use it

                            let algeimportcv = self
                                .opt
                                .graph
                                .import_context(
                                    algefn
                                        .lambda
                                        .as_outport_location(OutputType::LambdaDeclaration),
                                    self.lambda_region,
                                )
                                .unwrap();

                            assert!(self
                                .lmd_context
                                .imported_functions
                                .insert(
                                    algefn.name.0.clone(),
                                    FnImport {
                                        port: algeimportcv,
                                        span: algefn.span.clone(),
                                        args: algefn.args.iter().map(|a| a.1.clone()).collect(),
                                        ret: algefn.retty.clone()
                                    }
                                )
                                .is_none());

                            self.lmd_context.known_function(&algefn.name.0).unwrap()
                        };
                        //import the algefn into scope, then add an apply node
                        //and hookup the arguments. Bail if the argument count does not
                        //fit

                        //import algefn

                        let applynode_output = self
                            .opt
                            .graph
                            .on_region(&self.lambda_region, |regbuilder| {
                                let (applynode, input_edges) =
                                    regbuilder.call(alge_import.port.clone(), &subargs).unwrap();
                                //Set edge types already, since we know them
                                //from the imported function
                                //NOTE: that the first input is always just a callable
                                for (argidx, edg) in input_edges.iter().skip(1).enumerate() {
                                    assert!(
                                        regbuilder
                                            .ctx_mut()
                                            .edge_mut(*edg)
                                            .ty
                                            .set_type(alge_import.args[argidx].clone())
                                            == Some(TypeState::Unset)
                                    );
                                }

                                applynode.output(0)
                            })
                            .unwrap();
                        //tag the output with the result type
                        self.opt
                            .typemap
                            .set(applynode_output.clone().into(), alge_import.ret.clone());
                        Ok(applynode_output)
                    } else {                        
                        let err = OptError::Any {
                            text: format!("\"{}\" does not name a build-in function, or declared algebraic function in this scope!", c.ident.0),
                        };
                        report(
                            error_reporter(err.clone(), expr_span.clone())
                                .with_label(
                                    Label::new(expr_span.clone())
                                        .with_message("here"),
                                )
                                .finish(),
                        );
                        Err(err)
                    }
                }
            }
            AlgeExprTy::EvalExpr(evalexpr) => {
                if !self.is_eval_allowed {
                    let err = OptError::Any {
                        text: "Eval expressions are only allowed in impl-blocks of operations!"
                            .to_owned(),
                    };
                    report(
                        error_reporter(err.clone(), evalexpr.span.clone())
                            .with_label(
                                Label::new(evalexpr.span.clone())
                                    .with_message("Consider removing this eval"),
                            )
                            .finish(),
                    );
                    return Err(err);
                }

                //For the eval expression, first find / insert the cv_import we need.
                // then setup all arguments, and finally add the call.
                let (eval_cv, eval_cv_type) = parent.get_cv_for_eval(self, &evalexpr)?;
                let mut inputs: SmallVec<[OutportLocation; 3]> = smallvec![eval_cv];

                let argount = evalexpr.params.len();
                for arg in evalexpr.params.into_iter() {
                    inputs.push(self.setup_alge_expr(arg, parent)?);
                }

                //Now setup the eval node
                let opnode = self
                    .opt
                    .graph
                    .on_region(&self.lambda_region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(
                                OptNode::new(
                                    EvalNode::new(argount, evalexpr.concept.0.clone()),
                                    expr_span,
                                ),
                                &inputs,
                            )
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();

                //tag output type
                self.opt.typemap.set(opnode.into(), eval_cv_type);

                Ok(opnode)
            }
            AlgeExprTy::FieldAccess { src, accessors } => {
                //Try to find the access source, if successful, hook the source up to this and
                // return the value

                let src = if let Some(access) = self.lmd_context.defined_vars.get(&src.0) {
                    access.port
                } else {
                    let err = OptError::Any {
                        text: format!("could not find {} in scope", src.0),
                    };
                    report(
                        error_reporter(err.clone(), expr_span.clone())
                            .with_label(
                                Label::new(expr_span.clone())
                                    .with_message("Try to define this before using it."),
                            )
                            .finish(),
                    );
                    return Err(err);
                };

                //Unwrap the `accessors` list into a chain of `ConstantIndex`, each feeding into its
                //successor.
                let mut src = src;
                for accessor in accessors {
                    let idx = if let Some(idx) = accessor.try_to_index() {
                        idx
                    } else {
                        let err = OptError::Any {
                            text: format!("Could not convert {} to index!", accessor),
                        };
                        report(
                            error_reporter(err.clone(), expr_span.clone())
                                .with_label(
                                    Label::new(expr_span.clone())
                                        .with_message("here"),
                                )
                                .finish(),
                        );
                        return Err(err);
                    };

                    let cinode = OptNode::new(ConstantIndex::new(idx), expr_span.clone());
                    //now connect it to the predecessor.
                    let new_src = self
                        .opt
                        .graph
                        .on_region(&self.lambda_region, |reg| {
                            let (opnode, _) = reg.connect_node(cinode, &[src.clone()]).unwrap();
                            opnode.output(0)
                        })
                        .unwrap();
                    //finally overwrite src
                    src = new_src;
                }

                //last src is the opnode with the _final_ value
                Ok(src)
            }
            AlgeExprTy::Ident(i) => {
                //try to resolve the ident, or throw an error if not possible
                let ident_node = if let Some(noderef) = self.lmd_context.defined_vars.get(&i.0) {
                    noderef
                } else {
                    let err = OptError::Any {
                        text: format!("could not find {} in scope", i.0),
                    };
                    report(
                        error_reporter(err.clone(), expr_span.clone())
                            .with_label(
                                Label::new(expr_span.clone())
                                    .with_message("Try to define this before using it."),
                            )
                            .finish(),
                    );
                    return Err(err);
                };

                Ok(ident_node.port)
            }
            AlgeExprTy::List(lst) => {
                //For now we just have a special "list" constructor that connects as many srcs as there are
                // list elements.

                //For it to not panic we first collect all items, then calculate how many
                // list items we need, resize the inputs array and then connect them

                let mut items: SmallVec<[OutportLocation; 3]> = SmallVec::new();
                for itm in lst {
                    items.push(self.setup_alge_expr(itm, parent)?);
                }

                let mut node = Construct::new();
                node.inputs = smallvec![Input::default(); items.len()];

                let opnode = self
                    .opt
                    .graph
                    .on_region(&self.lambda_region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(OptNode::new(node, expr_span), &items)
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            AlgeExprTy::Literal(lit) => {
                let optnode = match lit {
                    vola_ast::common::Literal::IntegerLiteral(i) => {
                        OptNode::new(ImmNat::new(i), expr_span)
                    }
                    vola_ast::common::Literal::FloatLiteral(f) => {
                        OptNode::new(ImmScalar::new(f), expr_span)
                    }
                };

                let opnode = self
                    .opt
                    .graph
                    .on_region(&self.lambda_region, |regbuilder| {
                        let opnode = regbuilder.insert_node(optnode);
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
        }
    }

    pub fn setup_csg_tree(
        &mut self,
        tree: vola_ast::csg::CSGOp,
        parent: &mut impl LambdaBuilderCtx,
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
                    if let Some(localcsg) = self.lmd_context.defined_vars.get(&tree.op.0) {
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
                                    .with_label(
                                        Label::new(tree.head_span().clone())
                                            .with_message(
                                                "Used in the place of an entity, or local field def here"
                                            ),
                                    )
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
                            let mut args: SmallVec<[OutportLocation; 3]> = SmallVec::new();
                            for arg in tree.args {
                                args.push(self.setup_alge_expr(arg, parent)?);
                            }
                            let (call_node, _) = self
                                .opt
                                .graph
                                .on_region(&self.lambda_region, |reg| {
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
                                text: format!("{} does not name an entity, local csg variable or field def", tree.op.0), 
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

        let mut args: SmallVec<[OutportLocation; 3]> = SmallVec::new();
        for subtree in tree.sub_trees {
            args.push(self.setup_csg_tree(subtree, parent)?);
        }

        //then build all argument expressions
        for arg in tree.args {
            args.push(self.setup_alge_expr(arg, parent)?);
        }

        //finally setup the node an connect all sub-trees and argumets to it
        let opnode = self
            .opt
            .graph
            .on_region(&self.lambda_region, |regbuilder| {
                let (opnode, _) = regbuilder
                    .connect_node(OptNode::new(opnode, span), &args)
                    .unwrap();
                opnode.output(0)
            })
            .unwrap();
        Ok(opnode)
    }
}
