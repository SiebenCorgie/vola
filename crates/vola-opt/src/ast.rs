//! Module that handles the opt-graph building based on [AST](vola-ast) nodes.

use rvsdg::{
    edge::OutportLocation,
    region::{Input, RegionLocation},
    smallvec::{smallvec, SmallVec},
    NodeRef, Rvsdg,
};
use vola_ast::{
    alge::{AlgeExpr, AlgeExprTy, EvalExpr},
    AstEntry, TopLevelNode,
};
use vola_common::report;

use crate::{
    alge::{CallOp, EvalNode, FieldAccess, Imm, ListConst, WkOp},
    common::LmdContext,
    error::OptError,
    OptEdge, OptNode, Optimizer,
};

impl Optimizer {
    ///Adds the top-level node to the optimizer graph. If it applies, it returns a reference to the created node.
    pub fn add_tl_node(&mut self, tlnode: TopLevelNode) -> Result<Option<NodeRef>, OptError> {
        match tlnode.entry {
            //We ignore those atm
            AstEntry::Comment(_) => Ok(None),
            AstEntry::Concept(csgcon) => {
                if let Some(existing_concept) = self.concepts.get(&csgcon.name.0) {
                    let err = OptError::AnySpannedWithSource {
                        source_span: existing_concept.span.clone().into(),
                        source_text: format!("First occurrence of {}", existing_concept.name.0),
                        text: format!("Concept {} was already defined", existing_concept.name.0),
                        span: csgcon.span.clone().into(),
                        span_text: format!("Tried to redefine it here"),
                    };

                    report(err.clone(), existing_concept.span.get_file());

                    return Err(err);
                } else {
                    //No yet in collection, therefore push
                    self.concepts.insert(csgcon.name.0.clone(), csgcon);
                    Ok(None)
                }
            }
            AstEntry::CSGNodeDef(csgnd) => {
                //similar to the concept case, test if there is already one, if not, push
                if let Some(existing_csg) = self.csg_node_defs.get(&csgnd.name.0) {
                    let err = OptError::AnySpannedWithSource {
                        source_span: existing_csg.span.clone().into(),
                        source_text: format!("First occurrence of {}", existing_csg.name.0),
                        text: format!("Operation or Entity {} was already defined. \nNote that operations and entities share one name space.", existing_csg.name.0),
                        span: csgnd.span.clone().into(),
                        span_text: format!("Tried to redefine it here"),
                    };

                    report(err.clone(), existing_csg.span.get_file());

                    return Err(err);
                } else {
                    //No yet in collection, therefore push
                    self.csg_node_defs.insert(csgnd.name.0.clone(), csgnd);
                    Ok(None)
                }
            }
            AstEntry::ImplBlock(implblock) => self.add_impl_block(implblock).map(|t| Some(t)),
            AstEntry::FieldDefine(fdef) => Ok(None),
            AstEntry::ExportFn(expfn) => Ok(None),
        }
    }
}

///Helper that can take an AST statement or expression, and build the corresponding node setup in the RVSDG-Graph.
///
/// NOTE: If we'd have a non-structured language this would be the structurizer. But we are structured, we can
/// build the graph directly. See [the RVSDG paper](https://arxiv.org/pdf/1912.05036.pdf) 5.1 _Construction_.
pub struct AstLambdaBuilder<'a> {
    pub graph: &'a mut Rvsdg<OptNode, OptEdge>,
    pub lambda: NodeRef,
    pub lambda_region: RegionLocation,
    pub lmd_context: LmdContext,
}

pub trait LambdaBuilderCtx {
    ///Registers the `eval_expr` in the lambda's context_variable set, and returns the Outport of said cv.
    fn get_cv_for_eval(
        &mut self,
        builder: &mut AstLambdaBuilder,
        eval_expr: &EvalExpr,
    ) -> Result<OutportLocation, OptError> {
        let err = OptError::AnySpanned { span: eval_expr.span.clone().into(), text: format!("Cannot use eval expression in this context.\nEval expressions are only allowed in concept implementations!"), span_text: format!("consider moving this eval into a concept implementation") };
        report(err.clone(), eval_expr.span.get_file());
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
                // Since the language currently has no arbritray _functions_ a call must always resolve to one of those.
                //
                // NOTE: We could probably implement a subset of the glsl language, which would allow us to let people copy
                // paste glsl expressions into vola :D

                let wknode = if let Some(wknode) = WkOp::try_parse(&c.ident.0) {
                    wknode
                } else {
                    let err = OptError::AnySpanned {
                        span: expr_span.clone().into(),
                        text: format!("Unknown function {} called!", c.ident.0),
                        span_text: "here".to_owned(),
                    };
                    report(err.clone(), c.span.get_file());
                    return Err(err);
                };

                //Build the call node with its sub args
                let mut subargs: SmallVec<[OutportLocation; 3]> = SmallVec::new();
                for arg in c.args {
                    let arg = self.setup_alge_expr(arg, parent)?;
                    subargs.push(arg);
                }

                let opnode = self
                    .graph
                    .on_region(&self.lambda_region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(OptNode::new(CallOp::new(wknode), expr_span), &subargs)
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();

                Ok(opnode)
            }
            AlgeExprTy::EvalExpr(evalexpr) => {
                //For the eval expression, first find / insert the cv_import we need.
                // then setup all arguments, and finally add the call.
                let eval_cv = parent.get_cv_for_eval(self, &evalexpr)?;
                let mut inputs: SmallVec<[OutportLocation; 3]> = smallvec![eval_cv];
                for arg in evalexpr.params.into_iter() {
                    inputs.push(self.setup_alge_expr(arg, parent)?);
                }

                //Now setup the eval node
                let opnode = self
                    .graph
                    .on_region(&self.lambda_region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(OptNode::new(EvalNode::new(), expr_span), &inputs)
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            AlgeExprTy::FieldAccess { src, accessors } => {
                //Try to find the access source, if successful, hook the source up to this and
                // return the value

                let src = if let Some(access) = self.lmd_context.defined_vars.get(&src.0) {
                    access.port
                } else {
                    let err = OptError::AnySpanned {
                        span: expr_span.clone().into(),
                        text: format!("could not find {} in scope", src.0),
                        span_text: "try to define this before using it".to_owned(),
                    };
                    report(err.clone(), expr_span.get_file());
                    return Err(err);
                };

                let opnode = self
                    .graph
                    .on_region(&self.lambda_region, |regbuilder| {
                        let (opnode, _) = regbuilder
                            .connect_node(
                                OptNode::new(FieldAccess::new(accessors), expr_span),
                                &[src],
                            )
                            .unwrap();
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
            AlgeExprTy::Ident(i) => {
                //try to resolve the ident, or throw an error if not possible
                let ident_node = if let Some(noderef) = self.lmd_context.defined_vars.get(&i.0) {
                    noderef
                } else {
                    let err = OptError::AnySpanned {
                        span: expr_span.clone().into(),
                        text: format!("could not find {} in scope", i.0),
                        span_text: "try to define this before using it".to_owned(),
                    };
                    report(err.clone(), expr_span.get_file());
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

                let mut node = ListConst::new();
                node.inputs = smallvec![Input::default(); items.len()];

                let opnode = self
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
                let opnode = self
                    .graph
                    .on_region(&self.lambda_region, |regbuilder| {
                        let opnode = regbuilder.insert_node(OptNode::new(Imm::new(lit), expr_span));
                        opnode.output(0)
                    })
                    .unwrap();
                Ok(opnode)
            }
        }
    }
}
