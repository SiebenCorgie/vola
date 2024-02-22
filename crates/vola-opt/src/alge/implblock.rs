use crate::{
    common::{LmdContext, VarDef},
    error::OptError,
    OptNode, Optimizer,
};
use ahash::{AHashMap, AHashSet};
use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::LambdaNode,
    region::{Input, RegionLocation},
    smallvec::{smallvec, SmallVec},
    NodeRef,
};
use vola_ast::{
    alge::{AlgeExpr, AlgeExprTy, AlgeStmt, AssignStmt, EvalExpr, ImplBlock, LetStmt},
    common::Ident,
    csg::CSGNodeTy,
};
use vola_common::{report, Span};

use super::{CallOp, DummyNode, EvalNode, FieldAccess, Imm, ListConst, WkOp};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ConceptImplKey {
    pub concept_name: String,
    ///Name of either a entity or operation definition.
    pub node_name: String,
}

///Key to a operand that allows access to a operand's concept.
///
/// for instance in the expression `eval a.color();` we access `operand` "a" and access the concept `color`.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct OperandAccessKey {
    operand: String,
    concept: String,
}

///Tracks Operand access in a concept impl. For later use and debug purposes.
pub struct OperandAccess {
    ///the port that is register for that operand access
    outport: OutportLocation,
    cv_index: usize,
    access: SmallVec<[Span; 1]>,
}

///Contains the meta data of a concept implementation
pub struct ConceptImpl {
    pub span: Span,
    pub concept: Ident,
    pub node_type: CSGNodeTy,

    /// Operand variable description. This is the actual _generic_ interface of this λ-Node.
    ///
    /// In practice we might know that `OP<a,b>` has _two_ sub-trees. However, we don't know which concepts
    /// of a,b this λ is interested in. Therefore when building the block, everytime we eval a.X or b.Y, we check if concept
    /// a.X or b.Y are already described by the argument-interface. If not, we push it into the lookup table.
    ///
    /// When building a CSG tree we can then configure a.X and b.Y via the argument, which always need to be Lambda nodes.
    /// and call them as described by concept that is implemented.
    ///
    /// This map is keyed by operand-name first and operand's concept name second.
    //TODO The hash map might be overkill, but its working right now.
    // This is a 1..2 long hashmap usually. So this is really not worth it lol.
    pub cv_desc: AHashMap<OperandAccessKey, OperandAccess>,

    ///The lambda node representing this expression
    pub lambda: NodeRef,
    pub lambda_region: RegionLocation,
}

impl ConceptImpl {
    fn build_block(
        mut self,
        opt: &mut Optimizer,
        block: ImplBlock,
        mut lmd_context: LmdContext,
    ) -> Result<Self, OptError> {
        let ImplBlock {
            span,
            dst,
            operands,
            concept,
            concept_arg_naming,
            stmts,
            return_expr,
        } = block;

        for stmt in stmts {
            match stmt {
                AlgeStmt::Assign(assign) => self.setup_assign(opt, assign, &mut lmd_context)?,
                AlgeStmt::Let(letstmt) => self.setup_let(opt, letstmt, &mut lmd_context)?,
            }
        }

        //after setting up stmts in order, build the port of the final expr and connect that to the output of our
        // concept impl
        let return_expr_port = self.setup_alge_expr(opt, return_expr, &mut lmd_context)?;
        //add the output port and connect
        let result_port = opt
            .graph
            .node_mut(self.lambda)
            .node_type
            .unwrap_lambda_mut()
            .add_result();
        assert!(
            result_port == 0,
            "Result port index of concept impl should be 0 (only one result possible)"
        );

        //now connect to it
        opt.graph.on_region(&self.lambda_region, |reg| {
            reg.connect_to_result(return_expr_port, InputType::Result(result_port))
                .expect("Could not connect to result!")
        });

        Ok(self)
    }

    fn setup_assign(
        &mut self,
        opt: &mut Optimizer,
        assignstmt: AssignStmt,
        lmd_context: &mut LmdContext,
    ) -> Result<(), OptError> {
        let AssignStmt { span, dst, expr } = assignstmt;

        //Assign stmt, similar to the let stmt works, by setting up the expr on the left hand site, but
        // then overwriting the last known definition of dst.

        if !lmd_context.var_exists(&dst.0) {
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

        let sub_tree_output = self.setup_alge_expr(opt, expr, lmd_context)?;
        let last_def = lmd_context.defined_vars.get_mut(&dst.0).unwrap();
        last_def.port = sub_tree_output;
        Ok(())
    }

    fn setup_let(
        &mut self,
        opt: &mut Optimizer,
        let_stmt: LetStmt,
        lmd_context: &mut LmdContext,
    ) -> Result<(), OptError> {
        //for a let stmt we have to define the new variable _after_ we parsed the rhs expression.

        let LetStmt {
            span,
            decl_name,
            expr,
        } = let_stmt;

        if lmd_context.var_exists(&decl_name.0) {
            let existing = lmd_context.defined_vars.get(&decl_name.0).unwrap();
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

        let def_port = self.setup_alge_expr(opt, expr, lmd_context)?;

        //register in the lmd context
        lmd_context.add_define(
            decl_name.0,
            VarDef {
                port: def_port,
                span,
            },
        );

        Ok(())
    }

    //Sets up the alge expression, and if successful, return the output port that defines the final value.
    // TODO: Right now we don't have anything stateful, so a simple Outport location is enough. Hover, this might change whenever we introduce
    // buffer and image reads at a later stage.
    fn setup_alge_expr(
        &mut self,
        opt: &mut Optimizer,
        expr: AlgeExpr,
        lmd_context: &mut LmdContext,
    ) -> Result<OutportLocation, OptError> {
        let expr_span = expr.op_span();
        match expr.expr_ty {
            AlgeExprTy::Unary { op, operand } => {
                //setup the unary node, than recurse, setup the subexpression and hook it up to our unary expression
                let sub_output = self.setup_alge_expr(opt, *operand, lmd_context)?;
                let opnode = opt
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
                let left_out = self.setup_alge_expr(opt, *left, lmd_context)?;
                let right_out = self.setup_alge_expr(opt, *right, lmd_context)?;

                let opnode = opt
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
                    let arg = self.setup_alge_expr(opt, arg, lmd_context)?;
                    subargs.push(arg);
                }

                let opnode = opt
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
                let eval_cv = self.get_cv_for_eval(opt, &evalexpr);
                let mut inputs: SmallVec<[OutportLocation; 3]> = smallvec![eval_cv];
                for arg in evalexpr.params.into_iter() {
                    inputs.push(self.setup_alge_expr(opt, arg, lmd_context)?);
                }

                //Now setup the eval node
                let opnode = opt
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

                let src = if let Some(access) = lmd_context.defined_vars.get(&src.0) {
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

                let opnode = opt
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
                let ident_node = if let Some(noderef) = lmd_context.defined_vars.get(&i.0) {
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
                    items.push(self.setup_alge_expr(opt, itm, lmd_context)?);
                }

                let mut node = ListConst::new();
                node.inputs = smallvec![Input::default(); items.len()];

                let opnode = opt
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
                let opnode = opt
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

    fn get_cv_for_eval(&mut self, opt: &mut Optimizer, eval_expr: &EvalExpr) -> OutportLocation {
        let key = OperandAccessKey {
            operand: eval_expr.evaluator.0.clone(),
            concept: eval_expr.concept.0.clone(),
        };
        if let Some(registered) = self.cv_desc.get_mut(&key) {
            //Note that we use that
            registered.access.push(eval_expr.span.clone());
            return registered.outport.clone();
        }

        //Not found, therefore add
        let cv = opt
            .graph
            .node_mut(self.lambda)
            .node_type
            .unwrap_lambda_mut()
            .add_context_variable();
        let port = OutportLocation {
            node: self.lambda,
            output: OutputType::ContextVariableArgument(cv),
        };

        let old = self.cv_desc.insert(
            key,
            OperandAccess {
                outport: port.clone(),
                cv_index: cv,
                access: smallvec![eval_expr.span.clone()],
            },
        );
        //there shouldn't be one in here
        assert!(old.is_none());

        port
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
            src_csg_def
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

        //At this point we should have verified the overall signature. We therefore start building the actual λ-Node.
        // We do this by building the function-context helper that contains all concept-args as defined variables, as well as the
        // operation variables.

        //Create the lambda node. NOTE that we currently alway export. For debug purposes.
        let (lmd, lmd_region) = self.graph.on_omega_node(|omg| {
            omg.new_function(true, |lmd_builder| {
                lmd_builder.on_region(|reg| reg.parent_location())
            })
        });

        println!("Created lmd: {} with region {:?}", lmd, lmd_region);

        let lmd_context = LmdContext::new_for_impl_block(
            &mut self.graph,
            &mut self.typemap,
            lmd.clone(),
            &implblock,
            &src_csg_def,
            &src_concept,
        );

        //Now reverse the game by building the initial ConceptImpl and then letting it handle itself.
        let concept_impl = ConceptImpl {
            span: implblock.span.clone(),
            concept: implblock.concept.clone(),
            node_type: src_csg_def.ty.clone(),
            cv_desc: AHashMap::default(),
            lambda: lmd,
            lambda_region: lmd_region,
        };

        let build_concept_impl = concept_impl.build_block(self, implblock, lmd_context)?;

        //After finishing (successfully) add the impl description to the Optimizer so we can
        // find that later on if needed and return
        //NOTE: _should be the same, but could also be changed at some point I guess_
        let lmd = build_concept_impl.lambda.clone();

        let old = self.concept_impl.insert(concept_key, build_concept_impl);
        assert!(old.is_none(), "Had an old concept + node combination already, should have been caught before adding it!");

        Ok(lmd)
    }
}
