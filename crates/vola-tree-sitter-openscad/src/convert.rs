//! Takes care of the conversion of an ScadAst to a VolaAst, which is also where we test for support, and introduce some
//! conventions:
//!
//! # Conventions
//!
//! 1. If a Scad _CSG-Statement_ (ScadStmt::Chain) is converted, the result will always be _union-ed_ with
//!    an assumed csg-variable `union_tree`.

use std::str::FromStr;

use smallvec::{SmallVec, smallvec};
use vola_ast::{
    alge::{Expr, Func},
    common::{Block, DataTy, Ty, TypedIdent},
    csg::ScopedCall,
};
use vola_common::Span;

use crate::{
    error::ParserError,
    report_here,
    scad_ast::{ChainElement, ScadBlock, ScadCall, ScadExpr, ScadModule, ScadParameter, ScadStmt},
    util::ScadLiteral,
    warn_here,
};

enum ConvertedLiteral {
    #[allow(unused)]
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        increment: Option<Box<Expr>>,
        span: Span,
    },
    Expr(Expr),
}

fn convert_literal(lit: ScadLiteral, span: Span) -> Result<ConvertedLiteral, ParserError> {
    match lit {
        crate::util::ScadLiteral::Undef => Err(ParserError::Unexpected(
            "undefined values are not supported in Vola!".to_owned(),
        )),
        crate::util::ScadLiteral::Decimal(i) => {
            if i.is_negative() {
                Ok(ConvertedLiteral::Expr(Expr {
                    span: span.clone(),
                    expr_ty: vola_ast::alge::ExprTy::Unary {
                        op: vola_ast::alge::UnaryOp::Neg,
                        operand: Box::new(Expr {
                            span,
                            expr_ty: vola_ast::alge::ExprTy::Literal(
                                vola_ast::common::Literal::IntegerLiteral(i.abs() as usize),
                            ),
                        }),
                    },
                }))
            } else {
                Ok(ConvertedLiteral::Expr(Expr {
                    span,
                    expr_ty: vola_ast::alge::ExprTy::Literal(
                        vola_ast::common::Literal::IntegerLiteral(i.abs() as usize),
                    ),
                }))
            }
        }
        crate::util::ScadLiteral::Float(f) => Ok(ConvertedLiteral::Expr(Expr {
            span,
            expr_ty: vola_ast::alge::ExprTy::Literal(vola_ast::common::Literal::FloatLiteral(f)),
        })),
        crate::util::ScadLiteral::Boolean(b) => Ok(ConvertedLiteral::Expr(Expr {
            span,
            expr_ty: vola_ast::alge::ExprTy::Literal(vola_ast::common::Literal::BoolLiteral(b)),
        })),
        crate::util::ScadLiteral::List(l) => {
            let elements = l
                .into_iter()
                .map(|ele| convert_expr(ele))
                .collect::<Result<_, ParserError>>()?;
            Ok(ConvertedLiteral::Expr(Expr {
                span: Span::empty(),
                expr_ty: vola_ast::alge::ExprTy::List(elements),
            }))
        }
        crate::util::ScadLiteral::Range {
            start,
            end,
            increment,
        } => {
            let start = Box::new(convert_expr(*start)?);
            let end = Box::new(convert_expr(*end)?);
            let increment = if let Some(inc) = increment {
                Some(Box::new(convert_expr(*inc)?))
            } else {
                None
            };

            Ok(ConvertedLiteral::Range {
                start,
                end,
                increment,
                span,
            })
        }
        crate::util::ScadLiteral::Function(_f) => todo!(),
        ScadLiteral::String(s) => {
            warn_here(
                "converting string to color, that might be lossy",
                span.clone(),
            );
            match color_art::Color::from_str(&s) {
                Ok(c) => {
                    let [r, g, b] = [c.red(), c.green(), c.blue()];
                    Ok(ConvertedLiteral::Expr(Expr {
                        span: span.clone(),
                        expr_ty: vola_ast::alge::ExprTy::List(vec![
                            Expr {
                                span: span.clone(),
                                expr_ty: vola_ast::alge::ExprTy::Literal(
                                    vola_ast::common::Literal::FloatLiteral(r as f64 / 255.0),
                                ),
                            },
                            Expr {
                                span: span.clone(),
                                expr_ty: vola_ast::alge::ExprTy::Literal(
                                    vola_ast::common::Literal::FloatLiteral(g as f64 / 255.0),
                                ),
                            },
                            Expr {
                                span: span.clone(),
                                expr_ty: vola_ast::alge::ExprTy::Literal(
                                    vola_ast::common::Literal::FloatLiteral(b as f64 / 255.0),
                                ),
                            },
                        ]),
                    }))
                }
                Err(_e) => {
                    report_here(format!("Could not convert '{s}' to color"), span);
                    Err(ParserError::MalformedNode(
                        "could not convert string to color".to_owned(),
                    ))
                }
            }
        }
    }
}

fn convert_expr(expr: ScadExpr) -> Result<vola_ast::alge::Expr, ParserError> {
    match expr {
        ScadExpr::Assert => Err(ParserError::Unexpected("assert in expr".to_owned())),
        ScadExpr::Index { value, index, span } => {
            let src = convert_expr(*value)?;
            let access = convert_expr(*index)?;

            //try to convert the access-expr to an accessor, otherwise something doesn't
            //isn't right
            let accessor = match access.expr_ty {
                vola_ast::alge::ExprTy::Ident(i) => vola_ast::alge::FieldAccessor::Ident {
                    span: access.span,
                    ident: i,
                },
                vola_ast::alge::ExprTy::Literal(l) => match l {
                    vola_ast::common::Literal::IntegerLiteral(i) => {
                        vola_ast::alge::FieldAccessor::Digit {
                            span: access.span,
                            digit: i,
                        }
                    }
                    other => {
                        report_here(
                            format!("can not index with {other:?}, only integer literal"),
                            access.span,
                        );
                        return Err(ParserError::Unexpected(
                            "index with non-integer literal".to_owned(),
                        ));
                    }
                },
                other => {
                    report_here(format!("cannot index with {other:?}"), access.span);
                    return Err(ParserError::MalformedNode(
                        "index must be identifier or integer".to_owned(),
                    ));
                }
            };

            let src = if let Expr {
                span: _,
                expr_ty: vola_ast::alge::ExprTy::Ident(i),
            } = src
            {
                i
            } else {
                report_here("Can only index values", span);
                return Err(ParserError::MalformedNode(
                    "can only index identifier".to_owned(),
                ));
            };

            Ok(Expr {
                span,
                expr_ty: vola_ast::alge::ExprTy::FieldAccess {
                    src,
                    accessors: smallvec![accessor],
                },
            })
        }
        ScadExpr::Var { span, var } => Ok(Expr {
            span,
            expr_ty: vola_ast::alge::ExprTy::Ident(var),
        }),
        ScadExpr::Literal { span, lit } => match convert_literal(lit, span)? {
            ConvertedLiteral::Expr(expr) => Ok(expr),
            ConvertedLiteral::Range { span, .. } => {
                report_here("Unexpected range expression".to_owned(), span);
                Err(ParserError::Unexpected("range-expression".to_owned()))
            }
        },
        ScadExpr::Unary { op, expr, span } => {
            let expr = convert_expr(*expr)?;
            Ok(Expr {
                span,
                expr_ty: vola_ast::alge::ExprTy::Unary {
                    op,
                    operand: Box::new(expr),
                },
            })
        }
        ScadExpr::Binary {
            op,
            left,
            right,
            span,
        } => {
            let left = convert_expr(*left)?;
            let right = convert_expr(*right)?;
            Ok(Expr {
                span,
                expr_ty: vola_ast::alge::ExprTy::Binary {
                    left: Box::new(left),
                    right: Box::new(right),
                    op,
                },
            })
        }
        ScadExpr::Ternary {
            condition,
            consequence,
            alternative,
            span,
        } => {
            //Ternary expressions in Scad will be _just_
            //if-expressions
            let condition = convert_expr(*condition)?;
            let consequence = convert_expr(*consequence)?;
            let alternative = convert_expr(*alternative)?;

            Ok(Expr {
                span: span.clone(),
                expr_ty: vola_ast::alge::ExprTy::Branch(Box::new(vola_ast::common::Branch {
                    span,
                    conditional: (
                        condition,
                        Box::new(Block {
                            span: consequence.span.clone(),
                            stmts: SmallVec::new(),
                            retexpr: Some(consequence),
                        }),
                    ),
                    unconditional: Some(Box::new(Block {
                        span: alternative.span.clone(),
                        stmts: SmallVec::new(),
                        retexpr: Some(alternative),
                    })),
                })),
            })
        }
        ScadExpr::Overwrite { span, .. } => {
            report_here(
                "can not handle overwrite expressiont ('let' / 'assign') yet",
                span,
            );
            Err(ParserError::UnsupportedScadFeature(
                "overwrite-expr".to_owned(),
            ))
        }
        ScadExpr::Call(_c) => {
            todo!()
        }
    }
}

fn convert_call(call: ScadCall) -> Result<vola_ast::common::Call, ParserError> {
    let args = call
        .args
        .into_iter()
        .map(|arg| match arg {
            crate::scad_ast::ScadArg::Expr(expr) => convert_expr(expr),
            crate::scad_ast::ScadArg::Assign(assign) => {
                report_here(
                    "assignment argument should have been normalized before!",
                    assign.span,
                );
                return Err(ParserError::Unexpected("assignment argument".to_owned()));
            }
        })
        .collect::<Result<_, ParserError>>()?;

    Ok(vola_ast::common::Call {
        span: call.span.clone(),
        ident: call.function,
        args,
    })
}

fn convert_chain(elements: Vec<ChainElement>) -> Result<ScopedCall, ParserError> {
    //basically reverse through the chain, and assemble the _sub-tree_ with the previouse op.
    //
    //_the-thing_ is, that you can't build actual trees in scad, only chains, so thats quiete easy

    let mut sub_tree = None;

    let mut elements = elements.into_iter().rev();
    //NOTE: we sometimes have to jump in the chain, For instance if we encounter a block, we need to jump to the previous
    //      (hopefully) call-element, in order to know what is being _blocked_
    //
    while let Some(ele) = elements.next() {
        match ele {
            ChainElement::Call(c) => {
                let call = convert_call(c)?;
                let span = call.span.clone();
                let scoped_csg_call = ScopedCall {
                    span: span.clone(),
                    call,
                    //push down the sub-tree into the block, if there is any
                    blocks: if let Some(taken) = sub_tree.take() {
                        vec![expr_to_block(taken)]
                    } else {
                        Vec::with_capacity(0)
                    },
                };
                //now setup the new scoped call as subtree
                sub_tree = Some(Expr {
                    span: span.clone(),
                    expr_ty: vola_ast::alge::ExprTy::ScopedCall(Box::new(scoped_csg_call)),
                });
            }
            ChainElement::Block(mut b) => {
                let call = if let Some(next) = elements.next() {
                    match next {
                        ChainElement::Block(b) => {
                            report_here(
                                "A block's previouse chain element must be some kind of call",
                                b.span,
                            );
                            return Err(ParserError::Unexpected(
                                "block can't be block's predecesor".to_owned(),
                            ));
                        }
                        ChainElement::Call(c) => c,
                    }
                } else {
                    report_here(
                        "expected a previous call like `union` or `intersect`",
                        b.span.clone(),
                    );
                    return Err(ParserError::Unexpected("no previous call".to_owned()));
                };
                let call = convert_call(call)?;
                let span = call.span.clone();

                //pull out the first (csg)statment of the sub-block, if this is a boolean operation.
                //because for instance OpenScad's
                //`
                // difference(){
                //    x
                //    y
                //    z
                // };
                //`
                //means
                //`
                //Subtract(){
                //  x
                //}{
                // Union(){y}{z}
                //}
                //`
                //In vola-terms.

                let is_binary_csg = match call.ident.0.as_str() {
                    "OSUnion" | "OSDifference" | "OSIntersection" => true,
                    _ => false,
                };

                if is_binary_csg {
                    //We have to handle two edge cases:
                    // 1. There is only one CSG-Stmt in the block => just return that statment in a union,
                    // 2. There is no CSG-Stmt in the block => do not change sub-tree at all
                    let mut first_csg_stmt = None;
                    b.stmts.retain(|item| {
                        if first_csg_stmt.is_none() {
                            if let ScadStmt::Chain { span, chain } = item {
                                first_csg_stmt = Some(ScadStmt::Chain {
                                    span: span.clone(),
                                    chain: chain.clone(),
                                });
                                false
                            } else {
                                true
                            }
                        } else {
                            true
                        }
                    });

                    let has_more_csg_stmts = b.stmts.iter().fold(false, |had_one, item| {
                        if let ScadStmt::Chain { .. } = item {
                            true
                        } else {
                            had_one
                        }
                    });

                    if let Some(first) = first_csg_stmt {
                        if has_more_csg_stmts {
                            //put the first stmt in the _left_ part, and the rest in the second part.
                            let left = ScadBlock {
                                span: Span::empty(),
                                stmts: vec![first],
                            };
                            let right = ScadBlock {
                                span: Span::empty(),
                                stmts: b.stmts,
                            };
                            let leftblock = convert_block(left)?;
                            let rightblock = convert_block(right)?;
                            let scoped_csg_call = ScopedCall {
                                span: span.clone(),
                                call,
                                //push down the sub-tree into the block, if there is any
                                blocks: vec![leftblock, rightblock],
                            };
                            sub_tree = Some(Expr {
                                span,
                                expr_ty: vola_ast::alge::ExprTy::ScopedCall(Box::new(
                                    scoped_csg_call,
                                )),
                            });
                        } else {
                            //no further stmts, therfore just union with the current tree
                            let sub_block = convert_block(ScadBlock {
                                span: Span::empty(),
                                stmts: vec![first],
                            })?;
                            let scoped_csg_call = ScopedCall {
                                span: span.clone(),
                                call: vola_ast::common::Call {
                                    span: call.span,
                                    ident: vola_ast::common::Ident("OSUnion".to_owned()),
                                    args: SmallVec::new(),
                                },
                                //push down the sub-tree into the block, if there is any
                                blocks: vec![sub_block],
                            };
                            sub_tree = Some(Expr {
                                span,
                                expr_ty: vola_ast::alge::ExprTy::ScopedCall(Box::new(
                                    scoped_csg_call,
                                )),
                            });
                        }
                    } else {
                        //do nothing, cause there are no csg stmts
                        assert!(!has_more_csg_stmts);
                    }
                } else {
                    let sub_block = convert_block(b)?;
                    //build the call, based on the scad-defined sub-block (this-time)

                    let scoped_csg_call = ScopedCall {
                        span: span.clone(),
                        call,
                        //push down the sub-tree into the block, if there is any
                        blocks: vec![sub_block],
                    };

                    if sub_tree.is_some() {
                        report_here(
                            "Cannot use a scope'd in a chain, only as the last element of a chain!"
                                .to_owned(),
                            span,
                        );
                    } else {
                        sub_tree = Some(Expr {
                            span,
                            expr_ty: vola_ast::alge::ExprTy::ScopedCall(Box::new(scoped_csg_call)),
                        });
                    }
                }
            }
        }
    }

    match sub_tree {
        Some(Expr {
            span: _,
            expr_ty: vola_ast::alge::ExprTy::ScopedCall(sc),
        }) => Ok(*sc),
        _other => Err(ParserError::MalformedNode(
            "CSG chain did not resolve to csg-object".to_owned(),
        )),
    }
}

///A converted Scad statement might actually be a statement, or
///a CSG-Chain, which is a Expr in vola
enum ConvertedStatement {
    CsgChain(ScopedCall),
    Stmt(vola_ast::common::Stmt),
}
impl ConvertedStatement {
    pub fn unwrap_stmt(self) -> vola_ast::common::Stmt {
        if let Self::Stmt(s) = self {
            s
        } else {
            panic!("Was not a statement!")
        }
    }
}
fn convert_stmt(stmt: ScadStmt) -> Result<ConvertedStatement, ParserError> {
    match stmt {
        ScadStmt::None | ScadStmt::Assert => Err(ParserError::Unexpected(
            "Cannot convert None or Assert to Vola statement".to_owned(),
        )),
        ScadStmt::IncludeStmt(_) => Err(ParserError::Unexpected(
            "Include Statement should have been resolved by now".to_owned(),
        )),
        ScadStmt::Comment(c) => Ok(ConvertedStatement::Stmt(vola_ast::common::Stmt::Comment(c))),
        ScadStmt::Assign(assign) => {
            //Note, the normalization should have removed double-assignments, since they don't work (as you might intend) in
            //scad anyways, so any assign actually becomes a "let" in vola
            let converted_expr = convert_expr(*assign.expr)?;
            let letstmt = vola_ast::alge::LetStmt {
                span: assign.span,
                decl_name: assign.var,
                expr: converted_expr,
            };
            Ok(ConvertedStatement::Stmt(vola_ast::common::Stmt::Let(
                letstmt,
            )))
        }
        ScadStmt::Overwrite { overwrites, block } => {
            //overwrites work by creating an inline-block, and moving the assignments into that block
            let mut inline_block = convert_block(block)?;
            let overwrites = overwrites
                .into_iter()
                .map(|ov| convert_stmt(ScadStmt::Assign(ov)))
                .collect::<Result<Vec<_>, ParserError>>()?;
            //push the assignments to the front
            inline_block
                .stmts
                .insert_many(0, overwrites.into_iter().map(|ov| ov.unwrap_stmt()));
            Ok(ConvertedStatement::Stmt(vola_ast::common::Stmt::Block(
                Box::new(inline_block),
            )))
        }
        ScadStmt::IfBlock {
            condition,
            consequence,
            alternative,
            head_span,
        } => {
            let condition = convert_expr(condition)?;
            let consequence = convert_block(consequence)?;
            let alternative = if let Some(alt) = alternative {
                Some(Box::new(convert_block(alt)?))
            } else {
                None
            };
            Ok(ConvertedStatement::Stmt(vola_ast::common::Stmt::Branch(
                vola_ast::common::Branch {
                    span: head_span,
                    conditional: (condition, Box::new(consequence)),
                    unconditional: alternative,
                },
            )))
        }
        #[allow(unused)]
        ScadStmt::ForBlock {
            range_start,
            range_end,
            increment,
            block,
            is_intersect,
        } => todo!(),
        ScadStmt::Chain { span: _, chain } => {
            //This is the case, where we emit a chain of Elements as a CSG expression.
            Ok(ConvertedStatement::CsgChain(convert_chain(chain)?))
        }
    }
}

fn expr_to_block(expr: Expr) -> Block {
    vola_ast::common::Block {
        span: Span::empty(),
        stmts: SmallVec::new(),
        retexpr: Some(expr),
    }
}

///converts `block` into a valid Vola block. Returns the build CSG tree in the return expression, and pushes all other
///statements _in-order_.
fn convert_block(block: ScadBlock) -> Result<Block, ParserError> {
    let mut vblock = Block {
        span: block.span,
        stmts: SmallVec::new(),
        retexpr: None,
    };

    //Now thats where the _interesting_ stuff happens.
    //
    //We treat each csg-statment (chains, union-blocks, transforms etc.) as a _union_ with all previouse
    //statements (saved in `build_union_tree`).
    //
    //Effectively we build a left_sided union _chain_, where each statment is _unioned_
    //with all previous statements.
    for stmt in block.stmts {
        match convert_stmt(stmt)? {
            ConvertedStatement::Stmt(stmt) => vblock.stmts.push(stmt),
            ConvertedStatement::CsgChain(scope_call) => {
                let unioned_expr = if let Some(resting_expr) = vblock.retexpr.take() {
                    let union = Expr {
                        span: Span::empty(),
                        expr_ty: vola_ast::alge::ExprTy::ScopedCall(Box::new(ScopedCall {
                            span: Span::empty(),
                            call: vola_ast::common::Call {
                                span: Span::empty(),
                                ident: vola_ast::common::Ident("OSUnion".to_string()),
                                args: SmallVec::new(),
                            },
                            blocks: vec![
                                expr_to_block(resting_expr),
                                expr_to_block(Expr {
                                    span: scope_call.span.clone(),
                                    expr_ty: vola_ast::alge::ExprTy::ScopedCall(Box::new(
                                        scope_call,
                                    )),
                                }),
                            ],
                        })),
                    };

                    union
                } else {
                    Expr {
                        span: scope_call.span.clone(),
                        expr_ty: vola_ast::alge::ExprTy::ScopedCall(Box::new(scope_call)),
                    }
                };

                vblock.retexpr = Some(unioned_expr);
            }
        }
    }

    Ok(vblock)
}

pub fn emit_module_as_function(module: ScadModule) -> Result<Func, ParserError> {
    //TODO: build the header, and fill in _real_ as all parameter types (for now)
    //      This will / should work for most parameters.

    let block = convert_block(module.block)?;

    let args = module
        .args
        .into_iter()
        .map(|arg| match arg {
            ScadParameter::ByIdent { span, ident } => Ok(TypedIdent {
                span,
                ty: Ty::Simple(DataTy::Real),
                ident,
            }),
            ScadParameter::ByAssignment(a) => {
                report_here("Assignment-Parameter not supported", a.span.clone());
                Err(ParserError::UnsupportedScadFeature(
                    "assignment-parameter".to_owned(),
                ))
            }
        })
        .collect::<Result<SmallVec<[TypedIdent; 3]>, ParserError>>()?;

    Ok(Func {
        span: module.span,
        is_export: false,
        name: module.name,
        args,
        return_type: Ty::Simple(DataTy::Csg),
        block,
    })
}

pub fn emit_block_as_main_function(block: ScadBlock) -> Result<Func, ParserError> {
    let vblock = convert_block(block)?;
    //build a function called '__scad_main__' that uses `block` as body, no arguments, and only a csg return value

    Ok(Func {
        span: Span::empty(),
        is_export: false,
        name: vola_ast::common::Ident("__scad_main__".to_owned()),
        args: SmallVec::new(),
        return_type: Ty::Simple(DataTy::Csg),
        block: vblock,
    })
}
