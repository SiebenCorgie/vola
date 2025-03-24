use ahash::AHashMap;
use vola_common::Span;

use crate::{
    error::ParserError,
    report_here,
    scad_ast::{ScadArg, ScadBlock, ScadCall, ScadExpr, ScadStmt},
    util::ScadLiteral,
};

impl ScadExpr {
    pub fn normalize_as_arg(&mut self) {
        match self {
            Self::Assert | Self::Var { .. } => {}
            Self::Unary { expr, .. } => expr.normalize_as_arg(),
            Self::Binary { left, right, .. } => {
                left.normalize_as_arg();
                right.normalize_as_arg();
            }
            Self::Ternary {
                consequence,
                alternative,
                ..
            } => {
                consequence.normalize_as_arg();
                alternative.normalize_as_arg();
            }
            Self::Call(call) => {
                for arg in call.args.iter_mut() {
                    arg.normalize_as_arg();
                }
            }
            Self::Overwrite { .. } => {} // I don't think those need to be normalized ...
            Self::Literal { lit, .. } => {
                match lit {
                    ScadLiteral::List(list) => {
                        for list_element in list {
                            list_element.normalize_as_arg();
                        }
                    }
                    ScadLiteral::Decimal(d) => *lit = ScadLiteral::Float(*d as f64),
                    _other => {}
                };
            }
            Self::Index {
                span: _,
                value,
                index: _,
            } => value.normalize_as_arg(),
        }
    }
}

impl ScadArg {
    ///Normalizes the argument. This means converting any list-of-values into a list of floats,
    ///and integer into a float, and
    pub fn normalize_as_arg(&mut self) {
        match self {
            Self::Expr(expr) => expr.normalize_as_arg(),
            Self::Assign(assign) => assign.expr.normalize_as_arg(),
        }
    }
}

impl ScadCall {
    ///Normalizes any known calls. This takes care mostly of wrangling CSG-calls into the
    ///Vola argument structure supplied by the standard-implementation.
    pub fn normalize(&mut self) -> Result<(), ParserError> {
        //rename CSG functions to their equivalent in the openscad_library.vola file.
        //then reorder argument / introduce default values where needed.
        //
        //We do that by pre-defining all arguments by their _default_ value, and then overwrite either by indexed
        //(in case of a simple expression), or by-name, in case of an assignment.
        match self.function.0.as_str() {
            "sphere" => {
                self.function.0 = "OSSphere".to_owned();
                let args_iter = std::mem::take(&mut self.args);
                //predefine args with default values, in this case [1.0]
                self.args.push(ScadArg::Expr(ScadExpr::Literal {
                    span: Span::empty(),
                    lit: ScadLiteral::Float(1.0),
                }));
                let mut expr_index = 0;
                for arg in args_iter {
                    match arg {
                        ScadArg::Expr(mut expr) => {
                            //normalize before using
                            expr.normalize_as_arg();
                            self.args[expr_index] = ScadArg::Expr(expr);
                            expr_index += 1;
                        }
                        ScadArg::Assign(assign) => {
                            match assign.var.0.as_str() {
                                "d" => {
                                    //overwrite second
                                    let d_arg = ScadArg::Expr(ScadExpr::Binary {
                                        span: Span::empty(),
                                        op: vola_ast::alge::BinaryOp::Mul,
                                        left: assign.expr,
                                        right: Box::new(ScadExpr::Literal {
                                            span: Span::empty(),
                                            lit: ScadLiteral::Float(0.5),
                                        }),
                                    });
                                    self.args[0] = d_arg;
                                }
                                other => {
                                    report_here(
                                        format!("unknown named argument '{other}' for cube!"),
                                        assign.span,
                                    );
                                    return Err(ParserError::MalformedNode(
                                        "Unknown named argument".to_owned(),
                                    ));
                                }
                            }
                        }
                    }
                }
            }
            "cube" => {
                self.function.0 = "OSCube".to_owned();
                let args_iter = std::mem::take(&mut self.args);
                //predefine args with default values, in this case [[1; 3], false]
                self.args.push(ScadArg::Expr(ScadExpr::Literal {
                    span: Span::empty(),
                    lit: ScadLiteral::vec_n(1.0, 3),
                }));
                self.args.push(ScadArg::Expr(ScadExpr::Literal {
                    span: Span::empty(),
                    lit: ScadLiteral::Boolean(false),
                }));

                let mut expr_index = 0;
                for arg in args_iter {
                    match arg {
                        ScadArg::Expr(mut expr) => {
                            //normalize before using
                            expr.normalize_as_arg();
                            //note, if this is the first arg, and its a single number, splice to a vec
                            let expr = if expr_index == 0 {
                                if let ScadExpr::Literal {
                                    span,
                                    lit: ScadLiteral::Float(f),
                                } = expr
                                {
                                    ScadExpr::Literal {
                                        span,
                                        lit: ScadLiteral::vec_n(f, 3),
                                    }
                                } else {
                                    expr
                                }
                            } else {
                                expr
                            };

                            self.args[expr_index] = ScadArg::Expr(expr);
                            expr_index += 1;
                        }
                        ScadArg::Assign(assign) => {
                            match assign.var.0.as_str() {
                                "center" => {
                                    //overwrite second
                                    self.args[1] = ScadArg::Expr(*assign.expr);
                                }
                                other => {
                                    report_here(
                                        format!("unknown named argument '{other}' for cube!"),
                                        assign.span,
                                    );
                                    return Err(ParserError::MalformedNode(
                                        "Unknown named argument".to_owned(),
                                    ));
                                }
                            }
                        }
                    }
                }
            }
            "cylinder" => {
                self.function.0 = "OSCylinder".to_owned();
                let args_iter = std::mem::take(&mut self.args);
                //predefine args with default values, in this case [h=1.0, r1=1.0, r2=1.0, center=false]
                self.args.push(ScadArg::Expr(ScadExpr::Literal {
                    span: Span::empty(),
                    lit: ScadLiteral::Float(1.0),
                }));
                self.args.push(ScadArg::Expr(ScadExpr::Literal {
                    span: Span::empty(),
                    lit: ScadLiteral::Float(1.0),
                }));
                self.args.push(ScadArg::Expr(ScadExpr::Literal {
                    span: Span::empty(),
                    lit: ScadLiteral::Float(1.0),
                }));
                self.args.push(ScadArg::Expr(ScadExpr::Literal {
                    span: Span::empty(),
                    lit: ScadLiteral::Boolean(false),
                }));

                let mut expr_index = 0;
                for arg in args_iter {
                    match arg {
                        ScadArg::Expr(mut expr) => {
                            //normalize before using
                            expr.normalize_as_arg();
                            self.args[expr_index] = ScadArg::Expr(expr);
                            expr_index += 1;
                        }
                        ScadArg::Assign(assign) => match assign.var.0.as_str() {
                            "h" => {
                                self.args[0] = ScadArg::Expr(*assign.expr);
                            }
                            "r" => {
                                let mut r_expr = *assign.expr.clone();
                                r_expr.normalize_as_arg();
                                self.args[1] = ScadArg::Expr(r_expr.clone());
                                self.args[2] = ScadArg::Expr(r_expr);
                            }
                            "r1" => {
                                let mut r_expr = *assign.expr.clone();
                                r_expr.normalize_as_arg();
                                self.args[1] = ScadArg::Expr(r_expr);
                            }
                            "r2" => {
                                let mut r_expr = *assign.expr.clone();
                                r_expr.normalize_as_arg();
                                self.args[2] = ScadArg::Expr(r_expr);
                            }
                            "d" => {
                                //Diameter is double radius, so half it
                                let mut d_expr = ScadArg::Expr(ScadExpr::Binary {
                                    span: Span::empty(),
                                    op: vola_ast::alge::BinaryOp::Mul,
                                    left: assign.expr,
                                    right: Box::new(ScadExpr::Literal {
                                        span: Span::empty(),
                                        lit: ScadLiteral::Float(0.5),
                                    }),
                                });
                                d_expr.normalize_as_arg();
                                self.args[1] = d_expr.clone();
                                self.args[2] = d_expr.clone();
                            }
                            "d1" => {
                                //Diameter is double radius, so half it
                                let mut d_expr = ScadArg::Expr(ScadExpr::Binary {
                                    span: Span::empty(),
                                    op: vola_ast::alge::BinaryOp::Mul,
                                    left: assign.expr,
                                    right: Box::new(ScadExpr::Literal {
                                        span: Span::empty(),
                                        lit: ScadLiteral::Float(0.5),
                                    }),
                                });
                                d_expr.normalize_as_arg();
                                self.args[1] = d_expr.clone();
                            }
                            "d2" => {
                                //Diameter is double radius, so half it
                                let mut d_expr = ScadArg::Expr(ScadExpr::Binary {
                                    span: Span::empty(),
                                    op: vola_ast::alge::BinaryOp::Mul,
                                    left: assign.expr,
                                    right: Box::new(ScadExpr::Literal {
                                        span: Span::empty(),
                                        lit: ScadLiteral::Float(0.5),
                                    }),
                                });
                                d_expr.normalize_as_arg();
                                self.args[2] = d_expr.clone();
                            }
                            "center" => {
                                //overwrite second
                                self.args[3] = ScadArg::Expr(*assign.expr);
                            }
                            other => {
                                report_here(
                                    format!("unknown named argument '{other}' for cylinder!"),
                                    assign.span,
                                );
                                return Err(ParserError::MalformedNode(
                                    "Unknown named argument".to_owned(),
                                ));
                            }
                        },
                    }
                }
            }
            "translate" => {
                self.function.0 = "OSTranslate".to_owned();
                for arg in &mut self.args {
                    arg.normalize_as_arg();
                }
            }
            "rotate" => {
                self.function.0 = "OSRotate".to_owned();
                for arg in &mut self.args {
                    arg.normalize_as_arg();
                }
            }
            "union" => {
                self.function.0 = "OSUnion".to_owned();
            }
            "difference" => {
                self.function.0 = "OSDifference".to_owned();
            }
            "intersection" => {
                self.function.0 = "OSIntersection".to_owned();
                for arg in &mut self.args {
                    arg.normalize_as_arg();
                }
            }
            _ => {}
        }

        Ok(())
    }
}

impl ScadBlock {
    ///Recursively normalizes this, and all sub-blocks by moving all _last_ variables assignments
    ///to the top of the block.
    pub fn normalize(&mut self) -> Result<(), ParserError> {
        //filter out all assignments, and put them on the top of the block.
        //we do that in reverse, in order to only keep the _last-write_, which is
        //(more or less) what Scad does internally.

        //now iterate through the assignments, and only keep the last one for each variable
        //NOTE: the indirection lets us keep the initial order, which we need, (later), to find undefined
        //      variables at compile-time (of vola). In OpenScad those would _just_ be undefined.
        let mut assign_map = AHashMap::default();
        let mut assigns = Vec::new();
        //filter all assigns
        self.stmts = std::mem::take(&mut self.stmts)
            .into_iter()
            .filter_map(|x| match x {
                ScadStmt::Assign(a) => {
                    let index = if let Some(known_index) = assign_map.get(&a.var.0) {
                        *known_index
                    } else {
                        let new_index = assigns.len();
                        assign_map.insert(a.var.0.clone(), new_index);
                        //Init slot
                        assigns.push(ScadStmt::None);
                        new_index
                    };
                    //overwrite index of assign
                    assigns[index] = ScadStmt::Assign(a);

                    None
                }
                other => Some(other),
            })
            .collect();

        //now prepend all _kept_ assigns
        assigns.append(&mut self.stmts);
        assert!(self.stmts.len() == 0);
        self.stmts = assigns;

        //finally recurse on any block_carrying stmt
        for stmt in &mut self.stmts {
            stmt.normalize()?;
        }

        //Alright, finished
        Ok(())
    }
}

impl ScadStmt {
    pub fn normalize(&mut self) -> Result<(), ParserError> {
        match self {
            Self::Assert | Self::Assign(_) | Self::None | Self::IncludeStmt(_) => Ok(()),
            Self::Overwrite {
                overwrites: _,
                block,
            } => block.normalize(),
            Self::IfBlock {
                head_span: _,
                condition: _,
                consequence,
                alternative,
            } => {
                consequence.normalize()?;
                if let Some(alt) = alternative {
                    alt.normalize()?;
                }
                Ok(())
            }
            Self::ForBlock { block, .. } => block.normalize(),

            Self::Comment(comment) => {
                comment.content = format!("//{}", comment.content);
                Ok(())
            }
            Self::Chain { chain, span: _ } => {
                for element in chain {
                    match element {
                        crate::scad_ast::ChainElement::Block(b) => b.normalize()?,
                        crate::scad_ast::ChainElement::Call(c) => c.normalize()?,
                    }
                }
                Ok(())
            }
        }
    }
}
