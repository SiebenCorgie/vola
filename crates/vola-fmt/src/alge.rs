use vola_ast::{
    alge::{AssignStmt, EvalExpr, Expr, ExprTy, FieldAccessor, Func, LetStmt},
    common::{Call, Stmt},
    csg::ImplBlock,
};

use crate::{FormatTree, Keyword};

impl From<&Expr> for FormatTree {
    fn from(value: &Expr) -> Self {
        match &value.expr_ty {
            ExprTy::Unary { op, operand } => FormatTree::UnaryOp {
                op: format!("{}", op),
                operand: Box::new(FormatTree::from(operand.as_ref())),
            },
            ExprTy::Binary { left, right, op } => FormatTree::BinaryOp {
                left: Box::new(FormatTree::from(left.as_ref())),
                op: format!("{}", op),
                right: Box::new(FormatTree::from(right.as_ref())),
            },
            ExprTy::Eval(eval) => FormatTree::from(eval),
            ExprTy::Ident(i) => FormatTree::from(i),
            ExprTy::FieldAccess { src, accessors } => {
                let mut seq = vec![FormatTree::from(src)];
                for accessor in accessors {
                    seq.push(FormatTree::Keyword(Keyword::AccessDot));
                    seq.push(FormatTree::from(accessor));
                }

                FormatTree::Seq(seq)
            }
            ExprTy::Call(c) => FormatTree::from(c.as_ref()),
            ExprTy::ScopedCall(s) => FormatTree::from(s.as_ref()),
            ExprTy::List(l) => FormatTree::Wrapped {
                left: '[',
                right: ']',
                sub: Box::new(FormatTree::seperated_list(",", l.iter())),
            },
            ExprTy::Tuple(l) => FormatTree::Wrapped {
                left: '(',
                right: ')',
                sub: Box::new(FormatTree::seperated_list(",", l.iter())),
            },
            ExprTy::Literal(l) => FormatTree::from(l),
            ExprTy::Branch(b) => FormatTree::from(b.as_ref()),
            ExprTy::Splat { expr, count } => FormatTree::Wrapped {
                left: '[',
                right: ']',
                sub: Box::new(FormatTree::Seq(vec![
                    FormatTree::from(expr.as_ref()),
                    FormatTree::Token(";".to_owned()),
                    FormatTree::Token(format!("{count}")),
                ])),
            },
            ExprTy::Cast { span: _, expr, ty } => FormatTree::Seq(vec![
                FormatTree::from(expr.as_ref()),
                FormatTree::Keyword(crate::Keyword::Cast),
                FormatTree::from(ty),
            ]),
        }
    }
}

impl From<&FieldAccessor> for FormatTree {
    fn from(value: &FieldAccessor) -> Self {
        match value {
            FieldAccessor::Digit { span: _, digit } => FormatTree::Token(format!("{digit}")),
            FieldAccessor::Ident { span: _, ident } => FormatTree::Token(ident.0.clone()),
        }
    }
}

impl From<&LetStmt> for FormatTree {
    fn from(value: &LetStmt) -> Self {
        FormatTree::Seq(vec![
            FormatTree::Keyword(crate::Keyword::Let),
            FormatTree::Token(value.decl_name.0.clone()),
            FormatTree::Keyword(crate::Keyword::Equal),
            FormatTree::from(&value.expr),
            FormatTree::StmtEnd,
        ])
    }
}

impl From<&AssignStmt> for FormatTree {
    fn from(value: &AssignStmt) -> Self {
        FormatTree::Seq(vec![
            FormatTree::Token(value.dst.0.clone()),
            FormatTree::Keyword(crate::Keyword::Equal),
            FormatTree::from(&value.expr),
            FormatTree::StmtEnd,
        ])
    }
}

impl From<&EvalExpr> for FormatTree {
    fn from(value: &EvalExpr) -> Self {
        let eval_call = Call {
            span: value.span.clone(),
            ident: value.concept.clone(),
            args: value.params.iter().cloned().collect(),
        };
        FormatTree::Seq(vec![
            FormatTree::Keyword(crate::Keyword::Eval),
            FormatTree::Token(value.evaluator.0.clone()),
            FormatTree::Keyword(Keyword::AccessDot),
            FormatTree::from(&eval_call),
        ])
    }
}

impl From<&ImplBlock> for FormatTree {
    fn from(value: &ImplBlock) -> Self {
        //format tree is a header, followed by a block... usually.

        let mut header_builder = vec![
            FormatTree::Keyword(Keyword::Impl),
            FormatTree::from(&value.dst),
        ];

        if value.operands.len() > 0 {
            header_builder.push(FormatTree::Wrapped {
                left: '<',
                right: '>',
                sub: Box::new(FormatTree::seperated_list(",", value.operands.iter())),
            });
        }
        header_builder.push(FormatTree::Keyword(Keyword::For));
        header_builder.push(FormatTree::from(&value.concept));
        header_builder.push(FormatTree::Wrapped {
            left: '(',
            right: ')',
            sub: Box::new(FormatTree::from(&value.concept_arg_name)),
        });

        FormatTree::Seq(vec![
            //Header sequence
            FormatTree::Seq(header_builder),
            //the main block
            FormatTree::from(&value.block),
            FormatTree::Space,
        ])
    }
}

impl From<&Stmt> for FormatTree {
    fn from(value: &Stmt) -> Self {
        match value {
            Stmt::Let(l) => FormatTree::from(l),
            Stmt::Assign(a) => FormatTree::from(a),
            Stmt::Csg(csg) => FormatTree::from(csg),
            Stmt::Loop(l) => FormatTree::from(l),
            Stmt::Branch(b) => FormatTree::from(b),
            Stmt::Comment(c) => FormatTree::from(c),
            Stmt::Block(b) => FormatTree::from(b.as_ref()),
        }
    }
}

impl From<&Func> for FormatTree {
    fn from(value: &Func) -> Self {
        //similar to impl block, we have the header part,
        // and the block part
        let mut header_list = Vec::new();
        if value.is_export {
            header_list.push(FormatTree::Keyword(Keyword::Export));
        }
        header_list.push(FormatTree::Keyword(Keyword::Fn));
        header_list.push(FormatTree::from(&value.name));
        header_list.push(FormatTree::Wrapped {
            left: '(',
            right: ')',
            sub: Box::new(FormatTree::seperated_list(",", value.args.iter())),
        });
        header_list.push(FormatTree::Keyword(Keyword::ResultArrow));
        header_list.push(FormatTree::from(&value.return_type));

        //assemble the actual stmt block, and the header
        FormatTree::Seq(vec![
            FormatTree::Seq(header_list),
            FormatTree::from(&value.block),
            FormatTree::Space,
        ])
    }
}
