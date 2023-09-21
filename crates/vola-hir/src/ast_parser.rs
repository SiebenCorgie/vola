use vola_ast::{
    alge::AlgeExpr,
    comb::OpNode,
    common::{Alge, Field, Keyword, Op, Prim, PrimBlock, Stmt},
    Ast,
};

use crate::{
    AlgeNode, AlgeOp, CombNode, CombOp, EntryPointType, Ident, ModuleBuilder, NodeRef,
    RegionBuilder,
};

pub fn parse_alge_expr<'a>(exprs: &AlgeExpr, builder: &mut RegionBuilder<'a>) -> NodeRef {
    match exprs {
        AlgeExpr::Identifier(ident) => {
            let ident = crate::Ident::from(ident.0.clone());
            //Try to resolve immediatly, otherwise use ref
            if let Some(node) = builder.module.symbols.resolve(&ident) {
                node
            } else {
                builder
                    .module()
                    .new_node(AlgeNode::new(AlgeOp::Ref(ident.0.clone().into())))
            }
        }
        AlgeExpr::List(list) => {
            let mut list_node = AlgeNode::new(AlgeOp::List);
            for item in list {
                let item_ref = parse_alge_expr(item, builder);
                list_node.in_args.push(item_ref);
            }

            builder.module().new_node(list_node)
        }
        AlgeExpr::BinOp { op, left, right } => {
            let left = parse_alge_expr(left, builder);
            let right = parse_alge_expr(right, builder);
            builder.module().new_node(
                AlgeNode::new(AlgeOp::BinOp(op.clone()))
                    .with_arg(left)
                    .with_arg(right),
            )
        }
        AlgeExpr::UnaryOp { op, expr } => {
            let expr = parse_alge_expr(expr, builder);
            builder
                .module()
                .new_node(AlgeNode::new(AlgeOp::UnOp(op.clone())).with_arg(expr))
        }
        AlgeExpr::Call { ident, args } => {
            let mut call_node = AlgeNode::new(AlgeOp::Call(ident.0.clone().into()));
            for item in args {
                let item_ref = parse_alge_expr(item, builder);
                call_node.in_args.push(item_ref);
            }

            builder.module().new_node(call_node)
        }
        AlgeExpr::PrimAccess { ident, field } => {
            let node = AlgeNode::new(AlgeOp::FieldAccess(field.0.clone().into())).with_child(
                builder
                    .module()
                    .symbols
                    .resolve(&ident.0.as_str().into())
                    .expect("FieldAccess node was not declared before"),
            );
            //find the reference

            builder.module().new_node(node)
        }

        AlgeExpr::Kw(kw) => {
            match kw {
                Keyword::KwAt => {
                    //referencen the regions at
                    builder.get_at()
                }
                _ => {
                    panic!("Keyword {:?} undefined in AlgeScope!", kw)
                }
            }
        }
        AlgeExpr::Float(imm) => builder
            .module()
            .new_node(AlgeNode::new(AlgeOp::Imm(imm.clone()))),
    }
}

///Parses the single statment, returning its node
pub fn parse_stmt<'a>(stmt: &Stmt, builder: &mut RegionBuilder<'a>) -> NodeRef {
    match stmt {
        Stmt::AtAssign { assign_op, expr } => {
            //Rewrite at node to be calculation
            if let Some(assign_op) = assign_op {
                let assign_expr = parse_alge_expr(expr, builder);

                let old_at = builder.get_at();

                //wrap into the op
                let wrapped = builder.module().new_node(
                    AlgeNode::new(AlgeOp::BinOp(assign_op.clone()))
                        .with_arg(old_at)
                        .with_arg(assign_expr),
                );
                builder.at_ref = wrapped;
            } else {
                builder.at_ref = parse_alge_expr(expr, builder);
            }

            builder.get_at()
        }
        Stmt::FieldAssign {
            prim,
            field,
            assign_op,
            expr,
        } => {
            let primref = if let Some(nref) = builder
                .module()
                .symbols
                .resolve(&Ident::from(prim.0.as_str()))
            {
                nref
            } else {
                panic!("Unknown reference to primitive {} in scope", prim.0);
            };

            let subarg = parse_alge_expr(expr, builder);
            let mutate_node = builder.module().new_node(
                CombNode::new(CombOp::PrimFieldMutate {
                    ident: field.0.as_str().into(), // NOTE currently hardcoding to "AT"
                    op: assign_op.clone(),
                })
                .with_child(primref)
                .with_arg(subarg),
            );

            //now push mutation as child to primref
            builder
                .module()
                .nodes
                .get_mut(primref)
                .unwrap()
                .unwrap_comb_node()
                .in_children
                .push(mutate_node);

            mutate_node
        }
        Stmt::LetStmt { ident, expr } => {
            //let statement is a algebraic expression that is bound to the identifier.
            let algeexpr = parse_alge_expr(expr, builder);
            builder
                .module()
                .symbols
                .push_ref(ident.ident.0.as_str(), algeexpr);
            algeexpr
        }
        Stmt::PrimAtAssign {
            prim,
            assign_op,
            expr,
        } => {
            //Rewrite as "AT" field assignment
            parse_stmt(
                &Stmt::FieldAssign {
                    prim: prim.clone(),
                    field: "At".into(),
                    assign_op: assign_op.clone(),
                    expr: expr.clone(),
                },
                builder,
            )
        }
        Stmt::PrimDef { ident, init } => {
            let mut node = CombNode::new(CombOp::PrimDef(ident.0.as_str().into()));

            if let Some(init) = init {
                node = node.with_child(parse_op_node(init, builder));
            }

            let node_ref = builder.module().new_node(node);

            builder
                .module()
                .symbols
                .push_ref(ident.0.as_str(), node_ref);
            node_ref
        }
    }
}

pub fn parse_op_node<'a>(node: &OpNode, builder: &mut RegionBuilder<'a>) -> NodeRef {
    match node {
        OpNode::OpCall { ident, args, prims } => {
            let mut node = CombNode::new(CombOp::OpCall(ident.0.as_str().into()));

            for arg in args {
                node = node.with_arg(parse_alge_expr(arg, builder));
            }

            for prim in prims {
                node = node.with_child(parse_op_node(prim, builder));
            }

            builder.module().new_node(node)
        }
        OpNode::Prim {
            prim_call_ident,
            args,
        } => {
            let mut node = CombNode::new(CombOp::PrimCall(prim_call_ident.0.as_str().into()));

            for arg in args {
                node = node.with_arg(parse_alge_expr(arg, builder));
            }

            builder.module().new_node(node)
        }
        OpNode::PrimIdent(ident) => {
            //in the case of an ident, try to get the primitive for that ident, if not available, retrun the ref
            if let Some(nref) = builder
                .module()
                .symbols
                .resolve(&Ident::from(ident.0.as_str()))
            {
                nref
            } else {
                builder
                    .module()
                    .new_node(CombNode::new(CombOp::PrimCall(ident.0.as_str().into())))
            }
        }
    }
}

pub fn parse_block<'a>(prim: &PrimBlock, builder: &mut RegionBuilder<'a>) -> NodeRef {
    //First parse all statements
    for stmt in &prim.stmt_list {
        let _stmt_node = parse_stmt(stmt, builder);
    }

    //Now build the op-tree for that prim
    parse_op_node(&prim.op_tree, builder)
}

pub fn parse_ast(ast: Ast, mut builder: ModuleBuilder) -> ModuleBuilder {
    assert!(builder.symbols.is_top_scope());

    for (ident, alge) in ast.alges {
        assert!(builder.symbols.is_top_scope());
        let Alge { ident, args, ret } = alge;
        builder.new_entrypoint(ident.0, EntryPointType::Alge, |mut b| {
            for arg in args {
                //TODO add types
                let _argid = b.register_arg(arg.ident.0);
            }

            let alge_root = parse_alge_expr(&ret, &mut b);

            b.set_out_node(alge_root);

            b
        });
    }

    for (ident, prim) in ast.prims {
        assert!(builder.symbols.is_top_scope());
        builder.new_entrypoint(ident.0, EntryPointType::Prim, |mut b| {
            let Prim { ident, args, block } = prim;
            for arg in args {
                //TODO add types
                let _argid = b.register_arg(arg.ident.0);
            }

            let prim_root = parse_block(&block, &mut b);

            b.set_out_node(prim_root);
            b
        });
    }

    for (ident, op) in ast.ops {
        let Op {
            ident,
            prims,
            args,
            block,
        } = op;
        builder.new_entrypoint(ident.0, EntryPointType::Op, |mut b| {
            for arg in args {
                let _argid = b.register_arg(arg.ident.0);
            }

            for argop in prims {
                let _opid = b.register_arg_prim(argop.0);
            }

            let op_root = parse_block(&block, &mut b);

            b.set_out_node(op_root);

            b
        })
    }

    for (ident, field) in ast.fields {
        let Field { ident, args, block } = field;
        builder.new_entrypoint(ident.0, EntryPointType::Field, |mut b| {
            for arg in args {
                let _argid = b.register_arg(arg.ident.0);
            }

            let op_root = parse_block(&block, &mut b);

            b.set_out_node(op_root);

            b
        })
    }

    builder
}
