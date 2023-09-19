use vola_ast::{
    alge::AlgeExpr,
    common::{Alge, Keyword},
    Ast,
};

use crate::{AlgeNode, AlgeOp, EntryPointType, ModuleBuilder, NodeRef, RegionBuilder};

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
            builder.module().new_node(AlgeNode::new(AlgeOp::PrimAccess {
                prim: ident.0.clone().into(),
                accessed: field.0.clone().into(),
            }))
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

pub fn parse_ast(ast: Ast, mut builder: ModuleBuilder) -> ModuleBuilder {
    for (ident, alge) in ast.alges {
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

    builder
}
