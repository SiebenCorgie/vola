/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use vola_common::dot::{
    graphviz_rust::{
        cmd::Format,
        exec_dot,
        printer::{DotPrinter, PrinterContext},
    },
    DotNode, GraphvizBuilder,
};

use crate::{
    alge::{AssignStmt, EvalExpr, Expr, ExprTy, FieldAccessor, Func, LetStmt},
    common::{Block, Branch, CTArg, Call, Loop, Stmt, TypedIdent},
    csg::{CSGConcept, CsgDef, CsgStmt, CsgTy, ImplBlock, ScopedCall},
    AstEntry, TopLevelNode, VolaAst,
};

///Creates a Dot svg for `ast`.
pub fn ast_to_svg(ast: &VolaAst, file_name: &str) {
    let mut builder = GraphvizBuilder::new();

    for entry in &ast.entries {
        let name = format!("{:?}", entry.id());
        builder.start_graph(&name);

        builder.add_node(entry);
        builder = entry.build_children(builder);

        builder.end_graph();
    }

    //now take the graph and make the svg

    let dot = builder.graph.print(&mut PrinterContext::default());

    let format = Format::Svg;
    let graph_svg = exec_dot(dot, vec![format.into()]).unwrap();

    std::fs::write(file_name, graph_svg).unwrap();
}

impl DotNode for TopLevelNode {
    fn id(&self) -> String {
        format!("TopLevelNode {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        format!("Top Level Node")
    }
    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for ct in &self.ct_args {
            builder.add_node(ct);
            builder.connect(self, ct);
            builder = ct.build_children(builder)
        }

        builder.add_node(&self.entry);
        builder.connect(self, &self.entry);
        self.entry.build_children(builder)
    }
}

impl DotNode for AstEntry {
    fn id(&self) -> String {
        match self {
            AstEntry::Comment(s) => format!("Comment {:?}..{:?}", s.from, s.to),
            AstEntry::CsgDef(def) => def.id(),
            AstEntry::Concept(def) => def.id(),
            AstEntry::ImplBlock(b) => b.id(),
            AstEntry::Module(m) => format!("Module {:?}..{:?}", m.span.from.0, m.span.to.0),
            AstEntry::Func(f) => f.id(),
        }
    }

    fn content(&self) -> String {
        match self {
            AstEntry::Comment(_s) => format!("Comment"),
            AstEntry::CsgDef(def) => def.content(),
            AstEntry::Concept(s) => s.content(),
            AstEntry::ImplBlock(b) => b.content(),
            AstEntry::Module(_m) => format!("Module"),
            AstEntry::Func(f) => f.content(),
        }
    }

    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        match self {
            AstEntry::Comment(_s) => builder,
            AstEntry::CsgDef(def) => def.build_children(builder),
            AstEntry::Concept(s) => s.build_children(builder),
            AstEntry::ImplBlock(b) => b.build_children(builder),
            AstEntry::Module(_m) => builder,
            AstEntry::Func(f) => f.build_children(builder),
        }
    }
}

impl DotNode for Block {
    fn id(&self) -> String {
        format!("Block {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        format!("block")
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for stmt in &self.stmts {
            builder.add_node(stmt);
            builder.connect(self, stmt);

            //now recurse
            builder = stmt.build_children(builder)
        }

        if let Some(retexpr) = &self.retexpr {
            builder.add_node(retexpr);
            builder.connect(self, retexpr);
            //now recurse
            builder = retexpr.build_children(builder)
        }
        builder
    }
}

impl DotNode for Func {
    fn id(&self) -> String {
        format!("AlgeFunc {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        self.name.0.clone()
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for inp in &self.args {
            builder.add_node(inp);
            builder.connect(self, inp);

            //now recurse
            builder = inp.build_children(builder)
        }

        builder.add_node(&self.block);
        builder.connect(self, &self.block);
        //now recurse
        builder = self.block.build_children(builder);

        builder
    }
}

impl DotNode for ImplBlock {
    fn id(&self) -> String {
        format!("ImplBlock {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        format!(
            "impl {}<{}> for {}({})",
            self.dst.0,
            self.operands.len(),
            self.concept.0,
            self.concept_arg_naming.len()
        )
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        builder.add_node(&self.block);
        builder.connect(self, &self.block);
        //now recurse
        builder = self.block.build_children(builder);

        builder
    }
}

impl DotNode for Stmt {
    fn id(&self) -> String {
        match &self {
            Self::Assign(a) => a.id(),
            Self::Let(l) => l.id(),
            Self::Csg(c) => c.id(),
            Self::Loop(l) => l.id(),
            Self::Branch(b) => b.id(),
        }
    }
    fn content(&self) -> String {
        match self {
            Self::Assign(a) => a.content(),
            Self::Let(a) => a.content(),
            Self::Csg(c) => c.content(),
            Self::Loop(l) => l.content(),
            Self::Branch(b) => b.content(),
        }
    }
    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        match self {
            Self::Assign(a) => a.build_children(builder),
            Self::Let(l) => l.build_children(builder),
            Self::Csg(c) => c.build_children(builder),
            Self::Loop(l) => l.build_children(builder),
            Self::Branch(b) => b.build_children(builder),
        }
    }
}

impl DotNode for AssignStmt {
    fn id(&self) -> String {
        format!("assign {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        format!("{} = ", self.dst.0)
    }
    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        builder.add_node(&self.expr);
        builder.connect(self, &self.expr);
        self.expr.build_children(builder)
    }
}

impl DotNode for CsgStmt {
    fn id(&self) -> String {
        format!("CsgStmt {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        format!("csg {}", self.decl_name.0.clone())
    }

    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::blue
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        //build sub tree and connect to self
        builder.add_node(&self.expr);
        builder.connect(self, &self.expr);
        self.expr.build_children(builder)
    }
}

impl DotNode for LetStmt {
    fn id(&self) -> String {
        format!("LetStmt {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        format!("let {}", self.decl_name.0)
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        builder.add_node(&self.expr);
        builder.connect(self, &self.expr);
        self.expr.build_children(builder)
    }
}

impl DotNode for EvalExpr {
    fn id(&self) -> String {
        format!("EvalExpr {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        format!("eval {} ", self.evaluator.0)
    }
    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for arg in &self.params {
            builder.add_node(arg);
            builder.connect(self, arg);
            builder = arg.build_children(builder);
        }

        builder
    }
}

impl DotNode for Branch {
    fn id(&self) -> String {
        format!("GammaExpr {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        format!("if-then<{}>-else<{}>", 1, self.unconditional.is_some())
    }

    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::orange
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        builder.add_node(&self.conditional.0);
        builder.add_node(self.conditional.1.as_ref());
        builder.connect(self, &self.conditional.0);
        builder.connect(self, self.conditional.1.as_ref());
        builder = self.conditional.0.build_children(builder);
        builder = self.conditional.1.build_children(builder);

        if let Some(uncond) = &self.unconditional {
            builder.add_node(uncond.as_ref());
            builder.connect(self, uncond.as_ref());
            builder = uncond.build_children(builder);
        }

        builder
    }
}

impl DotNode for Loop {
    fn id(&self) -> String {
        format!("ThetaExpr {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        format!("theta-over-{}", self.iteration_variable_ident.0)
    }

    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::orange
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        builder.add_node(&self.bound_lower);
        builder.connect(self, &self.bound_lower);
        builder = self.bound_lower.build_children(builder);

        builder.add_node(&self.bound_upper);
        builder.connect(self, &self.bound_upper);
        builder = self.bound_upper.build_children(builder);

        builder.add_node(self.body.as_ref());
        builder.connect(self, self.body.as_ref());
        //now recurse
        builder = self.body.build_children(builder);

        builder
    }
}

impl DotNode for ScopedCall {
    fn id(&self) -> String {
        format!("ScopedCall {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        format!("Scoped call<{}>: {}", self.blocks.len(), self.call.ident.0)
    }

    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::blue
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        builder.add_node(&self.call);
        builder.connect(self, &self.call);
        builder = self.call.build_children(builder);
        for blk in &self.blocks {
            builder.add_node(blk);
            builder.connect(self, blk);

            //now recurse
            builder = blk.build_children(builder)
        }

        builder
    }
}

impl DotNode for Expr {
    fn id(&self) -> String {
        match &self.expr_ty {
            ExprTy::EvalExpr(e) => e.id(),
            ExprTy::Call(c) => c.id(),
            ExprTy::ScopedCall(s) => s.id(),
            ExprTy::BranchExpr(e) => e.id(),
            _ => format!("AlgeExpr {:?}..{:?}", self.span.from, self.span.to),
        }
    }

    fn shape(&self) -> vola_common::dot::graphviz_rust::attributes::shape {
        vola_common::dot::graphviz_rust::attributes::shape::oval
    }

    fn content(&self) -> String {
        match &self.expr_ty {
            ExprTy::Binary {
                left: _,
                right: _,
                op,
            } => format!("{:?}", op),
            ExprTy::Unary { op, operand: _ } => format!("{:?}", op),
            ExprTy::Call(call) => call.content(),
            ExprTy::EvalExpr(eex) => eex.content(),
            ExprTy::FieldAccess { src, accessors: _ } => {
                format!("Access: {}.", src.0)
            }
            ExprTy::Ident(i) => format!("{}", i.0),
            ExprTy::List(_l) => format!("List"),
            ExprTy::Literal(lit) => format!("Lit: {:?}", lit),
            ExprTy::ScopedCall(s) => s.content(),
            ExprTy::BranchExpr(e) => e.content(),
            ExprTy::SplatExpr { expr: _, count } => format!("Splat<{count}>"),
        }
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        match &self.expr_ty {
            ExprTy::Binary { left, right, op: _ } => {
                builder.add_node(left.as_ref());
                builder.add_node(right.as_ref());
                builder.connect(self, left.as_ref());
                builder.connect(self, right.as_ref());

                builder = left.build_children(builder);
                builder = right.build_children(builder);
                builder
            }
            ExprTy::Unary { op: _, operand } => {
                builder.add_node(operand.as_ref());
                builder.connect(self, operand.as_ref());
                operand.build_children(builder)
            }
            ExprTy::Call(op) => op.build_children(builder),
            ExprTy::ScopedCall(op) => op.build_children(builder),
            ExprTy::EvalExpr(e) => e.build_children(builder),
            ExprTy::FieldAccess { src: _, accessors } => {
                for param in accessors {
                    builder.add_node(param);
                    builder.connect(self, param);
                    builder = param.build_children(builder);
                }
                builder
            }
            ExprTy::List(l) => {
                for li in l {
                    builder.add_node(li);
                    builder.connect(self, li);
                    builder = li.build_children(builder);
                }
                builder
            }
            ExprTy::BranchExpr(e) => e.build_children(builder),
            ExprTy::Ident(_) | ExprTy::Literal(_) => builder,
            ExprTy::SplatExpr { expr, count: _ } => expr.build_children(builder),
        }
    }
}

impl DotNode for Call {
    fn id(&self) -> String {
        format!("Call {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        format!("call {}", self.ident.0)
    }
    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for arg in &self.args {
            builder.add_node(arg);
            builder.connect(self, arg);
            builder = arg.build_children(builder);
        }
        builder
    }
}

impl DotNode for FieldAccessor {
    fn id(&self) -> String {
        match self {
            FieldAccessor::Digit { span, .. } => {
                format!("FieldAccessor {:?}..{:?}", span.from, span.to)
            }
            FieldAccessor::Ident { span, .. } => {
                format!("FieldAccessor {:?}..{:?}", span.from, span.to)
            }
        }
    }

    fn content(&self) -> String {
        match self {
            FieldAccessor::Digit { span: _, digit } => format!("{digit}"),
            FieldAccessor::Ident { span: _, ident } => format!("{}", ident.0),
        }
    }

    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        builder
    }
}

impl DotNode for TypedIdent {
    fn id(&self) -> String {
        format!("TypedIdent {:?}..{:?}", self.span.from, self.span.to)
    }
    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::green
    }
    fn shape(&self) -> vola_common::dot::graphviz_rust::attributes::shape {
        vola_common::dot::graphviz_rust::attributes::shape::rarrow
    }
    fn content(&self) -> String {
        format!("{} : {:?}", self.ident.0, self.ty)
    }
    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        builder
    }
}

impl DotNode for CTArg {
    fn id(&self) -> String {
        format!("CompileTimeArg {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        format!("CTArg: {}", self.ident.0)
    }
    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for arg in &self.args {
            builder.add_node(arg);
            builder.connect(self, arg);
            builder = arg.build_children(builder);
        }
        builder
    }
}

impl DotNode for CsgDef {
    fn id(&self) -> String {
        format!("CSGDef {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        match self.ty {
            CsgTy::Entity => format!("entity {}", self.name.0),
            CsgTy::Operation => format!("operation {}", self.name.0),
        }
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for arg in &self.args {
            builder.add_node(arg);
            builder.connect(self, arg);
            builder = arg.build_children(builder);
        }
        builder
    }
}
impl DotNode for CSGConcept {
    fn id(&self) -> String {
        format!("CSGConcept {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        if self.src_ty.len() > 0 {
            format!(
                "concept {}: {:?} -> {:?}",
                self.name.0, self.src_ty, self.dst_ty
            )
        } else {
            format!("concept {}: -> {:?}", self.name.0, self.dst_ty)
        }
    }
    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        builder
    }
}
