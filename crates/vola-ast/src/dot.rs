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
    alge::{AlgeFunc, AssignStmt, EvalExpr, Expr, ExprTy, FieldAccessor, ImplBlock, LetStmt},
    common::{Block, CTArg, Call, GammaExpr, Stmt, ThetaExpr, TypedIdent},
    csg::{AccessDesc, CSGConcept, CSGNodeDef, CSGNodeTy, CsgStmt, ExportFn, FieldDef, ScopedCall},
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
            AstEntry::CSGNodeDef(def) => def.id(),
            AstEntry::Concept(def) => def.id(),
            AstEntry::ImplBlock(b) => b.id(),
            AstEntry::FieldDefine(fd) => fd.id(),
            AstEntry::ExportFn(ef) => ef.id(),
            AstEntry::Module(m) => format!("Module {:?}..{:?}", m.span.from.0, m.span.to.0),
            AstEntry::AlgeFunc(f) => f.id(),
        }
    }

    fn content(&self) -> String {
        match self {
            AstEntry::Comment(_s) => format!("Comment"),
            AstEntry::CSGNodeDef(def) => def.content(),
            AstEntry::Concept(s) => s.content(),
            AstEntry::ImplBlock(b) => b.content(),
            AstEntry::FieldDefine(fd) => fd.content(),
            AstEntry::ExportFn(ef) => ef.content(),
            AstEntry::Module(_m) => format!("Module"),
            AstEntry::AlgeFunc(f) => f.content(),
        }
    }

    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        match self {
            AstEntry::Comment(_s) => builder,
            AstEntry::CSGNodeDef(def) => def.build_children(builder),
            AstEntry::Concept(s) => s.build_children(builder),
            AstEntry::ImplBlock(b) => b.build_children(builder),
            AstEntry::FieldDefine(fd) => fd.build_children(builder),
            AstEntry::ExportFn(ef) => ef.build_children(builder),
            AstEntry::Module(_m) => builder,
            AstEntry::AlgeFunc(f) => f.build_children(builder),
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

impl DotNode for FieldDef {
    fn id(&self) -> String {
        format!("FieldDef {:?}..{:?}", self.span.from, self.span.to)
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
        self.block.build_children(builder)
    }
}

impl DotNode for ExportFn {
    fn id(&self) -> String {
        format!("ExportFn {:?}..{:?}", self.span.from, self.span.to)
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
        self.block.build_children(builder)
    }
}

impl DotNode for AlgeFunc {
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

impl DotNode for AccessDesc {
    fn id(&self) -> String {
        format!("AccessDecs {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        format!("AccessDesc")
    }
    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::red
    }
    fn shape(&self) -> vola_common::dot::graphviz_rust::attributes::shape {
        vola_common::dot::graphviz_rust::attributes::shape::rarrow
    }
    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for sib in &self.evals {
            builder.add_node(sib);
            builder.connect(self, sib);

            //now recurse
            builder = sib.build_children(builder)
        }
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
            Self::ThetaExpr => todo!(),
        }
    }
    fn content(&self) -> String {
        match self {
            Self::Assign(a) => a.content(),
            Self::Let(a) => a.content(),
            Self::Csg(c) => c.content(),
            Self::ThetaExpr => todo!(),
        }
    }
    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        match self {
            Self::Assign(a) => a.build_children(builder),
            Self::Let(l) => l.build_children(builder),
            Self::Csg(c) => c.build_children(builder),
            Self::ThetaExpr => todo!(),
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

impl DotNode for GammaExpr {
    fn id(&self) -> String {
        format!("GammaExpr {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        format!(
            "if-then<{}>-else<{}>",
            self.conditionals.len() - 1,
            self.unconditional.is_some()
        )
    }

    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::orange
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for (expr, block) in &self.conditionals {
            builder.add_node(expr);
            builder.add_node(block);
            builder.connect(self, expr);
            builder.connect(self, block);
            builder = expr.build_children(builder);
            builder = block.build_children(builder);
        }

        if let Some(uncond) = &self.unconditional {
            builder.add_node(uncond);
            builder.connect(self, uncond);
            builder = uncond.build_children(builder);
        }

        builder
    }
}

impl DotNode for ThetaExpr {
    fn id(&self) -> String {
        format!("ThetaExpr {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        format!("theta-over-{}", self.initial_assignment.dst.0)
    }

    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::orange
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        builder.add_node(&self.initial_assignment);
        builder.connect(self, &self.initial_assignment);
        builder = self.initial_assignment.build_children(builder);

        builder.add_node(&self.bound_lower);
        builder.connect(self, &self.bound_lower);
        builder = self.bound_lower.build_children(builder);

        builder.add_node(&self.bound_upper);
        builder.connect(self, &self.bound_upper);
        builder = self.bound_upper.build_children(builder);

        builder.add_node(&self.body);
        builder.connect(self, &self.body);
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
            ExprTy::GammaExpr(e) => e.id(),
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
            ExprTy::AccessExpr(_e) => format!("AccessDesc"),
            ExprTy::GammaExpr(e) => e.content(),
            ExprTy::ThetaExpr(t) => t.content(),
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
            ExprTy::AccessExpr(ae) => {
                for expr in &ae.evals {
                    builder.add_node(expr);
                    builder.connect(self, expr);
                    builder = expr.build_children(builder);
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
            ExprTy::GammaExpr(e) => e.build_children(builder),
            ExprTy::ThetaExpr(t) => t.build_children(builder),
            ExprTy::Ident(_) | ExprTy::Literal(_) => builder,
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

impl DotNode for CSGNodeDef {
    fn id(&self) -> String {
        format!("CSGDef {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        match self.ty {
            CSGNodeTy::Entity => format!("entity {}", self.name.0),
            CSGNodeTy::Operation => format!("operation {}", self.name.0),
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
