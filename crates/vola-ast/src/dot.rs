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
    alge::{
        AlgeExpr, AlgeExprTy, AlgeFunc, AlgeStmt, AssignStmt, EvalExpr, FieldAccessor, ImplBlock,
        LetStmt,
    },
    common::{CTArg, Call, TypedIdent},
    csg::{
        AccessDesc, CSGBinding, CSGConcept, CSGNodeDef, CSGNodeTy, CSGOp, CSGStmt, ExportFn,
        FieldDef,
    },
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
            AstEntry::Module(m) => format!("Module({:?})", m.path),
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
            AstEntry::Module(m) => format!("Module: {:?}", m.path),
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

impl DotNode for FieldDef {
    fn id(&self) -> String {
        format!("FieldDef {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        self.name.0.clone()
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for inp in &self.inputs {
            builder.add_node(inp);
            builder.connect(self, inp);

            //now recurse
            builder = inp.build_children(builder)
        }

        //Add all sub nodes and connect them to our selfs
        for stmt in &self.stmts {
            builder.add_node(stmt);
            builder.connect(self, stmt);

            //now recurse
            builder = stmt.build_children(builder)
        }

        builder.add_node(&self.ret);
        builder.connect(self, &self.ret);

        //now recurse
        self.ret.build_children(builder)
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
        for inp in &self.inputs {
            builder.add_node(inp);
            builder.connect(self, inp);

            //now recurse
            builder = inp.build_children(builder)
        }
        //Add all sub nodes and connect them to our selfs
        for stmt in &self.stmts {
            builder.add_node(stmt);
            builder.connect(self, stmt);

            //now recurse
            builder = stmt.build_children(builder)
        }

        //Finally add accessors
        for acc in &self.access_descriptors {
            builder.add_node(acc);
            builder.connect(self, acc);

            //now recurse
            builder = acc.build_children(builder)
        }

        builder
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
        //Add all sub nodes and connect them to our selfs
        for stmt in &self.stmts {
            builder.add_node(stmt);
            builder.connect(self, stmt);

            //now recurse
            builder = stmt.build_children(builder)
        }

        builder.add_node(&self.return_expr);
        builder.connect(self, &self.return_expr);

        //now recurse
        builder = self.return_expr.build_children(builder);

        builder
    }
}

impl DotNode for AccessDesc {
    fn id(&self) -> String {
        format!("AccessDecs {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        format!("{}.{}", self.tree_ref.0, self.call.ident.0)
    }
    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::red
    }
    fn shape(&self) -> vola_common::dot::graphviz_rust::attributes::shape {
        vola_common::dot::graphviz_rust::attributes::shape::rarrow
    }
    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for sib in &self.call.args {
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
        //Add all sub nodes and connect them to our selfs
        for stmt in &self.stmts {
            builder.add_node(stmt);
            builder.connect(self, stmt);
            builder = stmt.build_children(builder)
        }

        //Finally add accessors
        builder.add_node(&self.return_expr);
        builder.connect(self, &self.return_expr);
        builder = self.return_expr.build_children(builder);

        builder
    }
}

impl DotNode for AlgeStmt {
    fn id(&self) -> String {
        match &self {
            AlgeStmt::Assign(a) => a.id(),
            AlgeStmt::Let(l) => l.id(),
        }
    }
    fn content(&self) -> String {
        match self {
            AlgeStmt::Assign(a) => a.content(),
            AlgeStmt::Let(a) => a.content(),
        }
    }
    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        match self {
            AlgeStmt::Assign(a) => a.build_children(builder),
            AlgeStmt::Let(l) => l.build_children(builder),
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

impl DotNode for CSGStmt {
    fn id(&self) -> String {
        match self {
            CSGStmt::CSGBinding(b) => b.id(),
            CSGStmt::LetStmt(l) => l.id(),
        }
    }

    fn content(&self) -> String {
        match self {
            CSGStmt::CSGBinding(b) => b.content(),
            CSGStmt::LetStmt(l) => l.content(),
        }
    }

    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        match self {
            CSGStmt::CSGBinding(_) => vola_common::dot::graphviz_rust::attributes::color_name::blue,
            CSGStmt::LetStmt(_) => vola_common::dot::graphviz_rust::attributes::color_name::black,
        }
    }

    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        match self {
            CSGStmt::CSGBinding(b) => b.build_children(builder),
            CSGStmt::LetStmt(l) => l.build_children(builder),
        }
    }
}

impl DotNode for CSGBinding {
    fn id(&self) -> String {
        format!("CSGBinding {:?}..{:?}", self.span.from, self.span.to)
    }

    fn content(&self) -> String {
        format!("csg {}", self.decl_name.0.clone())
    }

    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::blue
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        //build sub tree and connect to self
        builder.add_node(&self.tree);
        builder.connect(self, &self.tree);
        self.tree.build_children(builder)
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

impl DotNode for CSGOp {
    fn id(&self) -> String {
        format!("CSGOp {:?}..{:?}", self.span.from, self.span.to)
    }
    fn content(&self) -> String {
        format!("{}", self.op.0)
    }

    fn color(&self) -> vola_common::dot::graphviz_rust::attributes::color_name {
        vola_common::dot::graphviz_rust::attributes::color_name::blue
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        for stmt in &self.sub_trees {
            builder.add_node(stmt);
            builder.connect(self, stmt);

            //now recurse
            builder = stmt.build_children(builder)
        }

        for arg in &self.args {
            builder.add_node(arg);
            builder.connect(self, arg);

            builder = arg.build_children(builder);
        }

        builder
    }
}

impl DotNode for AlgeExpr {
    fn id(&self) -> String {
        match &self.expr_ty {
            AlgeExprTy::EvalExpr(e) => e.id(),
            AlgeExprTy::Call(c) => c.id(),
            _ => format!("AlgeExpr {:?}..{:?}", self.span.from, self.span.to),
        }
    }

    fn shape(&self) -> vola_common::dot::graphviz_rust::attributes::shape {
        vola_common::dot::graphviz_rust::attributes::shape::oval
    }

    fn content(&self) -> String {
        match &self.expr_ty {
            AlgeExprTy::Binary {
                left: _,
                right: _,
                op,
            } => format!("{:?}", op),
            AlgeExprTy::Unary { op, operand: _ } => format!("{:?}", op),
            AlgeExprTy::Call(call) => call.content(),
            AlgeExprTy::EvalExpr(eex) => eex.content(),
            AlgeExprTy::FieldAccess { src, accessors: _ } => {
                format!("Access: {}.", src.0)
            }
            AlgeExprTy::Ident(i) => format!("{}", i.0),
            AlgeExprTy::List(_l) => format!("List"),
            AlgeExprTy::Literal(lit) => format!("Lit: {:?}", lit),
        }
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        match &self.expr_ty {
            AlgeExprTy::Binary { left, right, op: _ } => {
                builder.add_node(left.as_ref());
                builder.add_node(right.as_ref());
                builder.connect(self, left.as_ref());
                builder.connect(self, right.as_ref());

                builder = left.build_children(builder);
                builder = right.build_children(builder);
                builder
            }
            AlgeExprTy::Unary { op: _, operand } => {
                builder.add_node(operand.as_ref());
                builder.connect(self, operand.as_ref());
                operand.build_children(builder)
            }
            AlgeExprTy::Call(op) => op.build_children(builder),
            AlgeExprTy::EvalExpr(e) => e.build_children(builder),
            AlgeExprTy::FieldAccess { src: _, accessors } => {
                for param in accessors {
                    builder.add_node(param);
                    builder.connect(self, param);
                    builder = param.build_children(builder);
                }
                builder
            }
            AlgeExprTy::List(l) => {
                for li in l {
                    builder.add_node(li);
                    builder.connect(self, li);
                    builder = li.build_children(builder);
                }
                builder
            }
            AlgeExprTy::Ident(_) | AlgeExprTy::Literal(_) => builder,
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
