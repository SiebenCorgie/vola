use std::hash::BuildHasher;

use ahash::AHasher;
use vola_common::dot::{
    graphviz_rust::{
        cmd::Format,
        exec_dot,
        printer::{DotPrinter, PrinterContext},
    },
    DotNode, GraphvizBuilder,
};

use crate::{
    alge::{AlgeExpr, LetStmt},
    common::TypedIdent,
    csg::{AccessDesc, CSGBinding, CSGOp, CSGStmt, ExportFn, FieldDef},
    AstEntry, VolaAst,
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

    println!("DOT STRING:\n{}", dot);

    let format = Format::Svg;
    let graph_svg = exec_dot(dot, vec![format.into()]).unwrap();

    std::fs::write(file_name, graph_svg).unwrap();
}

impl DotNode for AstEntry {
    fn id(&self) -> String {
        match self {
            AstEntry::Comment(s) => format!("Comment {:?}..{:?}", s.from, s.to),
            AstEntry::Entity(s) => format!("Entity {:?}..{:?}", s.from, s.to),
            AstEntry::Concept(s) => format!("Concept {:?}..{:?}", s.from, s.to),
            AstEntry::Operation(s) => format!("Operation {:?}..{:?}", s.from, s.to),
            AstEntry::ImplBlock(s) => format!("ImplBlock {:?}..{:?}", s.from, s.to),
            AstEntry::FieldDefine(fd) => fd.id(),
            AstEntry::ExportFn(ef) => ef.id(),
        }
    }

    fn content(&self) -> String {
        match self {
            AstEntry::Comment(_s) => format!("Comment"),
            AstEntry::Entity(_s) => format!("Entitiy"),
            AstEntry::Concept(_s) => format!("Concept"),
            AstEntry::Operation(_s) => format!("Operation"),
            AstEntry::ImplBlock(_s) => format!("ImplBlock"),
            AstEntry::FieldDefine(fd) => fd.content(),
            AstEntry::ExportFn(ef) => ef.content(),
        }
    }

    fn build_children(&self, mut builder: GraphvizBuilder) -> GraphvizBuilder {
        match self {
            AstEntry::Comment(_s) => builder,
            AstEntry::Entity(_s) => builder,
            AstEntry::Concept(_s) => builder,
            AstEntry::Operation(_s) => builder,
            AstEntry::ImplBlock(_s) => builder,
            AstEntry::FieldDefine(fd) => fd.build_children(builder),
            AstEntry::ExportFn(ef) => ef.build_children(builder),
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
    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
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
        self.decl_name.0.clone()
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
        format!("AlgeExpr {:?}..{:?}", self.span.from, self.span.to)
    }

    fn shape(&self) -> vola_common::dot::graphviz_rust::attributes::shape {
        vola_common::dot::graphviz_rust::attributes::shape::oval
    }

    fn content(&self) -> String {
        format!("AlgeExpr!")
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
