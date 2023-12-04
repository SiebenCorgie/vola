use ahash::{AHashMap};
use common::EntryPoint;
use graph::{region::RegionConfig, NodeRefs, NodeTy};
use slotmap::SlotMap;

mod graph;
pub use graph::{region::RegionBuilder, AlgeNode, AlgeOp, CombNode, CombOp, Node, NodeRef, Region};
mod common;
pub use common::{EntryPointType, Ident};
mod symbol_table;
pub use symbol_table::SymbolTable;
mod debug;

use vola_ast::Ast;

mod ast_parser;

#[cfg(feature = "dot")]
mod dot;

pub struct Module;

impl Module {
    pub fn builder() -> ModuleBuilder {
        let mut nodes = SlotMap::with_key();
        let error_node = nodes.insert(Node::Error);

        ModuleBuilder {
            error_node,
            nodes,
            entry_points: AHashMap::default(),
        }
    }

    ///Creates the builder by reading in an [vola-ast::Ast].
    pub fn from_ast(ast: Ast) -> ModuleBuilder {
        let builder = Module::builder();
        ast_parser::parse_ast(ast, builder)
    }
}

pub struct ModuleBuilder {
    //Error node
    pub error_node: NodeRef,
    pub nodes: SlotMap<NodeRef, Node>,

    entry_points: AHashMap<Ident, EntryPoint>,
}

impl ModuleBuilder {
    pub fn new_region<'a>(
        &'a mut self,
        on_builder: impl FnOnce(RegionBuilder<'a>) -> RegionBuilder<'a>,
        resolve_passes: RegionConfig,
    ) -> NodeRef {
        let at_node = self.new_node(AlgeNode::new(AlgeOp::At));
        let builderctx = RegionBuilder {
            module: self,
            at_ref: at_node,
            args: NodeRefs::new(),
            prim_args: NodeRefs::new(),
            out: None,
            symbols: SymbolTable::new(),
        };

        let builder = on_builder(builderctx);

        builder.build(resolve_passes)
    }

    //Creates a new field entry point. `on_builder` is the [Region] of that entry point.
    pub fn new_entrypoint<'a>(
        &'a mut self,
        ident: impl Into<Ident>,
        entrypoint_type: EntryPointType,
        on_builder: impl FnOnce(RegionBuilder<'_>) -> RegionBuilder<'_>,
    ) {
        let config = match entrypoint_type {
            EntryPointType::Op => RegionConfig {
                resolve_prim_callsite: true,
                ..Default::default()
            },
            _ => RegionConfig::default(),
        };

        let node_ref = { self.new_region(on_builder, config) };
        self.entry_points.insert(
            ident.into(),
            EntryPoint {
                root_node: node_ref,
                ty: entrypoint_type,
            },
        );
    }

    pub fn new_node(&mut self, node: impl Into<Node>) -> NodeRef {
        self.nodes.insert(node.into())
    }

    ///Returns if the node exists and is of `ty`. Otherwise returns false
    pub fn is_ty(&self, ty: NodeTy, node: NodeRef) -> bool {
        if let Some(n) = self.nodes.get(node) {
            n.is_ty(ty)
        } else {
            false
        }
    }

    ///Returns true if this node is not a concrete value, but a template
    pub fn is_template(&self, node: NodeRef) -> bool {
        match self.nodes.get(node).as_ref().unwrap() {
            Node::CombNode(CombNode {
                op: CombOp::PrimArg(_),
                ..
            }) => true,
            _ => false,
        }
    }

    #[cfg(feature = "dot")]
    pub fn dot_graph(&self) -> graphviz_rust::dot_structures::Graph {
        let mut subs = Vec::new();

        let _graph_id = graphviz_rust::dot_structures::Id::Plain(format!("Module"));

        for (ident, entrypoint) in &self.entry_points {
            let subgraph = self.dot_sub_graph(entrypoint.root_node);
            let subgraphid = subgraph.id.clone();

            let entry_id = graphviz_rust::dot_structures::Id::Plain(format!("{}", ident.0));
            subs.push(graphviz_rust::dot_structures::Stmt::Node(
                graphviz_rust::dot_structures::Node {
                    id: graphviz_rust::dot_structures::NodeId(entry_id.clone(), None),
                    attributes: vec![
                        graphviz_rust::attributes::NodeAttributes::label(format!("{}", ident.0)),
                        graphviz_rust::attributes::NodeAttributes::shape(
                            graphviz_rust::attributes::shape::triangle,
                        ),
                    ],
                },
            ));

            subs.push(graphviz_rust::dot_structures::Stmt::Subgraph(subgraph));

            subs.push(graphviz_rust::dot_structures::Stmt::Edge(
                graphviz_rust::dot_structures::Edge {
                    ty: graphviz_rust::dot_structures::EdgeTy::Pair(
                        graphviz_rust::dot_structures::Vertex::N(
                            graphviz_rust::dot_structures::NodeId(entry_id, None),
                        ),
                        graphviz_rust::dot_structures::Vertex::N(
                            graphviz_rust::dot_structures::NodeId(subgraphid, None),
                        ),
                    ),
                    attributes: vec![],
                },
            ));
        }

        graphviz_rust::dot_structures::Graph::Graph {
            stmts: subs,
            strict: true,
            id: graphviz_rust::dot_structures::Id::Plain(format!("SuperGraph")),
        }
    }
}
