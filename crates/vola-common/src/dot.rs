//! Generic Dot-Graph transformer.
//!

use ahash::AHashSet;
use graphviz_rust::{
    attributes::{color, NodeAttributes},
    dot_structures::{Edge, Node, NodeId, Subgraph},
};

pub use graphviz_rust;

///Thin abstraction over the graphiviz structures to reduce
/// boilerplate code.
pub struct GraphvizBuilder {
    pub graph: graphviz_rust::dot_structures::Graph,
    active_subgraph: Option<Subgraph>,
}

impl GraphvizBuilder {
    pub fn new() -> Self {
        GraphvizBuilder {
            graph: graphviz_rust::dot_structures::Graph::Graph {
                id: graphviz_rust::dot_structures::Id::Plain(format!("DotGraph")),
                strict: false,
                stmts: Vec::new(),
            },
            active_subgraph: None,
        }
    }

    ///Builds a new subgraph with the given root node
    pub fn start_graph(&mut self, name: &str) {
        let subgraph = Subgraph {
            id: graphviz_rust::dot_structures::Id::Plain(format!("{}", name.to_owned())),
            stmts: Vec::new(),
        };

        self.active_subgraph = Some(subgraph);
    }

    pub fn end_graph(&mut self) {
        self.graph
            .add_stmt(graphviz_rust::dot_structures::Stmt::Subgraph(
                self.active_subgraph.take().unwrap(),
            ));
    }

    pub fn add_node(&mut self, node: &dyn DotNode) {
        //Add this node to the graph
        let new_node = Node {
            id: node.node_id(),
            attributes: vec![
                NodeAttributes::label(format!("\"{}\"", node.content())),
                NodeAttributes::color(node.color()),
                NodeAttributes::shape(node.shape()),
            ],
        };

        self.active_subgraph
            .as_mut()
            .unwrap()
            .stmts
            .push(graphviz_rust::dot_structures::Stmt::Node(new_node));
    }

    pub fn connect(&mut self, a: &dyn DotNode, b: &dyn DotNode) {
        let edge = Edge {
            ty: graphviz_rust::dot_structures::EdgeTy::Pair(
                graphviz_rust::dot_structures::Vertex::N(a.node_id()),
                graphviz_rust::dot_structures::Vertex::N(b.node_id()),
            ),
            attributes: vec![],
        };

        self.active_subgraph
            .as_mut()
            .unwrap()
            .stmts
            .push(graphviz_rust::dot_structures::Stmt::Edge(edge));
    }
}

pub trait DotNode {
    /// Unique Id of this node, used for edge connections
    fn id(&self) -> String;
    fn node_id(&self) -> NodeId {
        NodeId(
            graphviz_rust::dot_structures::Id::Plain(format!("\"{}\"", self.id())),
            None,
        )
    }

    ///Content of this node
    fn content(&self) -> String;
    fn color(&self) -> graphviz_rust::attributes::color_name {
        graphviz_rust::attributes::color_name::black
    }
    fn shape(&self) -> graphviz_rust::attributes::shape {
        graphviz_rust::attributes::shape::rect
    }
    fn build_children(&self, builder: GraphvizBuilder) -> GraphvizBuilder {
        builder
    }
}
