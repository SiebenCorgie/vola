use ahash::AHashMap;
use graphviz_rust::{
    attributes::{dir, shape, EdgeAttributes, NodeAttributes},
    dot_structures::{Edge, EdgeTy, Id, NodeId, Stmt, Subgraph, Vertex},
};
use slotmap::Key;

use crate::{ModuleBuilder, Node, NodeRef};

fn node_id(node: NodeRef) -> NodeId {
    NodeId(Id::Plain(format!("NODE_{}", node.data().as_ffi())), None)
}

#[allow(dead_code)]
fn graph_id(node: NodeRef) -> Id {
    Id::Plain(format!("GRAPH_{}", node.data().as_ffi()))
}

fn subgraph_id(node: NodeRef) -> Id {
    Id::Plain(format!("cluster_{}", node.data().as_ffi()))
}

impl ModuleBuilder {
    pub fn dot_sub_graph(&self, entry_node: NodeRef) -> Subgraph {
        let mut stmts = Vec::new();
        let mut known = AHashMap::default();
        let entry_vertex = self.dot_node(entry_node, &mut known, &mut stmts);
        let id = match &entry_vertex {
            Vertex::N(node) => node.0.clone(),
            Vertex::S(sg) => sg.id.clone(),
        };

        Subgraph { id, stmts }
    }

    pub fn dot_node(
        &self,
        nref: NodeRef,
        known_refs: &mut AHashMap<NodeRef, Vertex>,
        stmts: &mut Vec<Stmt>,
    ) -> Vertex {
        if let Some(vert) = known_refs.get(&nref) {
            return vert.clone();
        }

        if let Some(node) = self.nodes.get(nref.clone()) {
            match node {
                Node::Region(r) => {
                    //Region is characterized by a sub graph cluster
                    let mut sub_stmts = Vec::new();

                    //Push exit node first, then args. This
                    // should schedule the argnodes last.
                    let out_node_id = self.dot_node(r.out, known_refs, &mut sub_stmts);
                    known_refs.insert(nref, out_node_id.clone());

                    self.dot_node(r.in_at_node, known_refs, &mut sub_stmts);
                    for arg in r.in_args {
                        let _ver = self.dot_node(arg, known_refs, &mut sub_stmts);
                    }

                    let thisid = subgraph_id(nref);

                    stmts.push(Stmt::Subgraph(Subgraph {
                        id: thisid,
                        stmts: sub_stmts,
                    }));

                    out_node_id
                }
                Node::AlgeNode(a) => {
                    //get our own id
                    let id = node_id(nref);
                    let thisid = Vertex::N(id.clone());

                    known_refs.insert(nref, thisid.clone());
                    stmts.push(Stmt::Node(graphviz_rust::dot_structures::Node {
                        id,
                        attributes: vec![
                            NodeAttributes::label(format!("{}", a.op)),
                            NodeAttributes::shape(shape::oval),
                        ],
                    }));
                    //now schedule all args and draw edge between us and them
                    for arg in a.in_args {
                        let arg_id = self.dot_node(arg, known_refs, stmts);
                        stmts.push(Stmt::Edge(Edge {
                            ty: EdgeTy::Pair(thisid.clone(), arg_id),
                            attributes: vec![
                                EdgeAttributes::dir(dir::forward),
                                EdgeAttributes::style("dotted".to_owned()),
                            ],
                        }));
                    }

                    if let Some(child) = a.child_prim {
                        let arg_id = self.dot_node(child, known_refs, stmts);
                        stmts.push(Stmt::Edge(Edge {
                            ty: EdgeTy::Pair(thisid.clone(), arg_id),
                            attributes: vec![
                                EdgeAttributes::dir(dir::forward),
                                EdgeAttributes::style("solid".to_owned()),
                            ],
                        }));
                    }

                    thisid
                }
                Node::CombNode(c) => {
                    //get our own id
                    let id = node_id(nref);
                    let thisid = Vertex::N(id.clone());
                    known_refs.insert(nref, thisid.clone());
                    stmts.push(Stmt::Node(graphviz_rust::dot_structures::Node {
                        id,
                        attributes: vec![
                            NodeAttributes::label(format!("{}", c.op)),
                            NodeAttributes::shape(shape::box_),
                        ],
                    }));
                    //now schedule all args and draw edge between us and them
                    for arg in c.in_args {
                        let arg_id = self.dot_node(arg, known_refs, stmts);
                        stmts.push(Stmt::Edge(Edge {
                            ty: EdgeTy::Pair(thisid.clone(), arg_id),
                            attributes: vec![
                                EdgeAttributes::dir(dir::forward),
                                EdgeAttributes::style("dotted".to_owned()),
                            ],
                        }));
                    }
                    for child in c.in_children {
                        let child_id = self.dot_node(child, known_refs, stmts);
                        stmts.push(Stmt::Edge(Edge {
                            ty: EdgeTy::Pair(thisid.clone(), child_id),
                            attributes: vec![
                                EdgeAttributes::dir(dir::forward),
                                EdgeAttributes::style("solid".to_owned()),
                            ],
                        }))
                    }
                    let at_id = self.dot_node(c.in_at, known_refs, stmts);
                    //Reference at node
                    stmts.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(thisid.clone(), at_id),
                        attributes: vec![
                            EdgeAttributes::dir(dir::forward),
                            EdgeAttributes::style("dotted".to_owned()),
                        ],
                    }));

                    thisid
                }
                Node::Error => Vertex::N(node_id(self.error_node)),
            }
        } else {
            Vertex::N(node_id(self.error_node))
        }
    }
}
