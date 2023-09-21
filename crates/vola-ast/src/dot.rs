use graphviz_rust::dot_structures::{Edge, EdgeTy, Id, Node, NodeId, Stmt, Vertex};

use crate::{
    alge::AlgeExpr,
    comb::OpNode,
    common::{Alge, Field, Identifier, Op, Prim, PrimBlock, TypedIdent},
};

impl Field {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        let block_id = self.block.dot_node(rnd, stmt);
        for arg in &self.args {
            let arg_id = arg.dot_node(rnd, stmt);
            stmt.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(block_id.clone(), arg_id),
                attributes: vec![],
            }));
        }

        block_id
    }
}

impl Op {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        let block_id = self.block.dot_node(rnd, stmt);
        for arg in &self.args {
            let arg_id = arg.dot_node(rnd, stmt);
            stmt.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(block_id.clone(), arg_id),
                attributes: vec![],
            }));
        }
        for p in &self.prims {
            let arg_id = p.dot_node(rnd, stmt);
            stmt.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(block_id.clone(), arg_id),
                attributes: vec![],
            }));
        }

        block_id
    }
}

impl Prim {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        let block_id = self.block.dot_node(rnd, stmt);
        for arg in &self.args {
            let arg_id = arg.dot_node(rnd, stmt);
            stmt.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(block_id.clone(), arg_id),
                attributes: vec![],
            }));
        }

        block_id
    }
}

impl Alge {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        let block_id = self.ret.dot_node(rnd, stmt);
        for arg in &self.args {
            let arg_id = arg.dot_node(rnd, stmt);
            stmt.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(block_id.clone(), arg_id),
                attributes: vec![],
            }));
        }

        block_id
    }
}

impl Identifier {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        let id = NodeId(Id::Plain(format!("Ident_{}_{rnd}", self.0)), None);
        *rnd += 1;
        stmt.push(Stmt::Node(Node {
            id: id.clone(),
            attributes: vec![],
        }));
        Vertex::N(id)
    }
}

impl TypedIdent {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        let id = NodeId(Id::Plain(format!("Ident_{}_{rnd}", self.ident.0)), None);
        *rnd += 1;
        stmt.push(Stmt::Node(Node {
            id: id.clone(),
            attributes: vec![],
        }));
        Vertex::N(id)
    }
}

impl PrimBlock {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        let id = NodeId(Id::Plain(format!("Block_{rnd}")), None);
        *rnd += 1;
        for local_stmt in &self.stmt_list {
            let stmt_id = local_stmt.dot_node(rnd, stmt);

            //Attach statment to block
            stmt.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(Vertex::N(id.clone()), stmt_id),
                attributes: vec![],
            }));
        }

        let optree_node = self.op_tree.dot_node(rnd, stmt);
        stmt.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(Vertex::N(id.clone()), optree_node),
            attributes: vec![],
        }));

        Vertex::N(id)
    }
}

impl crate::common::Stmt {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        match self {
            crate::common::Stmt::AtAssign { assign_op, expr } => {
                let id = if let Some(op) = assign_op {
                    NodeId(Id::Plain(format!("AtAssign_{:?}_{rnd}", op)), None)
                } else {
                    NodeId(Id::Plain(format!("AtAssign_NONE_{rnd}")), None)
                };
                *rnd += 1;
                let expr_vertex = expr.dot_node(rnd, stmt);
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), expr_vertex),
                    attributes: vec![],
                }));

                Vertex::N(id)
            }
            crate::common::Stmt::FieldAssign {
                prim,
                field,
                assign_op,
                expr,
            } => {
                let id = if let Some(op) = assign_op {
                    NodeId(
                        Id::Plain(format!("FieldAssign_{}_{:?}_{}_rnd", prim.0, op, field.0)),
                        None,
                    )
                } else {
                    NodeId(
                        Id::Plain(format!("FieldAssign_{}_NONE_{}_{rnd}", prim.0, field.0)),
                        None,
                    )
                };
                *rnd += 1;
                let expr_vertex = expr.dot_node(rnd, stmt);
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), expr_vertex),
                    attributes: vec![],
                }));

                Vertex::N(id)
            }
            crate::common::Stmt::LetStmt { ident, expr } => {
                let id = NodeId(Id::Plain(format!("LetStmt_{}_{rnd}", ident.ident.0)), None);
                *rnd += 1;
                let sub_expr = expr.dot_node(rnd, stmt);

                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), sub_expr),
                    attributes: vec![],
                }));
                Vertex::N(id)
            }
            crate::common::Stmt::PrimAtAssign {
                prim,
                assign_op,
                expr,
            } => {
                let id = if let Some(op) = assign_op {
                    NodeId(
                        Id::Plain(format!("FieldAssign_{}_{:?}_AT_{rnd}", prim.0, op)),
                        None,
                    )
                } else {
                    NodeId(
                        Id::Plain(format!("FieldAssign_{}_NONE_AT_{rnd}", prim.0)),
                        None,
                    )
                };
                *rnd += 1;
                let expr_vertex = expr.dot_node(rnd, stmt);
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), expr_vertex),
                    attributes: vec![],
                }));

                Vertex::N(id)
            }
            crate::common::Stmt::PrimDef { ident, init } => {
                let id = NodeId(Id::Plain(format!("PrimDef_{}_{rnd}", ident.0)), None);
                *rnd += 1;

                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                if let Some(subexp) = init {
                    let sub_expr = subexp.dot_node(rnd, stmt);

                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(id.clone()), sub_expr),
                        attributes: vec![],
                    }));
                }

                Vertex::N(id)
            }
        }
    }
}

impl AlgeExpr {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        match self {
            AlgeExpr::BinOp { op, left, right } => {
                let id = NodeId(Id::Plain(format!("BinOp_{:?}_{rnd}", op)), None);
                *rnd += 1;

                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));

                let left = left.dot_node(rnd, stmt);
                let right = right.dot_node(rnd, stmt);

                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), left),
                    attributes: vec![],
                }));
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), right),
                    attributes: vec![],
                }));

                Vertex::N(id)
            }
            AlgeExpr::UnaryOp { op, expr } => {
                let id = NodeId(Id::Plain(format!("UnOp_{:?}_{rnd}", op)), None);
                *rnd += 1;

                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                let exp = expr.dot_node(rnd, stmt);
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), exp),
                    attributes: vec![],
                }));

                Vertex::N(id)
            }
            AlgeExpr::Call { ident, args } => {
                let id = NodeId(Id::Plain(format!("Call_{}_{rnd}", ident.0)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));

                for arg in args {
                    let exp = arg.dot_node(rnd, stmt);
                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(id.clone()), exp),
                        attributes: vec![],
                    }));
                }

                Vertex::N(id)
            }
            AlgeExpr::PrimAccess { ident, field } => {
                let id = NodeId(
                    Id::Plain(format!("PrimAccess_{}_{}_{rnd}", ident.0, field.0)),
                    None,
                );

                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                Vertex::N(id)
            }
            AlgeExpr::List(list) => {
                let id = NodeId(Id::Plain(format!("List_{rnd}")), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));

                for arg in list {
                    let exp = arg.dot_node(rnd, stmt);
                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(id.clone()), exp),
                        attributes: vec![],
                    }));
                }

                Vertex::N(id)
            }
            AlgeExpr::Float(imm) => {
                let id = NodeId(
                    Id::Plain(format!("Float_{}_{}_{rnd}", imm.0 .0, imm.1 .0)),
                    None,
                );
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                Vertex::N(id)
            }
            AlgeExpr::Identifier(ident) => {
                let id = NodeId(Id::Plain(format!("Ident_{}_{rnd}", ident.0)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                Vertex::N(id)
            }
            AlgeExpr::Kw(kw) => {
                let id = NodeId(Id::Plain(format!("KW_{:?}_{rnd}", kw)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                Vertex::N(id)
            }
        }
    }
}

impl OpNode {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        match self {
            OpNode::OpCall { ident, args, prims } => {
                let id = NodeId(Id::Plain(format!("OpCall_{}_{rnd}", ident.0)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));

                for arg in args {
                    let arg_id = NodeId(Id::Plain(format!("Arg_{}_{rnd}", ident.0)), None);
                    *rnd += 1;
                    stmt.push(Stmt::Node(Node {
                        id: arg_id.clone(),
                        attributes: vec![],
                    }));
                    let arg_expr = arg.dot_node(rnd, stmt);
                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(arg_id.clone()), arg_expr),
                        attributes: vec![],
                    }));
                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(id.clone()), Vertex::N(arg_id.clone())),
                        attributes: vec![],
                    }));
                }

                for prim in prims {
                    let arg_id = NodeId(Id::Plain(format!("PrimArg_{}_{rnd}", ident.0)), None);
                    *rnd += 1;
                    stmt.push(Stmt::Node(Node {
                        id: arg_id.clone(),
                        attributes: vec![],
                    }));
                    let arg_expr = prim.dot_node(rnd, stmt);
                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(arg_id.clone()), arg_expr),
                        attributes: vec![],
                    }));
                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(id.clone()), Vertex::N(arg_id.clone())),
                        attributes: vec![],
                    }));
                }

                Vertex::N(id)
            }
            OpNode::Prim {
                prim_call_ident,
                args,
            } => {
                let id = NodeId(
                    Id::Plain(format!("PrimCall_{}_{rnd}", prim_call_ident.0)),
                    None,
                );
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));

                for arg in args {
                    let arg_expr = arg.dot_node(rnd, stmt);
                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(id.clone()), arg_expr),
                        attributes: vec![],
                    }));
                }
                Vertex::N(id)
            }
            OpNode::PrimIdent(ident) => {
                let id = NodeId(Id::Plain(format!("Prim_{}_{rnd}", ident.0)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![],
                }));
                Vertex::N(id)
            }
        }
    }
}
