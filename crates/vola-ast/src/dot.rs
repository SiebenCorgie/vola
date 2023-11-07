use graphviz_rust::{
    attributes::NodeAttributes,
    dot_structures::{Edge, EdgeTy, Id, Node, NodeId, Stmt, Vertex},
};

use crate::{
    alge::AlgeExpr,
    comb::OpNode,
    common::{Alge, Field, Identifier, Op, Prim, PrimBlock, TypedIdent},
};

impl Field {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        let block_id = self.block.dot_node(rnd, &self.ident, stmt);
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
        let block_id = self.block.dot_node(rnd, &self.ident, stmt);
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
        let block_id = self.block.dot_node(rnd, &self.ident, stmt);
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
        let id = NodeId(Id::Plain(format!("Ident_{}_{rnd}", self.imm)), None);
        *rnd += 1;
        stmt.push(Stmt::Node(Node {
            id: id.clone(),
            attributes: vec![NodeAttributes::label(format!("\"{}\"", self.imm))],
        }));
        Vertex::N(id)
    }
}

impl TypedIdent {
    pub fn dot_node(&self, rnd: &mut usize, stmt: &mut Vec<Stmt>) -> Vertex {
        let id = NodeId(Id::Plain(format!("Ident_{}_{rnd}", self.ident.imm)), None);
        *rnd += 1;
        stmt.push(Stmt::Node(Node {
            id: id.clone(),
            attributes: vec![if let Some(ty) = &self.ty {
                NodeAttributes::label(format!("\"{}: {:?}\"", self.ident.imm, ty))
            } else {
                NodeAttributes::label(format!("\"{}: NoTy\"", self.ident.imm))
            }],
        }));
        Vertex::N(id)
    }
}

impl PrimBlock {
    pub fn dot_node(&self, rnd: &mut usize, ident: &Identifier, stmt: &mut Vec<Stmt>) -> Vertex {
        let id = NodeId(Id::Plain(format!("Block_{rnd}")), None);
        *rnd += 1;

        stmt.push(Stmt::Node(Node {
            id: id.clone(),
            attributes: vec![NodeAttributes::label(format!("\"{}\"", ident.imm))],
        }));
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
            crate::common::Stmt::AtAssign {
                assign_op,
                expr,
                src: _,
            } => {
                let id = if let Some(op) = assign_op {
                    NodeId(Id::Plain(format!("AtAssign_{:?}_{rnd}", op)), None)
                } else {
                    NodeId(Id::Plain(format!("AtAssign_NONE_{rnd}")), None)
                };
                *rnd += 1;
                let expr_vertex = expr.dot_node(rnd, stmt);
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!("\"@ {:?}= \"", assign_op))],
                }));
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), expr_vertex),
                    attributes: vec![],
                }));

                Vertex::N(id)
            }
            crate::common::Stmt::FieldAssign {
                src: _,
                prim,
                field,
                assign_op,
                expr,
            } => {
                let id = if let Some(op) = assign_op {
                    NodeId(
                        Id::Plain(format!(
                            "FieldAssign_{}_{:?}_{}_rnd",
                            prim.imm, op, field.imm
                        )),
                        None,
                    )
                } else {
                    NodeId(
                        Id::Plain(format!("FieldAssign_{}_NONE_{}_{rnd}", prim.imm, field.imm)),
                        None,
                    )
                };
                *rnd += 1;
                let expr_vertex = expr.dot_node(rnd, stmt);
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!(
                        "\"{}.{} {:?}= \"",
                        prim.imm, field.imm, assign_op
                    ))],
                }));
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), expr_vertex),
                    attributes: vec![],
                }));

                Vertex::N(id)
            }
            crate::common::Stmt::LetStmt {
                src: _,
                ident,
                expr,
            } => {
                let id = NodeId(
                    Id::Plain(format!("LetStmt_{}_{rnd}", ident.ident.imm)),
                    None,
                );
                *rnd += 1;
                let sub_expr = expr.dot_node(rnd, stmt);

                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!(
                        "\"let {}= \"",
                        ident.ident.imm
                    ))],
                }));
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), sub_expr),
                    attributes: vec![],
                }));
                Vertex::N(id)
            }
            crate::common::Stmt::PrimAtAssign {
                src: _,
                prim,
                assign_op,
                expr,
            } => {
                let id = if let Some(op) = assign_op {
                    NodeId(
                        Id::Plain(format!("FieldAssign_{}_{:?}_AT_{rnd}", prim.imm, op)),
                        None,
                    )
                } else {
                    NodeId(
                        Id::Plain(format!("FieldAssign_{}_NONE_AT_{rnd}", prim.imm)),
                        None,
                    )
                };
                *rnd += 1;
                let expr_vertex = expr.dot_node(rnd, stmt);
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!(
                        "\"{}.@ {:?}= \"",
                        prim.imm, assign_op
                    ))],
                }));
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), expr_vertex),
                    attributes: vec![],
                }));

                Vertex::N(id)
            }
            crate::common::Stmt::PrimDef {
                src: _,
                ident,
                init,
            } => {
                let id = NodeId(Id::Plain(format!("PrimDef_{}_{rnd}", ident.imm)), None);
                *rnd += 1;

                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!("\"def {} = \"", ident.imm))],
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
            crate::common::Stmt::EvalStmt {
                src: _,
                template_ident,
                binding,
            } => {
                let id = NodeId(Id::Plain(format!("EvalStmt_{}_{rnd}", binding.imm)), None);
                *rnd += 1;

                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!(
                        "\"eval {} -> {} \"",
                        template_ident.imm, binding.imm
                    ))],
                }));

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
                    attributes: vec![NodeAttributes::label(format!("\"x {:?} y\"", op))],
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
                    attributes: vec![NodeAttributes::label(format!("\"{:?} x\"", op))],
                }));
                let exp = expr.dot_node(rnd, stmt);
                stmt.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(Vertex::N(id.clone()), exp),
                    attributes: vec![],
                }));

                Vertex::N(id)
            }
            AlgeExpr::Call { ident, args } => {
                let id = NodeId(Id::Plain(format!("Call_{}_{rnd}", ident.imm)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!("\"{}() \"", ident.imm))],
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
                    Id::Plain(format!("PrimAccess_{}_{}_{rnd}", ident.imm, field.imm)),
                    None,
                );

                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!(
                        "\"{}.{}\"",
                        ident.imm, field.imm
                    ))],
                }));
                Vertex::N(id)
            }
            AlgeExpr::List(list) => {
                let id = NodeId(Id::Plain(format!("List_{rnd}")), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!("\"List\""))],
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
                    Id::Plain(format!(
                        "Float_{}_{}_{rnd}",
                        imm.digits.0.digit, imm.digits.1.digit
                    )),
                    None,
                );
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!(
                        "\"{}.{}\"",
                        imm.digits.0.digit, imm.digits.1.digit
                    ))],
                }));
                Vertex::N(id)
            }
            AlgeExpr::Identifier(ident) => {
                let id = NodeId(Id::Plain(format!("Ident_{}_{rnd}", ident.imm)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!("\"{}\"", ident.imm))],
                }));
                Vertex::N(id)
            }
            AlgeExpr::Kw(kw) => {
                let id = NodeId(Id::Plain(format!("KW_{:?}_{rnd}", kw)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!("\"KW:{:?}\"", kw))],
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
                let id = NodeId(Id::Plain(format!("OpCall_{}_{rnd}", ident.imm)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!("\"call {} \"", ident.imm))],
                }));

                for arg in args {
                    let arg_expr = arg.dot_node(rnd, stmt);

                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(id.clone()), arg_expr),
                        attributes: vec![],
                    }));
                }

                for prim in prims {
                    let arg_expr = prim.dot_node(rnd, stmt);
                    stmt.push(Stmt::Edge(Edge {
                        ty: EdgeTy::Pair(Vertex::N(id.clone()), arg_expr),
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
                    Id::Plain(format!("PrimCall_{}_{rnd}", prim_call_ident.imm)),
                    None,
                );
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!(
                        "\"prim {} \"",
                        prim_call_ident.imm
                    ))],
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
                let id = NodeId(Id::Plain(format!("Prim_{}_{rnd}", ident.imm)), None);
                *rnd += 1;
                stmt.push(Stmt::Node(Node {
                    id: id.clone(),
                    attributes: vec![NodeAttributes::label(format!(
                        "\"prim ref {} \"",
                        ident.imm
                    ))],
                }));
                Vertex::N(id)
            }
        }
    }
}
