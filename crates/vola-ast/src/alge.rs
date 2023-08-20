//! Algebra level AST components.

use ahash::AHashMap;

use crate::{
    common::{Identifier, ImmFloat, Keyword, TypedIdent},
    parser::FromSitter,
    AstError,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnOp {
    //The ! op
    Not,
    // The - op
    Neg,
}

impl FromSitter for UnOp {
    fn parse_node(_source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        match node.kind() {
            "!" => Ok(UnOp::Not),
            "-" => Ok(UnOp::Neg),
            _ => Err(AstError::AnyError(format!(
                "Unexpected token {} at {:?} while parsing UnaryOp",
                node.kind(),
                node
            ))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl FromSitter for BinOp {
    fn parse_node(_source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        match node.kind() {
            "+" => Ok(BinOp::Add),
            "-" => Ok(BinOp::Sub),
            "*" => Ok(BinOp::Mul),
            "/" => Ok(BinOp::Div),
            "%" => Ok(BinOp::Mod),
            _ => Err(AstError::AnyError(format!(
                "Unexpected token {} at {:?} while parsing BinaryOp",
                node.kind(),
                node
            ))),
        }
    }
}

///Algebraic expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AlgeExpr {
    Identifier(Identifier),
    List(Vec<AlgeExpr>),
    BinOp {
        op: BinOp,
        left: Box<AlgeExpr>,
        right: Box<AlgeExpr>,
    },
    UnaryOp {
        op: UnOp,
        expr: Box<AlgeExpr>,
    },
    ///Call to some algebraic function
    Call {
        ident: Identifier,
        args: Vec<AlgeExpr>,
    },
    ///Acccess on an primitives field
    PrimAccess {
        ident: Identifier,
        field: Identifier,
    },
    //Some keyword
    Kw(Keyword),
    Float(ImmFloat),
}

impl FromSitter for AlgeExpr {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        //let mut walker = node.walk();
        //let mut children = node.children(&mut walker);

        match node.kind() {
            "unary_expr" => {
                assert!(node.child_count() == 2, "Unary op must have 2 children!");
                let op = UnOp::parse_node(source, &node.child(0).unwrap())?;
                let expr = AlgeExpr::parse_node(source, &node.child(1).unwrap())?;
                Ok(AlgeExpr::UnaryOp {
                    op,
                    expr: Box::new(expr),
                })
            }
            "binary_expr" => {
                assert!(
                    node.child_count() == 3,
                    "Binary expression must have 3 nodes"
                );
                let left = Self::parse_node(source, &node.child(0).unwrap())?;
                let op = BinOp::parse_node(source, &node.child(1).unwrap())?;
                let right = Self::parse_node(source, &node.child(2).unwrap())?;
                Ok(AlgeExpr::BinOp {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }
            "scoped_expr" => {
                let mut walker = node.walk();
                let mut children = node.children(&mut walker);
                assert!(children.next().unwrap().kind() == "{");

                let mut life_idents = AHashMap::default();
                for child in children {
                    match child.kind() {
                        "let_stmt" => {
                            let (ident, expr) = Self::parse_let_stmt(source, &child)?;
                            //TODO similar to the let statements in the
                            // OpTree, we loose the type info here.
                            life_idents.insert(ident.ident, expr);
                        }
                        "," => {}     //ignore
                        "}" => break, //finish
                        _ => {
                            let mut expr = Self::parse_node(source, &child)?;
                            expr.resolve_identifier(&life_idents);
                            //now return the parsed scoped expression
                            return Ok(expr);
                        }
                    }
                }
                Err(AstError::ScopedEndNoAlge)
            }
            "call_expr" => {
                let (ident, args) = Self::parse_call(source, node)?;
                Ok(AlgeExpr::Call { ident, args })
            }
            "identifier" => Ok(AlgeExpr::Identifier(Identifier::parse_node(source, node)?)),
            "arg_access" => {
                assert!(node.child_count() == 3);
                let ident = Identifier::parse_node(source, &node.child(0).unwrap())?;
                let field = Identifier::parse_node(source, &node.child(2).unwrap())?;

                Ok(AlgeExpr::PrimAccess { ident, field })
            }
            "float" => Ok(AlgeExpr::Float(ImmFloat::parse_node(source, node)?)),
            "kw_at" => Ok(AlgeExpr::Kw(Keyword::parse_node(source, node)?)),
            "list" => {
                let mut walker = node.walk();
                let mut children = node.children(&mut walker);
                assert!(children.next().unwrap().kind() == "[");
                let mut list = Vec::with_capacity(node.child_count() - 2);
                for child in children {
                    match child.kind() {
                        "]" => break,
                        "," => {}
                        _ => {
                            let node = AlgeExpr::parse_node(source, &child)?;
                            list.push(node);
                        }
                    }
                }
                Ok(AlgeExpr::List(list))
            }
            _ => Err(AstError::AnyError(format!(
                "Unexpected token {} at {:?} in AlgeExpr",
                node.kind(),
                node
            ))),
        }
    }
}

impl AlgeExpr {
    pub fn resolve_identifier(&mut self, life_algebraic_expr: &AHashMap<Identifier, AlgeExpr>) {
        match self {
            AlgeExpr::BinOp { op: _, left, right } => {
                left.resolve_identifier(life_algebraic_expr);
                right.resolve_identifier(life_algebraic_expr);
            }
            AlgeExpr::Call { ident: _, args } => {
                for arg in args {
                    arg.resolve_identifier(life_algebraic_expr);
                }
            }
            AlgeExpr::Identifier(ident) => {
                //check if we can substitude our selfs
                if let Some(substitution) = life_algebraic_expr.get(ident) {
                    *self = substitution.clone();
                    //if we could substitude, restart resolving
                    self.resolve_identifier(life_algebraic_expr);
                } else {
                    //in this case we finish
                    return;
                }
            }
            AlgeExpr::List(list) => {
                for litm in list {
                    litm.resolve_identifier(life_algebraic_expr);
                }
            }
            AlgeExpr::UnaryOp { op: _, expr } => expr.resolve_identifier(life_algebraic_expr),
            AlgeExpr::Float(_) | AlgeExpr::Kw(_) | AlgeExpr::PrimAccess { .. } => {}
        }
    }

    ///Tries to parse a `call_expr`. If successfull, returns the call's identifier as well as all arguments.
    pub fn parse_call(
        source: &[u8],
        call_expr: &tree_sitter::Node,
    ) -> Result<(Identifier, Vec<AlgeExpr>), AstError> {
        assert!(call_expr.kind() == "call_expr");

        let mut walker = call_expr.walk();
        let mut children = call_expr.children(&mut walker);
        let ident_node = children.next().unwrap();
        let ident = Identifier::parse_node(source, &ident_node)?;
        assert!(children.next().unwrap().kind() == "(");

        let mut args = Vec::with_capacity(call_expr.child_count() - 2);
        for child in children {
            if child.kind() == ")" {
                break;
            } else {
                //try to parse the expr
                args.push(AlgeExpr::parse_node(source, &child)?);
            }
        }

        Ok((ident, args))
    }

    pub fn parse_let_stmt(
        source: &[u8],
        let_stmt: &tree_sitter::Node,
    ) -> Result<(TypedIdent, Self), AstError> {
        assert!(let_stmt.kind() == "let_stmt");

        let ident = TypedIdent::parse_node(source, &let_stmt.child(1).unwrap())?;
        let expr = Self::parse_node(source, &let_stmt.child(3).unwrap())?;

        Ok((ident, expr))
    }
}
