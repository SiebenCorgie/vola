//! Algebra level AST components.

use ahash::AHashMap;

use crate::{
    common::{Identifier, Imm, Keyword, TypedIdent},
    parser::FromSitter,
    AstError, AstErrorTy,
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
            _ => Err(AstError::at_node(
                &node,
                AstErrorTy::UnexpectedToken {
                    token: node.kind().to_owned(),
                    unit: "UnaryOperation".to_owned(),
                },
            )),
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

impl BinOp {
    ///Tries to parse assignment op. So `+=`, `*=` etc. Returns None if its just an assignment.
    pub fn parse_assign_op(
        _source: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Option<Self>, AstError> {
        match node.kind() {
            "+=" => Ok(Some(BinOp::Add)),
            "-=" => Ok(Some(BinOp::Sub)),
            "*=" => Ok(Some(BinOp::Mul)),
            "/=" => Ok(Some(BinOp::Div)),
            "%=" => Ok(Some(BinOp::Mod)),
            "=" => Ok(None),
            _ => Err(AstError::at_node(
                &node,
                AstErrorTy::UnexpectedToken {
                    token: node.kind().to_owned(),
                    unit: "Assignment BinOp".to_owned(),
                },
            )),
        }
    }
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
            _ => Err(AstError::at_node(
                &node,
                AstErrorTy::UnexpectedToken {
                    token: node.kind().to_owned(),
                    unit: "BinOp".to_owned(),
                },
            )),
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
    Float(Imm),
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
                            let expr = Self::parse_node(source, &child)?;
                            return Ok(expr);
                        }
                    }
                }
                Err(AstErrorTy::ScopedEndNoAlge.into())
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
            "float" => Ok(AlgeExpr::Float(Imm::parse_node(source, node)?)),
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
            _ => Err(AstError::at_node(
                &node,
                AstErrorTy::UnexpectedToken {
                    token: node.kind().to_owned(),
                    unit: "AlgeExpr".to_owned(),
                },
            )),
        }
    }
}

impl AlgeExpr {
    ///Tries to parse a `call_expr`. If successfull, returns the call's identifier as well as all arguments.
    pub fn parse_call(
        source: &[u8],
        call_expr: &tree_sitter::Node,
    ) -> Result<(Identifier, Vec<AlgeExpr>), AstError> {
        AstError::kind_expected(call_expr, "call_expr")?;

        let mut walker = call_expr.walk();
        let mut children = call_expr.children(&mut walker);
        let ident_node = children.next().unwrap();
        let ident = Identifier::parse_node(source, &ident_node)?;
        assert!(children.next().unwrap().kind() == "(");

        let mut args = Vec::with_capacity(call_expr.child_count() - 2);
        for child in children {
            match child.kind() {
                ")" => break,
                "," => {}
                _ => args.push(AlgeExpr::parse_node(source, &child)?),
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
