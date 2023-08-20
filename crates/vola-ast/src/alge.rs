//! Algebra level AST components.

use ahash::AHashMap;

use crate::{
    common::{Identifier, ImmFloat, ImmVal, Keyword},
    parser::FromSitter,
    AstError,
};

#[derive(Debug, Clone)]
pub enum UnOp {
    //The ! op
    Not,
    // The - op
    Neg,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub enum ListItem {
    List(Vec<ListItem>),
    ImmFloat(ImmFloat),
    ImmVal(ImmVal),
    Ident(Identifier),
    Keyword(Keyword),
}

///Algebraic expression.
#[derive(Debug, Clone)]
pub enum AlgeExpr {
    Identifier(Identifier),
    List(Vec<ListItem>),
    BinOp {
        op: BinOp,
        left: Box<AlgeExpr>,
        right: Box<AlgeExpr>,
    },
    UnaryOp {
        op: BinOp,
        operand: Box<AlgeExpr>,
    },
    ///Call to some algebraic function
    Call {
        ident: Identifier,
        args: Vec<AlgeExpr>,
    },
}

impl FromSitter for AlgeExpr {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        match node.kind() {
            "unary_expr" => {
                println!("Unary");
                Ok(AlgeExpr::Identifier(Identifier(String::new())))
            }
            "scoped_expr" => {
                println!("scoped");
                Ok(AlgeExpr::Identifier(Identifier(String::new())))
            }
            "binary_expr" => {
                println!("binary");
                Ok(AlgeExpr::Identifier(Identifier(String::new())))
            }
            "call_expr" => {
                println!("call");
                Ok(AlgeExpr::Identifier(Identifier(String::new())))
            }
            "identifier" => {
                println!("ident");
                Ok(AlgeExpr::Identifier(Identifier(String::new())))
            }
            "arg_access" => {
                println!("access");
                Ok(AlgeExpr::Identifier(Identifier(String::new())))
            }
            "float" => {
                println!("immfloat");
                Ok(AlgeExpr::Identifier(Identifier(String::new())))
            }
            "kw_at" => {
                println!("kwat");
                Ok(AlgeExpr::Identifier(Identifier(String::new())))
            }
            "list" => {
                println!("List");
                Ok(AlgeExpr::Identifier(Identifier(String::new())))
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
            AlgeExpr::BinOp { op, left, right } => {
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
            AlgeExpr::List(list) => for ele in list {},
            AlgeExpr::UnaryOp { op: _, operand } => operand.resolve_identifier(life_algebraic_expr),
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
}
