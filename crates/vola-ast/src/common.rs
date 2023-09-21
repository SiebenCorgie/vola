//! Common AST components like identifiers and types.

use crate::{
    alge::{AlgeExpr, BinOp},
    comb::OpNode,
    parser::FromSitter,
    AstError, AstErrorTy,
};

///Keywords
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    ///the @ symbol
    KwAt,
    KwPrim,
    ///Placeholder keyword
    Nill,
}

impl FromSitter for Keyword {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        match node.kind() {
            "kw_at" => Ok(Keyword::KwAt),
            "kw_prim" => Ok(Keyword::KwPrim),
            "keywoard" => Self::parse_node(source, &node.child(0).unwrap()),
            _ => Err(AstError::at_node(
                source,
                node,
                crate::AstErrorTy::UnexpectedToken {
                    token: node.kind().to_owned(),
                    unit: "Keyword".to_owned(),
                },
            )),
        }
    }
}

///Type description
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Ty {
    Scalar,
    Vector { count: usize },
    Mat { width: usize, height: usize },
}

impl FromSitter for Ty {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        match node.kind() {
            "scalar" => Ok(Ty::Scalar),
            "vec" => {
                //need to parse vec's count, for that,
                let count = node
                    .child(1)
                    .unwrap()
                    .utf8_text(source)
                    .unwrap()
                    .parse()
                    .map_err(|e| AstErrorTy::from(e))?;
                Ok(Ty::Vector { count })
            }
            "mat" => {
                let width = node
                    .child(1)
                    .unwrap()
                    .utf8_text(source)
                    .unwrap()
                    .parse()
                    .map_err(|e| AstErrorTy::from(e))?;
                let height = node
                    .child(3)
                    .unwrap()
                    .utf8_text(source)
                    .unwrap()
                    .parse()
                    .map_err(|e| AstErrorTy::from(e))?;

                Ok(Ty::Mat { width, height })
            }
            _ => Err(AstError::at_node(
                source,
                node,
                crate::AstErrorTy::UnexpectedToken {
                    token: node.kind().to_owned(),
                    unit: "Ty".to_owned(),
                },
            )),
        }
    }
}

//A string identifier.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Identifier(pub String);

impl FromSitter for Identifier {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        assert!(node.kind() == "identifier");
        let iden = node.utf8_text(source).map_err(|e| AstErrorTy::from(e))?;
        Ok(Identifier(iden.to_owned()))
    }
}

impl Identifier {
    pub fn parse_prim_list(
        source: &[u8],
        param_list: &tree_sitter::Node,
    ) -> Result<Vec<Identifier>, AstError> {
        let mut walker = param_list.walk();
        let mut children = param_list.children(&mut walker);

        let mut params = Vec::with_capacity(param_list.child_count() - 2);
        assert!(children.next().unwrap().kind() == "<");
        for child in children {
            match child.kind() {
                "," => {}
                ">" => break,
                _ => params.push(Identifier::parse_node(source, &child)?),
            }
        }

        Ok(params)
    }
}

impl From<&str> for Identifier {
    fn from(value: &str) -> Self {
        Identifier(value.to_owned())
    }
}

impl From<String> for Identifier {
    fn from(value: String) -> Self {
        Identifier(value)
    }
}

///A single digit
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ImmVal(pub usize);

impl FromSitter for ImmVal {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        assert!(node.kind() == "number");
        Ok(ImmVal(
            node.utf8_text(source)
                .unwrap()
                .parse()
                .map_err(|e| AstErrorTy::from(e))?,
        ))
    }
}

///a float immediate value
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ImmFloat(pub ImmVal, pub ImmVal);

impl FromSitter for ImmFloat {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        assert!(node.kind() == "float");
        let first = ImmVal::parse_node(source, &node.child(0).unwrap())?;

        //NOTE: 1 is the . symbol
        let second = if node.child_count() >= 3 {
            ImmVal::parse_node(source, &node.child(2).unwrap())?
        } else {
            ImmVal(0)
        };

        Ok(ImmFloat(first, second))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypedIdent {
    pub ident: Identifier,
    pub ty: Option<Ty>,
}

impl FromSitter for TypedIdent {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        assert!(node.kind() == "typed_identifier");

        let ident = Identifier::parse_node(source, &node.child(0).unwrap())?;
        let ty = if let Some(ty_node) = node.child(2) {
            let parsed_ty = Ty::parse_node(source, &ty_node)?;
            Some(parsed_ty)
        } else {
            None
        };

        Ok(TypedIdent { ident, ty })
    }
}

impl TypedIdent {
    fn parse_param_list(source: &[u8], node: &tree_sitter::Node) -> Result<Vec<Self>, AstError> {
        assert!(node.kind() == "parameter_list");
        let mut args = Vec::with_capacity(node.child_count());
        let mut walker = node.walk();

        for child in node.children(&mut walker) {
            match child.kind() {
                "(" | "," => {} //ignore
                ")" => break,
                "typed_identifier" => {
                    let arg = TypedIdent::parse_node(source, &child)?;
                    args.push(arg);
                }
                _ => {
                    return Err(AstError::at_node(
                        source,
                        node,
                        crate::AstErrorTy::UnexpectedToken {
                            token: node.kind().to_owned(),
                            unit: "TypedIdent".to_owned(),
                        },
                    ))
                }
            }
        }

        Ok(args)
    }
}

///A single statment within a block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ///Sub expression that is bound to a identifier. Possibly uninitialised.
    PrimDef {
        ident: Identifier,
        init: Option<OpNode>,
    },
    AtAssign {
        assign_op: Option<BinOp>,
        expr: AlgeExpr,
    },
    FieldAssign {
        prim: Identifier,
        field: Identifier,
        assign_op: Option<BinOp>,
        expr: AlgeExpr,
    },
    PrimAtAssign {
        prim: Identifier,
        assign_op: Option<BinOp>,
        expr: AlgeExpr,
    },
    LetStmt {
        ident: TypedIdent,
        expr: AlgeExpr,
    },
}

impl FromSitter for Stmt {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        match node.kind() {
            "assignment_stmt" => {
                //find out which kind of assignment we have.
                let mut walker = node.walk();
                let mut children = node.children(&mut walker);

                let assignee = children.next().unwrap();
                assert!(assignee.kind() == "assignee");

                // Based on the assignee we can find out which kind of assignment we have.
                // If it begins with kw_at, its at_assign,
                // Otherwise its a field assignment.
                match assignee.child(0).as_ref().unwrap().kind() {
                    "kw_at" => {
                        let op = BinOp::parse_assign_op(source, children.next().as_ref().unwrap())?;
                        let expr = AlgeExpr::parse_node(source, children.next().as_ref().unwrap())?;
                        Ok(Stmt::AtAssign {
                            assign_op: op,
                            expr,
                        })
                    }
                    _ => {
                        //try to parse the two fields
                        let prim = Identifier::parse_node(source, &assignee.child(0).unwrap())?;

                        assert!(assignee.child(1).as_ref().unwrap().kind() == ".");

                        let second_part = assignee.child(2).unwrap();
                        match second_part.kind() {
                            "identifier" => {
                                //To find if its a normal _field_ or the _at field_, match the field behind the dot.
                                let field =
                                    Identifier::parse_node(source, &assignee.child(2).unwrap())?;
                                let assign_op = BinOp::parse_assign_op(
                                    source,
                                    children.next().as_ref().unwrap(),
                                )?;

                                let expr = AlgeExpr::parse_node(
                                    source,
                                    children.next().as_ref().unwrap(),
                                )?;
                                Ok(Stmt::FieldAssign {
                                    prim,
                                    assign_op,
                                    field,
                                    expr,
                                })
                            }
                            "kw_at" => {
                                let assign_op = BinOp::parse_assign_op(
                                    source,
                                    children.next().as_ref().unwrap(),
                                )?;

                                let expr = AlgeExpr::parse_node(
                                    source,
                                    children.next().as_ref().unwrap(),
                                )?;
                                Ok(Stmt::PrimAtAssign {
                                    prim,
                                    assign_op,
                                    expr,
                                })
                            }
                            _ => Err(AstError::at_node(
                                source,
                                node,
                                crate::AstErrorTy::UnexpectedToken {
                                    token: second_part.kind().to_owned(),
                                    unit: "Assignment in Stmt".to_owned(),
                                },
                            )),
                        }
                    }
                }
            }
            _ => Err(AstError::at_node(
                source,
                node,
                crate::AstErrorTy::UnexpectedToken {
                    token: node.kind().to_owned(),
                    unit: "Stmt".to_owned(),
                },
            )),
        }
    }
}

///Primitive block is characterised by a series of statements, and a final `op_tree`
/// that defines the final tree that is being returned by this block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PrimBlock {
    pub stmt_list: Vec<Stmt>,
    ///The Op-Tree that is being calculated by this field
    pub op_tree: OpNode,
}

impl FromSitter for PrimBlock {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        assert!(node.kind() == "block");
        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        assert!(children.next().unwrap().kind() == "{");

        let mut stmt_list = Vec::with_capacity(node.child_count().checked_sub(3).unwrap_or(0));
        let mut ret_op = None;

        for child in children {
            match child.kind() {
                "def_prim" => {
                    //a prim statment consists of the prims identifier, and possibly an initialization prim_stmt
                    let ident = Identifier::parse_node(source, &child.child(1).as_ref().unwrap())?;
                    let init = if child.child_count() > 3 {
                        let node = OpNode::parse_node(source, &child.child(3).as_ref().unwrap())?;
                        Some(node)
                    } else {
                        None
                    };

                    stmt_list.push(Stmt::PrimDef { ident, init });
                }
                "let_stmt" => {
                    let (ident, expr) = AlgeExpr::parse_let_stmt(source, &child)?;
                    stmt_list.push(Stmt::LetStmt { ident, expr })
                }
                "assignment_stmt" => {
                    stmt_list.push(Stmt::parse_node(source, &child)?);
                }
                "prim_expr" => {
                    //this must be our last prim expression.
                    let final_node = OpNode::parse_node(source, &child)?;
                    if let Some(old_fin) = ret_op.take() {
                        println!("WARN: Already had final node: {:?}", old_fin);
                    }
                    ret_op = Some(final_node);
                }
                //Ignoring comments at this point.
                "comment" => {}
                "}" => break, // block ended
                _ => {
                    return Err(AstError::at_node(
                        source,
                        &child,
                        AstErrorTy::UnexpectedToken {
                            token: child.kind().to_owned(),
                            unit: "PrimBlock".to_owned(),
                        },
                    ))
                }
            }
        }

        if let Some(ret) = ret_op {
            Ok(PrimBlock {
                stmt_list,
                op_tree: ret,
            })
        } else {
            //NOTE: happens if the block ended, but no primitive statment / optree was set.
            Err(AstError::at_node(source, node, AstErrorTy::BlockEndNoPrim))
        }
    }
}

///Top level field node
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub ident: Identifier,
    ///All arguments for that field.
    pub args: Vec<TypedIdent>,
    pub block: PrimBlock,
}

impl FromSitter for Field {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        let mut field_walker = node.walk();
        let mut children = node.children(&mut field_walker);
        assert!(children.next().unwrap().kind() == "field");
        let ident = children.next().unwrap();
        assert!(ident.kind() == "identifier");
        let ident = Identifier::parse_node(source, &ident)?;

        let mut next_node = children.next();
        let args = if next_node.map(|node| node.kind()) == Some("parameter_list") {
            let args = TypedIdent::parse_param_list(source, next_node.as_ref().unwrap())?;
            //advance node
            next_node = children.next();
            args
        } else {
            Vec::with_capacity(0)
        };

        //At this point the next must be a block. Per-grammar this block will end with a optree (called prim_expr in treesitter)
        // therefore we can share the block parsing for `fields`  and `ops`. which is why we delegate that at this point.
        assert!(next_node.as_ref().unwrap().kind() == "block");

        let block = PrimBlock::parse_node(source, next_node.as_ref().unwrap())?;

        Ok(Field { ident, args, block })
    }
}

///Operation definition (not to be confused with a Op-Tree made from [OpNode]s).
#[derive(Debug, Clone)]
pub struct Op {
    pub ident: Identifier,
    pub prims: Vec<Identifier>,
    pub args: Vec<TypedIdent>,
    pub block: PrimBlock,
}

impl FromSitter for Op {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        assert!(children.next().unwrap().kind() == "op");
        let ident = Identifier::parse_node(source, children.next().as_ref().unwrap())?;

        //Collect all primitive identifier
        let mut next_node = children.next().unwrap();

        let prims = if next_node.kind() == "prim_list" {
            let prims = Identifier::parse_prim_list(source, &next_node)?;
            next_node = children.next().unwrap();
            prims
        } else {
            Vec::with_capacity(0)
        };

        let args = if next_node.kind() == "parameter_list" {
            let args = TypedIdent::parse_param_list(source, &next_node)?;
            //advance node
            next_node = children.next().unwrap();
            args
        } else {
            Vec::with_capacity(0)
        };

        //at this point we should be at a block
        if next_node.kind() != "block" {
            return Err(AstError::at_node(
                source,
                &next_node,
                AstErrorTy::UnexpectedToken {
                    token: next_node.kind().to_owned(),
                    unit: "Block".to_owned(),
                },
            ));
        }
        let block = PrimBlock::parse_node(source, &next_node)?;

        Ok(Op {
            ident,
            prims,
            args,
            block,
        })
    }
}

///Definition of a primitive. A primitive needs to return a primitive definition.
#[derive(Debug, Clone)]
pub struct Prim {
    pub ident: Identifier,
    pub args: Vec<TypedIdent>,

    ///The primitive that is being returned. Possibly nested in a sub tree
    pub block: PrimBlock,
}

impl FromSitter for Prim {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        assert!(children.next().unwrap().kind() == "prim");
        let ident = Identifier::parse_node(source, children.next().as_ref().unwrap())?;

        //Collect all primitive identifier
        let mut next_node = children.next().unwrap();

        let args = if next_node.kind() == "parameter_list" {
            let args = TypedIdent::parse_param_list(source, &next_node)?;
            //advance node
            next_node = children.next().unwrap();
            args
        } else {
            Vec::with_capacity(0)
        };

        //at this point we should be at a block
        assert!(next_node.kind() == "block");
        let block = PrimBlock::parse_node(source, &next_node)?;

        Ok(Prim { ident, args, block })
    }
}

///An algebraic function.
#[derive(Debug, Clone)]
pub struct Alge {
    pub ident: Identifier,
    pub args: Vec<TypedIdent>,
    pub ret: AlgeExpr,
}

impl FromSitter for Alge {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        assert!(children.next().unwrap().kind() == "alge");
        let ident = Identifier::parse_node(source, children.next().as_ref().unwrap())?;

        //Collect all primitive identifier
        let mut next_node = children.next().unwrap();

        let args = if next_node.kind() == "parameter_list" {
            let args = TypedIdent::parse_param_list(source, &next_node)?;
            //advance node
            next_node = children.next().unwrap();
            args
        } else {
            Vec::with_capacity(0)
        };

        //at this point we should be at a block
        assert!(next_node.kind() == "scoped_expr");

        let ret = AlgeExpr::parse_node(source, &next_node)?;

        Ok(Alge { ident, args, ret })
    }
}
