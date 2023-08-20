//! Common AST components like identifiers and types.

use crate::{alge::AlgeExpr, comb::OpNode, parser::FromSitter, AstError, ReportNode};

///Keywords
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    ///the @ symbol
    KwAt,
    KwPrim,
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
            _ => Err(AstError::AnyError(format!(
                "Unexpected token {} at {:?} while parsing keyword",
                node.kind(),
                node
            ))),
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
                let count = node.child(1).unwrap().utf8_text(source).unwrap().parse()?;
                Ok(Ty::Vector { count })
            }
            "mat" => {
                let width = node.child(1).unwrap().utf8_text(source).unwrap().parse()?;
                let height = node.child(3).unwrap().utf8_text(source).unwrap().parse()?;

                Ok(Ty::Mat { width, height })
            }
            _ => Err(AstError::AnyError(format!(
                "Unexpected node {} @ {:?}, expected type",
                node.kind(),
                node
            ))),
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
        let iden = node.utf8_text(source).map_err(|e| AstError::Utf8Err(e))?;
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

///A single digit
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ImmVal(usize);

impl FromSitter for ImmVal {
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        assert!(node.kind() == "number");
        Ok(ImmVal(node.utf8_text(source).unwrap().parse()?))
    }
}

///a float immediate value
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ImmFloat(ImmVal, ImmVal);

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
                    return Err(AstError::AnyError(format!(
                        "Unexpected token: {} at {:?}",
                        child.kind(),
                        child
                    )))
                }
            }
        }

        Ok(args)
    }
}

///Top level field node
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Field {
    pub ident: Identifier,
    ///All arguments for that field.
    pub args: Vec<TypedIdent>,

    ///The Op-Tree that is being calculated by this field
    pub op_tree: OpNode,
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
        let ident = Identifier::parse_node(source, &ident).report(source, &ident)?;

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

        let op_tree = OpNode::parse_block(source, next_node.as_ref().unwrap())?;

        Ok(Field {
            ident,
            args,
            op_tree,
        })
    }
}

///Operation definition (not to be confused with a Op-Tree made from [OpNode]s).
#[derive(Debug, Clone)]
pub struct Op {
    pub ident: Identifier,
    pub prims: Vec<Identifier>,
    pub args: Vec<TypedIdent>,
    ///The sub tree that is being build from this op and its nodes
    pub op_tree: OpNode,
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
        assert!(next_node.kind() == "block");

        let op_tree = OpNode::parse_block(source, &next_node)?;

        Ok(Op {
            ident,
            prims,
            args,
            op_tree,
        })
    }
}

///Definition of a primitive. A primitive needs to return a primitive definition.
#[derive(Debug, Clone)]
pub struct Prim {
    pub ident: Identifier,
    pub args: Vec<TypedIdent>,

    ///The primitive that is being returned. Possibly nested in a sub tree
    pub ret: OpNode,
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

        let ret = OpNode::parse_block(source, &next_node)?;

        Ok(Prim { ident, args, ret })
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
