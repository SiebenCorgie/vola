//! Combinatorical level AST.

use crate::{alge::AlgeExpr, common::Identifier, parser::FromSitter, AstError, AstErrorTy};

///A singular Op-Node in some fields Op-Tree. Note that the leafs of that tree are
/// always primitive calls
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OpNode {
    Prim {
        prim_call_ident: Identifier,
        args: Vec<AlgeExpr>,
    },
    ///A call to some `op some_name<prims, ..>(args, ..){block..}`
    OpCall {
        ///The operation that is being called.
        ident: Identifier,
        ///The arguments in this context that are supplied to the op-call
        args: Vec<AlgeExpr>,
        ///The sub nodes of that tree, aka. sub-trees for that op.
        prims: Vec<OpNode>,
    },
    ///Identifier to a not-yet resolved primitive or sub-tree.
    PrimIdent(Identifier),
}

impl FromSitter for OpNode {
    ///Parses a `prim_expr` from the tree-sitter grammar
    fn parse_node(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError>
    where
        Self: Sized,
    {
        assert!(
            node.kind() == "prim_expr",
            "expected prim_expr, was {}",
            node.kind()
        );

        //Match which kind of prim_expr we have
        let expr = node.child(0).unwrap();
        match expr.kind() {
            "identifier" => Ok(OpNode::PrimIdent(Identifier::parse_node(source, &expr)?)),
            "call_expr" => {
                let (prim_call_ident, args) = AlgeExpr::parse_call(source, &expr)?;

                Ok(OpNode::Prim {
                    prim_call_ident,
                    args,
                })
            }
            "optree" => Self::parse_optree(source, &expr),
            _ => Err(AstError::at_node(
                source,
                &expr,
                AstErrorTy::UnexpectedToken {
                    token: expr.kind().to_owned(),
                    unit: "OpNode".to_owned(),
                },
            )),
        }
    }
}

impl OpNode {
    //Resolves a level of an `optree`
    pub fn parse_optree(source: &[u8], node: &tree_sitter::Node) -> Result<OpNode, AstError> {
        assert!(node.kind() == "optree");

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        //First must be the op ident
        let ident = Identifier::parse_node(source, &children.next().unwrap())?;
        assert!(children.next().unwrap().kind() == "<"); // take off the <
                                                         //now parse the primitives, which could potentually be subtrees.
        let mut prims = Vec::new();
        for child in &mut children {
            match child.kind() {
                ">" => break,
                "," => {}
                _ => prims.push(OpNode::parse_node(source, &child)?),
            }
        }
        assert!(children.next().unwrap().kind() == "(");

        //now parse all args
        let mut args = Vec::new();

        println!("ARGS--->");
        for child in children {
            println!("{}", child.kind());
            match child.kind() {
                ")" => break,
                "," => {}
                _ => args.push(AlgeExpr::parse_node(source, &child)?),
            }
        }

        Ok(OpNode::OpCall { ident, args, prims })
    }
}
