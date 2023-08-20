//! Combinatorical level AST.

use ahash::{AHashMap, AHashSet};

use crate::{
    alge::AlgeExpr,
    common::{Identifier, TypedIdent},
    parser::FromSitter,
    AstError,
};

///A singular Op-Node in some fields Op-Tree. Note that the leafs of that tree are
/// always primitive calls
#[derive(Debug, Clone)]
pub enum OpNode {
    Node {
        ///Identifier of op that is being called
        op_ident: Identifier,
        ///Arguments to the op call,
        args: Vec<AlgeExpr>,
        ///Sub nodes to this tree.
        prims: Vec<OpNode>,
    },
    Prim {
        prim_call_ident: Identifier,
        args: Vec<AlgeExpr>,
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

        println!("Found {}", expr.kind());
        match expr.kind() {
            "identifier" => Ok(OpNode::PrimIdent(Identifier::parse_node(source, &expr)?)),
            "call_expr" => {
                let prim_call_ident =
                    Identifier::parse_node(source, expr.child(0).as_ref().unwrap())?;

                for i in 0.. {
                    if let Some(child) = expr.child(i) {
                        println!("    C: {}", child.kind());
                    } else {
                        break;
                    }
                }

                Ok(OpNode::Prim {
                    prim_call_ident,
                    args: Vec::with_capacity(0),
                })
            }
            _ => Err(AstError::AnyError(format!(
                "Unexpected token {} in prim_expr",
                expr.kind()
            ))),
        }
    }
}

impl OpNode {
    ///Resolves all identifier that are referenced and life.
    pub fn resolve_sub_expressions(
        &mut self,
        life_algebraic: &AHashMap<Identifier, AlgeExpr>,
        life_prims: &AHashMap<Identifier, Option<OpNode>>,
    ) {
        //TODO I'm not really sure right now if we should resolve sub expressions already. Instead we could already build data dependency
        // vertices.  However, this would make the AST a Abstract-Syntax-DAG... so probably not.
        //
        // However, this way we loose the knowledge of common expressions / nodes at this point, and have to rebuild that
        // later on.
        //
        // A resolution might be to somehow tag nodes and algebraic expressions.

        match self {
            Self::Node {
                op_ident,
                args,
                prims,
            } => {
                //in case of a node, resolve all args that are an identifier, ther recurse
            }
        }
    }

    ///Parses the `block` part of the tree sitter grammer.
    pub fn parse_block(source: &[u8], node: &tree_sitter::Node) -> Result<Self, AstError> {
        //NOTE: This is the **magical** part. Basically at this point we build the actual
        // op tree of some `op` or `field`.
        //
        // This works by collecting all
        // def and let statements, and iteratively building all tree parts. We always inline them if possible until we reach the
        // last tree, which will be the actual final node tree.
        //
        // Therefore any unused sub-tree will be discarded automatically at this point. By parsing top down we also implement
        // let / def shadowing, since we always inline the most recent identifier at each step.

        assert!(node.kind() == "block");
        let mut life_algebraic = AHashMap::with_capacity(0);
        let mut life_prims = AHashMap::with_capacity(0);

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        assert!(children.next().unwrap().kind() == "{");

        for child in children {
            match child.kind() {
                "def_prim" => {
                    //a prim statment consists of the prims identifier, and possibly an initialization prim_stmt
                    let ident = Identifier::parse_node(source, &child.child(1).as_ref().unwrap())?;
                    let init = if child.child_count() > 2 {
                        let mut node =
                            OpNode::parse_node(source, &child.child(3).as_ref().unwrap())?;
                        //Immediatly resolve node's identifier
                        node.resolve_sub_expressions(&life_algebraic, &life_prims);
                        Some(node)
                    } else {
                        None
                    };

                    let _old = life_prims.insert(ident, init);
                }
                "let_stmt" => {}
                "assignment_stmt" => {}
                "prim_expr" => {
                    //this must be our last prim expression.
                }
                //Ignoring comments at this point.
                "comment" => {}
                "}" => break, // block ended
                _ => {
                    return Err(AstError::AnyError(format!(
                        "Unexpected token {} at {:?}",
                        child.kind(),
                        child
                    )))
                }
            }
        }

        Ok(OpNode::PrimIdent(Identifier(String::new())))
    }
}
