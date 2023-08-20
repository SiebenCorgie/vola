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
                let (prim_call_ident, args) = AlgeExpr::parse_call(source, &expr)?;

                Ok(OpNode::Prim {
                    prim_call_ident,
                    args,
                })
            }
            "optree" => Self::parse_optree(source, &expr),
            _ => Err(AstError::AnyError(format!(
                "Unexpected token {} in prim_expr while parsing OpNode",
                expr.kind()
            ))),
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
        let op_ident = Identifier::parse_node(source, &children.next().unwrap())?;
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
        for child in children {
            match child.kind() {
                ")" => break,
                "," => {}
                _ => args.push(AlgeExpr::parse_node(source, &child)?),
            }
        }

        Ok(OpNode::Node {
            op_ident,
            args,
            prims,
        })
    }

    ///Resolves all identifier that are referenced and life.
    pub fn resolve_identifier(
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
                op_ident: _,
                args,
                prims,
            } => {
                //in case of a node, resolve all args that are an identifier,
                for arg in args {
                    arg.resolve_identifier(life_algebraic);
                }
                //Now recurse tree and resolve rest of the tree
                for prim in prims {
                    prim.resolve_identifier(life_algebraic, life_prims);
                }
            }
            Self::Prim {
                prim_call_ident: _,
                args,
            } => {
                for arg in args {
                    arg.resolve_identifier(life_algebraic);
                }
            }
            Self::PrimIdent(ident) => {
                //Try to resolve the primitive to a tree
                if let Some(Some(resolved)) = life_prims.get(&ident) {
                    *self = resolved.clone();
                    //And restart
                    self.resolve_identifier(life_algebraic, life_prims);
                }
            }
        }
    }

    ///Parses the `block` part of the tree sitter grammar.
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
                        node.resolve_identifier(&life_algebraic, &life_prims);
                        Some(node)
                    } else {
                        None
                    };

                    let _old = life_prims.insert(ident, init);
                }
                "let_stmt" => {
                    let (ident, expr) = AlgeExpr::parse_let_stmt(source, &child)?;
                    //TODO: We lose the type here, but we
                    if let Some(_old) = life_algebraic.insert(ident.ident, expr) {
                        //TODO check types, or carry over
                    }
                }
                "assignment_stmt" => {
                    println!("Assignment not yet implemented!");
                }
                "prim_expr" => {
                    //this must be our last prim expression.

                    let mut final_node = Self::parse_node(source, &child)?;
                    //Resolve with the final context
                    final_node.resolve_identifier(&life_algebraic, &life_prims);
                    return Ok(final_node);
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

        Err(AstError::BlockEndNoPrim)
    }
}
