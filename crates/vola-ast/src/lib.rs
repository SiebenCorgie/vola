//! # Vola-AST
//!
//! Vola's *A*bstract *S*yntax *T*ree.
//!
//!
//! The AST is designed to be easily transformed into SSA 3-address code. This allows us to transform it into
//! MLIR Code, or directly into SPIR-V, depending on the chosen back/middle end.
//!
//!
//! The design somewhat reflects the design of the vola language syntax. There are two levels,
//! a `cobinatorical`, which applies operations to primitives, and, at the top level defines a `field`.
//!
//! Each of those primitives (and arguments to operations) are defined by an algebraic expression. That could be
//! a simple value, lets say `5`, or an expression, lets say `5+a`.
//!
//! The AST thefore is split into two as well. The toplevel `combinatorical`, and a lower level, more traditional
//! algebraic AST. For simplicity we call the combinatorical AST just **AST**.
//!
//! This allows a compiler to first resolve the evaluation time of each algebraic expression, before compiling down to
//! the actual equation.
//!
//! NOTE(tendsin): Lets see if this actually works out this way :D

use ahash::AHashMap;
use diag::{AstError, AstErrorTy};
use std::path::Path;

use common::{Alge, Field, Identifier, Op, Prim};
pub use parser::parser;

use crate::parser::FromSitter;

pub mod alge;
pub mod comb;
pub mod common;
pub mod diag;
pub mod parser;

#[cfg(feature = "dot")]
pub mod dot;

#[derive(Debug, Clone)]
pub struct Ast {
    pub fields: AHashMap<Identifier, Field>,
    pub ops: AHashMap<Identifier, Op>,
    pub prims: AHashMap<Identifier, Prim>,
    pub alges: AHashMap<Identifier, Alge>,
}

impl Ast {
    ///Creates an empty Ast with the given root node
    pub fn empty() -> Self {
        Ast {
            fields: AHashMap::default(),
            ops: AHashMap::default(),
            prims: AHashMap::default(),
            alges: AHashMap::default(),
        }
    }

    ///Tries to parse `file` using `tree-sitter-vola` into an [Ast].
    pub fn from_file(file: impl AsRef<Path>) -> Result<Self, AstError> {
        let mut parser = parser().map_err(|e| AstErrorTy::from(e))?;
        let file = std::fs::read(file).map_err(|e| AstErrorTy::from(e))?;
        let syn_tree = {
            let text = core::str::from_utf8(&file).map_err(|e| AstErrorTy::from(e))?;
            parser.parse(text, None).ok_or(AstErrorTy::ParseError)?
        };

        let mut ast = Ast::empty();

        ast.try_parse_tree(&file, &syn_tree)?;

        Ok(ast)
    }

    pub fn from_string(string: &str) -> Result<Self, AstError> {
        let mut parser = parser().map_err(|e| AstErrorTy::from(e))?;
        let str_bytes = string.as_bytes();
        let syn_tree = { parser.parse(string, None).ok_or(AstErrorTy::ParseError)? };

        let mut ast = Ast::empty();
        ast.try_parse_tree(str_bytes, &syn_tree)?;
        Ok(ast)
    }

    ///Parses `tree` into `self`'s context.
    pub fn try_parse_tree(
        &mut self,
        source: &[u8],
        tree: &tree_sitter::Tree,
    ) -> Result<(), AstError> {
        let root_node = tree.root_node();
        let mut cursor = tree.walk();

        let mut errors = Vec::new();

        for top_level_node in root_node.children(&mut cursor) {
            match top_level_node.kind() {
                "alge_definition" => match Alge::parse_node(source, &top_level_node) {
                    Ok(f) => {
                        if let Some(old) = self.alges.insert(f.ident.clone(), f) {
                            errors.push(AstError::at_node(
                                source,
                                &top_level_node,
                                AstErrorTy::IdentifierAlreadyExists {
                                    ty: "Alge".to_owned(),
                                    ident: old.ident.imm,
                                },
                            ));
                        }
                    }
                    Err(e) => println!("Failed to parse Op: {e}"),
                },
                "prim_definition" => match Prim::parse_node(source, &top_level_node) {
                    Ok(f) => {
                        if let Some(old) = self.prims.insert(f.ident.clone(), f) {
                            errors.push(AstError::at_node(
                                source,
                                &top_level_node,
                                AstErrorTy::IdentifierAlreadyExists {
                                    ty: "Prim Definition".to_owned(),
                                    ident: old.ident.imm,
                                },
                            ));
                        }
                    }
                    Err(e) => println!("Failed to parse Prim: {e}"),
                },
                "op_definition" => match Op::parse_node(source, &top_level_node) {
                    Ok(f) => {
                        if let Some(old) = self.ops.insert(f.ident.clone(), f) {
                            errors.push(AstError::at_node(
                                source,
                                &top_level_node,
                                AstErrorTy::IdentifierAlreadyExists {
                                    ty: "Op Definition".to_owned(),
                                    ident: old.ident.imm,
                                },
                            ));
                        }
                    }
                    Err(e) => println!("Failed to parse Op: {e}"),
                },
                "field_definition" => match Field::parse_node(source, &top_level_node) {
                    Ok(f) => {
                        if let Some(old) = self.fields.insert(f.ident.clone(), f) {
                            errors.push(AstError::at_node(
                                source,
                                &top_level_node,
                                AstErrorTy::IdentifierAlreadyExists {
                                    ty: "Field Definition".to_owned(),
                                    ident: old.ident.imm,
                                },
                            ));
                        }
                    }
                    Err(e) => println!("Failed to parse field: {e}"),
                },
                _ => {
                    println!(
                        "Unknown toplevel node {}. Skipping...",
                        top_level_node.kind()
                    );
                }
            }
        }

        if errors.len() > 0 {
            for err in &errors {
                println!("{err}");
            }
            Err(errors.remove(0))
        } else {
            Ok(())
        }
    }

    #[cfg(feature = "dot")]
    pub fn dot_graph(&self) -> graphviz_rust::dot_structures::Graph {
        //Build syntax tree for each field, op, prim, alge as a cluster

        let mut subgraphs = Vec::new();
        let mut rnd = 0usize;
        for (ident, field) in &self.fields {
            let mut local_stmt = Vec::new();

            let _root_node = field.dot_node(&mut rnd, &mut local_stmt);

            //Push as subgraph
            subgraphs.push(graphviz_rust::dot_structures::Stmt::Subgraph(
                graphviz_rust::dot_structures::Subgraph {
                    id: graphviz_rust::dot_structures::Id::Plain(format!("cluster_{}", ident.imm)),
                    stmts: local_stmt,
                },
            ))
        }
        for (ident, op) in &self.ops {
            let mut local_stmt = Vec::new();

            let _root_node = op.dot_node(&mut rnd, &mut local_stmt);

            //Push as subgraph
            subgraphs.push(graphviz_rust::dot_structures::Stmt::Subgraph(
                graphviz_rust::dot_structures::Subgraph {
                    id: graphviz_rust::dot_structures::Id::Plain(format!("cluster_{}", ident.imm)),
                    stmts: local_stmt,
                },
            ))
        }
        for (ident, prim) in &self.prims {
            let mut local_stmt = Vec::new();

            let _root_node = prim.dot_node(&mut rnd, &mut local_stmt);

            //Push as subgraph
            subgraphs.push(graphviz_rust::dot_structures::Stmt::Subgraph(
                graphviz_rust::dot_structures::Subgraph {
                    id: graphviz_rust::dot_structures::Id::Plain(format!("cluster_{}", ident.imm)),
                    stmts: local_stmt,
                },
            ))
        }

        for (ident, alge) in &self.alges {
            let mut local_stmt = Vec::new();

            let _root_node = alge.dot_node(&mut rnd, &mut local_stmt);

            //Push as subgraph
            subgraphs.push(graphviz_rust::dot_structures::Stmt::Subgraph(
                graphviz_rust::dot_structures::Subgraph {
                    id: graphviz_rust::dot_structures::Id::Plain(format!("cluster_{}", ident.imm)),
                    stmts: local_stmt,
                },
            ))
        }

        graphviz_rust::dot_structures::Graph::Graph {
            id: graphviz_rust::dot_structures::Id::Plain("AllAsts".to_owned()),
            strict: true,
            stmts: subgraphs,
        }
    }
}
