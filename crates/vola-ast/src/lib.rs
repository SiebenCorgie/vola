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

use std::{num::ParseIntError, path::Path};

use ahash::AHashMap;

use common::{Alge, Field, Identifier, Op, Prim};
pub use parser::parser;
use thiserror::Error;

use crate::parser::FromSitter;
pub mod alge;
pub mod comb;
pub mod common;
pub mod parser;

#[derive(Debug, Error)]
pub enum AstError {
    #[error("Failed to load vola parser: {0}")]
    LanguageError(#[from] tree_sitter::LanguageError),
    #[error("{0}")]
    IoError(#[from] std::io::Error),
    #[error("Failed to parse from file")]
    ParseError,
    #[error("Failed to parse utf8 string in source code: {0}")]
    Utf8Err(#[from] core::str::Utf8Error),
    #[error("{ty} with name {ident} already existed")]
    IdentifierAlreadyExists { ty: String, ident: String },
    #[error("Could not parse digit: {0}")]
    ParseDigitError(#[from] ParseIntError),
    #[error("{0}")]
    AnyError(String),
    #[error("Block did not end with a primitive statement")]
    BlockEndNoPrim,
    #[error("Scoped algebra expression ended with a none algebraic expression")]
    ScopedEndNoAlge,
}

pub enum ReportType {
    Note,
    Warn,
    Error,
}

impl ReportType {
    pub fn report(&self, node_string: &str, node: &tree_sitter::Node, reason: &str) {
        let level_str = match self {
            ReportType::Note => "NOTE",
            ReportType::Warn => "WARN",
            ReportType::Error => "ERROR",
        };
        println!(
            "{}: {}\n --> {:?}\n{}",
            level_str, reason, node, node_string
        )
    }
}

pub trait ReportNode {
    fn ty(&self) -> ReportType {
        ReportType::Error
    }

    ///Reports this node error/warning/whatever.
    fn report(self, source: &[u8], node: &tree_sitter::Node) -> Self;
}

impl<T> ReportNode for Result<T, AstError> {
    fn report(self, source: &[u8], node: &tree_sitter::Node) -> Self {
        if let Err(err) = &self {
            let node_string = node.utf8_text(source).unwrap_or("NODE_ERROR");
            self.ty().report(node_string, node, &format!("{}", err))
        }

        self
    }
}

#[derive(Debug, Clone)]
pub struct Ast {
    fields: AHashMap<Identifier, Field>,
    ops: AHashMap<Identifier, Op>,
    prims: AHashMap<Identifier, Prim>,
    alges: AHashMap<Identifier, Alge>,
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
        let mut parser = parser()?;
        let file = std::fs::read(file)?;
        let syn_tree = {
            let text = core::str::from_utf8(&file)?;
            parser.parse(text, None).ok_or(AstError::ParseError)?
        };

        //TODO transform to ast

        let mut ast = Ast::empty();

        ast.try_parse_tree(&file, &syn_tree)?;

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
        for top_level_node in root_node.children(&mut cursor) {
            match top_level_node.kind() {
                "alge_definition" => {}
                "prim_definition" => {}
                "op_definition" => {}
                "field_definition" => match Field::parse_node(source, &top_level_node) {
                    Ok(f) => {
                        if let Some(old) = self.fields.insert(f.ident.clone(), f) {
                            return Err(AstError::IdentifierAlreadyExists {
                                ty: "Field".to_owned(),
                                ident: old.ident.0,
                            });
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

        Ok(())
    }
}
