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

use std::{fmt::Display, num::ParseIntError, path::Path};

use ahash::AHashMap;

use common::{Alge, Field, Identifier, Op, Prim};
pub use parser::parser;
use thiserror::Error;

use crate::parser::FromSitter;

pub mod alge;
pub mod comb;
pub mod common;
pub mod parser;

#[derive(Debug)]
struct ESpan {
    from: (usize, usize),
    to: (usize, usize),
}

impl ESpan {
    pub fn empty() -> Self {
        ESpan {
            from: (0, 0),
            to: (0, 0),
        }
    }
}

impl<'a> From<&tree_sitter::Node<'a>> for ESpan {
    fn from(value: &tree_sitter::Node) -> Self {
        ESpan {
            from: (value.start_position().row, value.start_position().column),
            to: (value.end_position().row, value.end_position().column),
        }
    }
}

#[derive(Debug)]
pub struct AstError {
    span: ESpan,
    node_line: String,
    source: AstErrorTy,
}

impl AstError {
    pub fn at_node(source_code: &[u8], node: &tree_sitter::Node, source: AstErrorTy) -> Self {
        let node_line = node
            .utf8_text(source_code)
            .unwrap_or("CouldNotParseLine")
            .to_owned();

        let span = ESpan::from(node);
        AstError {
            span,
            node_line,
            source,
        }
    }
}

impl From<AstErrorTy> for AstError {
    fn from(value: AstErrorTy) -> Self {
        AstError {
            span: ESpan::empty(),
            node_line: String::new(),
            source: value,
        }
    }
}

impl Display for AstError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "
Error: {}
-->  {}:{} - {}:{}

{}

<--
",
            self.source,
            self.span.from.0,
            self.span.from.1,
            self.span.to.0,
            self.span.to.1,
            self.node_line
        )
    }
}

#[derive(Debug, Error)]
pub enum AstErrorTy {
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
    #[error("Block did not end with a primitive statement")]
    BlockEndNoPrim,
    #[error("Scoped algebra expression ended with a none algebraic expression")]
    ScopedEndNoAlge,
    #[error("Unexpected token {token} while parsing {unit}")]
    UnexpectedToken { token: String, unit: String },
}

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

        //TODO transform to ast

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
        for top_level_node in root_node.children(&mut cursor) {
            match top_level_node.kind() {
                "alge_definition" => match Alge::parse_node(source, &top_level_node) {
                    Ok(f) => {
                        if let Some(old) = self.alges.insert(f.ident.clone(), f) {
                            return Err(AstError::at_node(
                                source,
                                &top_level_node,
                                AstErrorTy::IdentifierAlreadyExists {
                                    ty: "Alge".to_owned(),
                                    ident: old.ident.0,
                                },
                            ));
                        }
                    }
                    Err(e) => println!("Failed to parse Op: {e}"),
                },
                "prim_definition" => match Prim::parse_node(source, &top_level_node) {
                    Ok(f) => {
                        if let Some(old) = self.prims.insert(f.ident.clone(), f) {
                            return Err(AstError::at_node(
                                source,
                                &top_level_node,
                                AstErrorTy::IdentifierAlreadyExists {
                                    ty: "Prim Definition".to_owned(),
                                    ident: old.ident.0,
                                },
                            ));
                        }
                    }
                    Err(e) => println!("Failed to parse Prim: {e}"),
                },
                "op_definition" => match Op::parse_node(source, &top_level_node) {
                    Ok(f) => {
                        if let Some(old) = self.ops.insert(f.ident.clone(), f) {
                            return Err(AstError::at_node(
                                source,
                                &top_level_node,
                                AstErrorTy::IdentifierAlreadyExists {
                                    ty: "Op Definition".to_owned(),
                                    ident: old.ident.0,
                                },
                            ));
                        }
                    }
                    Err(e) => println!("Failed to parse Op: {e}"),
                },
                "field_definition" => match Field::parse_node(source, &top_level_node) {
                    Ok(f) => {
                        if let Some(old) = self.fields.insert(f.ident.clone(), f) {
                            return Err(AstError::at_node(
                                source,
                                &top_level_node,
                                AstErrorTy::IdentifierAlreadyExists {
                                    ty: "Field Definition".to_owned(),
                                    ident: old.ident.0,
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

        Ok(())
    }
}
