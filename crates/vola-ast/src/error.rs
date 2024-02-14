use std::{
    num::{ParseFloatError, ParseIntError},
    str::Utf8Error,
};

use thiserror::Error;
use tree_sitter::Node;
use vola_common::{CommonError, ErrorReporter, Span};

#[derive(Debug, Error, Clone)]
pub enum ParserError {
    #[error("Unknown Error: {0}")]
    UnknownError(String),
    #[error("Could not run tree-sitter")]
    TreeSitterFailed,
    #[error("File Error occured: {0}")]
    FSError(String),
    #[error("Unknown AstNode: {0}")]
    UnknownAstNode(String),
    #[error("Unexpected AstNode: {kind}, expected {expected}")]
    UnexpectedAstNode { kind: String, expected: String },
    #[error("Expected child but got none")]
    NoChildAvailable,

    #[error("Did not expect any further nodes on AST-Level")]
    LevelNotEmpty,

    #[error("UTF8 parser error: {0}")]
    Utf8ParseError(Utf8Error),
    #[error("Parsing float literal failed: {0}")]
    ParseFloatLiteral(ParseFloatError),
    #[error("Parsing float literal failed: {0}")]
    ParseIntLiteral(ParseIntError),

    #[error("Expected at least one access descriptor. For instance:\n my_field.SDF(p)\n")]
    NoAccessDecs,

    #[error("No accessed field on field_access. Try adding one or multiple field names or numbers. For instance `myobject.fieldname.0`.")]
    NoAccessedField,

    #[error("Field define needs to end with a CSG-Tree")]
    NoCSGTreeAtDefineEnd,
}

impl ParserError {
    pub fn assert_node_kind(
        reporter: &mut ErrorReporter<Self>,
        node: &Node,
        kind: &str,
    ) -> Result<(), Self> {
        if node.kind() != kind {
            let error = Self::UnexpectedAstNode {
                kind: node.kind().to_owned(),
                expected: kind.to_owned(),
            };
            reporter.push_error(CommonError::new(Span::from(node), error.clone()));
            Err(error)
        } else {
            Ok(())
        }
    }

    ///Consumes `node` and checks that its of a certain kind.
    /// Shortcut to check keyword-like parts of the syntax-tree.
    pub fn consume_expected_node_kind(
        reporter: &mut ErrorReporter<Self>,
        node: Option<Node>,
        kind: &str,
    ) -> Result<(), Self> {
        match node {
            None => {
                reporter.push_error(CommonError::new_spanless(Self::NoChildAvailable));
                return Err(Self::NoChildAvailable);
            }
            Some(node) => Self::assert_node_kind(reporter, &node, kind),
        }
    }
    pub fn assert_ast_level_empty(
        reporter: &mut ErrorReporter<Self>,
        node: Option<Node>,
    ) -> Result<(), Self> {
        if let Some(node) = node {
            let err = Self::LevelNotEmpty;
            reporter.push_error(CommonError::new(Span::from(&node), err.clone()));
            Err(err)
        } else {
            Ok(())
        }
    }

    pub fn assert_node_no_error(
        reporter: &mut ErrorReporter<Self>,
        node: &Node,
    ) -> Result<(), Self> {
        if node.has_error() {
            let err = Self::UnknownError(format!("Ast node {:?} has uncaught error", node));
            reporter.push_error(CommonError::new(Span::from(node), err.clone()));
            Err(err)
        } else {
            Ok(())
        }
    }
}
