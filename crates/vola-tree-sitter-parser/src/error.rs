/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::{
    num::{ParseFloatError, ParseIntError},
    str::Utf8Error,
};

use thiserror::Error;
use tree_sitter::Node;
use vola_common::VolaError;

use crate::ParserCtx;

#[derive(Debug, Error, Clone)]
pub enum ParserError {
    #[error("Unknown Error: {0}")]
    UnknownError(String),
    #[error("{0}")]
    Other(String),
    #[error("Could not run tree-sitter")]
    TreeSitterFailed,
    #[error("File Error occured: {0}")]
    FSError(String),
    #[error("Unknown AstNode {kind}")]
    UnknownAstNode { kind: String },
    #[error("Unexpected AstNode: {kind}, expected {expected}")]
    UnexpectedAstNode { kind: String, expected: String },

    #[error("Unexpected value {val} expected it to be {exp}")]
    UnexpectedNodeValue { val: String, exp: String },

    #[error("Uncaught error in this region")]
    UncaughtError,
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

    #[error("Parsing bool literal failed")]
    ParseBoolFailed,

    #[error("Expected at least one access descriptor. For instance:\n my_field.SDF(p)\n")]
    NoAccessDecs,

    #[error("No accessed field on field_access. Try adding one or multiple field names or numbers. For instance `myobject.fieldname.0`.")]
    NoAccessedField,

    #[error("Field define needs to end with a CSG-Tree")]
    NoCSGTreeAtDefineEnd,

    #[error("The impl-block must have a algebraic expression as its last statement")]
    NoAlgeExprAtEnd,

    #[error("Could not parse {kind}, was empty")]
    EmptyParse { kind: String },
}

impl ParserError {
    pub fn assert_node_kind(
        ctx: &mut ParserCtx,
        node: &Node,
        kind: &str,
    ) -> Result<(), VolaError<Self>> {
        if node.kind() != kind {
            let error = Self::UnexpectedAstNode {
                kind: node.kind().to_owned(),
                expected: kind.to_owned(),
            };
            Err(VolaError::error_here(
                error,
                ctx.span(node),
                format!("expected node of kind '{kind}'"),
            ))
        } else {
            Ok(())
        }
    }

    pub fn assert_node_string(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &Node,
        string: &str,
    ) -> Result<(), VolaError<Self>> {
        let node_utf8 = node.utf8_text(dta);
        if node_utf8 != Ok(string) {
            let error = Self::UnexpectedNodeValue {
                val: node_utf8.unwrap_or("Could not parse").to_owned(),
                exp: string.to_owned(),
            };

            Err(VolaError::error_here(
                error,
                ctx.span(node),
                format!("expected '{string}'"),
            ))
        } else {
            Ok(())
        }
    }

    ///Consumes `node` and checks that its of a certain kind.
    /// Shortcut to check keyword-like parts of the syntax-tree.
    pub fn consume_expected_node_kind(
        ctx: &mut ParserCtx,
        node: Option<Node>,
        kind: &str,
    ) -> Result<(), VolaError<Self>> {
        match node {
            None => Err(VolaError::new(Self::NoChildAvailable)),
            Some(node) => Self::assert_node_kind(ctx, &node, kind),
        }
    }

    pub fn consume_expected_node_string(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: Option<Node>,
        string: &str,
    ) -> Result<(), VolaError<Self>> {
        match node {
            None => {
                //report(error_reporter(Self::NoChildAvailable, Span::empty()).with_message(&format!("tried to consume node with string {string}, but there was no node at all")).finish());
                return Err(VolaError::new(Self::NoChildAvailable));
            }
            Some(node) => Self::assert_node_string(ctx, dta, &node, string),
        }
    }

    pub fn assert_ast_level_empty(
        ctx: &mut ParserCtx,
        node: Option<Node>,
    ) -> Result<(), VolaError<Self>> {
        if let Some(node) = node {
            Err(VolaError::error_here(
                Self::LevelNotEmpty,
                ctx.span(&node),
                "here",
            ))
        } else {
            Ok(())
        }
    }

    pub fn assert_node_no_error(ctx: &mut ParserCtx, node: &Node) -> Result<(), VolaError<Self>> {
        if node.has_error() {
            Err(VolaError::error_here(
                Self::UncaughtError,
                ctx.span(node),
                "in this region",
            ))
        } else {
            Ok(())
        }
    }
}
