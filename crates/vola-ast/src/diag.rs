//! AST diagnosis helper. This is mostly Span of nodes, as well as AST errors and their reporting.

use std::{fmt::Display, num::ParseIntError};

use thiserror::Error;

use vola_common::{CommonError, Span};

#[derive(Debug)]
pub struct AstError(pub CommonError<AstErrorTy>);

impl AstError {
    pub fn at_node(source_code: &[u8], node: &tree_sitter::Node, source: AstErrorTy) -> Self {
        let node_line = node
            .utf8_text(source_code)
            .unwrap_or("CouldNotParseLine")
            .to_owned();

        let span = Span::from(node);
        AstError(CommonError::new(span, source))
    }

    ///Helper that creates an error if `node` is not of `expect_kind`.
    pub fn kind_expected(
        source_code: &[u8],
        node: &tree_sitter::Node,
        expect_kind: &str,
    ) -> Result<(), Self> {
        if node.kind() == expect_kind {
            return Ok(());
        }

        Err(Self::at_node(
            source_code,
            node,
            AstErrorTy::UnexpectedNodeKind {
                actual: node.kind().to_owned(),
                expected: expect_kind.to_owned(),
            },
        ))
    }

    ///Helper that checks if the child iterator has ended. Reports an error if not.
    pub fn expect_end<'a>(
        source_code: &[u8],
        iter: &mut dyn Iterator<Item = tree_sitter::Node<'a>>,
    ) -> Result<(), Self> {
        if let Some(node) = iter.next() {
            Err(AstError::at_node(
                source_code,
                &node,
                AstErrorTy::ExpectEndOfStatement,
            ))
        } else {
            Ok(())
        }
    }

    pub fn uncaught_error(source_code: &[u8], node: &tree_sitter::Node) -> Result<(), Self> {
        if node.has_error() {
            Err(AstError::at_node(
                source_code,
                node,
                AstErrorTy::UncaughtError,
            ))
        } else {
            Ok(())
        }
    }
}

impl From<AstErrorTy> for AstError {
    fn from(value: AstErrorTy) -> Self {
        AstError(CommonError {
            span: Span::empty(),
            source: value,
            backtrace: None,
        })
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
    #[error("Unexpected node {actual} in AST, expected {expected}")]
    UnexpectedNodeKind { actual: String, expected: String },
    #[error("Expected end of statement")]
    ExpectEndOfStatement,
    #[error("Expected end of block, because return operation was set before.")]
    ExpectedEndAfterRet,
    #[error("Expected {0}")]
    Expected(String),
    #[error("Invalid node in tree, but error was not caught correctly")]
    UncaughtError,
}

impl Default for AstErrorTy {
    fn default() -> Self {
        AstErrorTy::UncaughtError
    }
}
