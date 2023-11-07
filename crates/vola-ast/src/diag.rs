//! AST diagnosis helper. This is mostly Span of nodes, as well as AST errors and their reporting.

use std::{fmt::Display, num::ParseIntError};

use backtrace::Backtrace;
use thiserror::Error;

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub from: (usize, usize),
    pub to: (usize, usize),
}

impl Span {
    pub fn empty() -> Self {
        Span {
            from: (0, 0),
            to: (0, 0),
        }
    }
}

impl<'a> From<&tree_sitter::Node<'a>> for Span {
    fn from(value: &tree_sitter::Node) -> Self {
        Span {
            from: (value.start_position().row, value.start_position().column),
            to: (value.end_position().row, value.end_position().column),
        }
    }
}

#[derive(Debug)]
pub struct AstError {
    span: Span,
    node_line: String,
    source: AstErrorTy,
    backtrace: Option<Backtrace>,
}

impl AstError {
    pub fn at_node(source_code: &[u8], node: &tree_sitter::Node, source: AstErrorTy) -> Self {
        let node_line = node
            .utf8_text(source_code)
            .unwrap_or("CouldNotParseLine")
            .to_owned();

        let span = Span::from(node);
        AstError {
            span,
            node_line,
            source,
            backtrace: if std::env::var("VOLA_BACKTRACE").is_ok() {
                Some(Backtrace::new())
            } else {
                None
            },
        }
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
        AstError {
            span: Span::empty(),
            node_line: String::new(),
            source: value,
            backtrace: None,
        }
    }
}

impl Display for AstError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let error_str = self.source.to_string();
        //NOTE: usually an error is only on one line / at one point. However,
        // sometimes it goes over multiple lines. In this case, this makes sure we only
        // attach annotation to the last line, by clamping to the first line + start offset, and last line - start offset
        let snippet = if self.span.from.0 != self.span.to.0 {
            let mut slices = Vec::with_capacity(self.span.to.0 - self.span.from.0);
            for (slice_line, line_number) in self
                .node_line
                .split("\n")
                .zip(self.span.from.0..=self.span.to.0)
            {
                let annotations = if line_number == self.span.from.0 {
                    vec![SourceAnnotation {
                        label: &error_str,
                        annotation_type: AnnotationType::Error,
                        range: (0, slice_line.chars().count() - 1),
                    }]
                } else if line_number == self.span.to.0 {
                    vec![SourceAnnotation {
                        label: "end of error region",
                        annotation_type: AnnotationType::Error,
                        range: (0, slice_line.chars().count()),
                    }]
                } else {
                    vec![]
                };

                slices.push(Slice {
                    source: slice_line,
                    line_start: line_number + 1, //+1 since we are using indices, but want to display the line number
                    origin: None,                //TODO carry filename at some point
                    fold: false,
                    annotations,
                })
            }

            Snippet {
                title: Some(Annotation {
                    label: Some("AST"),
                    id: None, //TODO might want to turn those into error IDs at some point
                    annotation_type: AnnotationType::Error,
                }),
                footer: vec![],
                slices,
                opt: FormatOptions {
                    color: true,
                    ..Default::default()
                },
            }
        } else {
            //Simple single line reporting
            Snippet {
                title: Some(Annotation {
                    label: Some("AST"),
                    id: None, //TODO might want to turn those into error IDs at some point
                    annotation_type: AnnotationType::Error,
                }),
                footer: vec![],
                slices: vec![Slice {
                    source: &self.node_line,
                    line_start: self.span.from.0 + 1, //+1 since we are using indices, but want to display the line number
                    origin: None,                     //TODO carry filename at some point
                    fold: true,
                    annotations: vec![SourceAnnotation {
                        label: &error_str,
                        annotation_type: AnnotationType::Error,
                        range: (0, self.span.to.1 - self.span.from.1),
                    }],
                }],
                opt: FormatOptions {
                    color: true,
                    ..Default::default()
                },
            }
        };

        let dl = DisplayList::from(snippet);
        let res = dl.fmt(f);
        if let Some(bt) = &self.backtrace {
            write!(f, "\nBacktrace:")?;
            write!(f, "\n{:?}", bt)?;
        } else {
            write!(
                f,
                "\n`VOLA_BACKTRACE=1` to print the backtrace of the error occurrence"
            )?;
        }
        res
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
