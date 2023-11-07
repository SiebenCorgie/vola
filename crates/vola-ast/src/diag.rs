//! AST diagnosis helper. This is mostly Span of nodes, as well as AST errors and their reporting.

use std::{fmt::Display, num::ParseIntError};

use backtrace::Backtrace;
use thiserror::Error;

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

#[derive(Debug)]
struct Span {
    from: (usize, usize),
    to: (usize, usize),
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
        let snippet = Snippet {
            title: Some(Annotation {
                label: Some("Error"),
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
}
