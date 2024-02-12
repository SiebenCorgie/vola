//! Common diagnosis helper. This is mostly Span of nodes, as well their reporting.

use std::fmt::Display;

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use backtrace::Backtrace;
pub use reporter::ErrorReporter;
use serde::{Deserialize, Serialize};
use smallstr::SmallString;
use tree_sitter::Node;

pub use serde;
pub use serde_lexpr;

mod reporter;

pub type FileString = SmallString<[u8; 32]>;

///Source-Code span information.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub file: FileString,
    pub from: (usize, usize),
    pub to: (usize, usize),
}

impl Span {
    pub fn empty() -> Self {
        Span {
            file: FileString::default(),
            from: (0, 0),
            to: (0, 0),
        }
    }

    pub fn with_file(mut self, file_name: &str) -> Self {
        self.file = FileString::from_str(file_name);
        self
    }
}

impl<'a> From<&tree_sitter::Node<'a>> for Span {
    fn from(value: &tree_sitter::Node) -> Self {
        Span {
            file: FileString::default(),
            from: (value.start_position().row, value.start_position().column),
            to: (value.end_position().row, value.end_position().column),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CommonError<E: std::error::Error> {
    pub span: Option<Span>,
    pub source: E,
    pub backtrace: Option<Backtrace>,
}

impl<E: std::error::Error> CommonError<E> {
    pub fn new(span: Span, source: E) -> Self {
        CommonError {
            span: Some(span),
            source,
            backtrace: if std::env::var("VOLA_BACKTRACE").is_ok() {
                Some(Backtrace::new())
            } else {
                None
            },
        }
    }

    pub fn new_spanless(source: E) -> Self {
        CommonError {
            span: None,
            source,
            backtrace: if std::env::var("VOLA_BACKTRACE").is_ok() {
                Some(Backtrace::new())
            } else {
                None
            },
        }
    }

    pub fn new_on_node(node: &Node, source: E) -> Self {
        CommonError::new(Span::from(node), source)
    }
}

pub(crate) struct ErrorPrintBundle<'a, E: std::error::Error> {
    pub error: &'a CommonError<E>,
    pub src_lines: &'a [String],
}

impl<'a, E: std::error::Error> Display for ErrorPrintBundle<'a, E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ErrorPrintBundle { error, src_lines } = self;
        let error_str = error.source.to_string();

        let res = if let Some(span) = &error.span {
            //NOTE: usually an error is only on one line / at one point. However,
            // sometimes it goes over multiple lines. In this case, this makes sure we only
            // attach annotation to the last line, by clamping to the first line + start offset, and last line - start offset
            let snippet = if span.from.0 != span.to.0 {
                let mut slices = Vec::with_capacity(span.to.0 - span.from.0);
                for line_number in span.from.0..span.to.0 {
                    let slice_line = &src_lines[line_number];
                    let annotations = if line_number == span.from.0 {
                        vec![SourceAnnotation {
                            label: &error_str,
                            annotation_type: AnnotationType::Error,
                            range: (0, slice_line.chars().count() - 1),
                        }]
                    } else if line_number == span.to.0 {
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
                        label: Some("CommonError"),
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
                        label: None,
                        id: None, //TODO might want to turn those into error IDs at some point
                        annotation_type: AnnotationType::Error,
                    }),
                    footer: vec![],
                    slices: vec![Slice {
                        source: src_lines[span.from.0].as_str(),
                        line_start: span.from.0 + 1, //+1 since we are using indices, but want to display the line number
                        origin: None,                //TODO carry filename at some point
                        fold: true,
                        annotations: vec![SourceAnnotation {
                            label: &error_str,
                            annotation_type: AnnotationType::Error,
                            range: (span.from.1, span.to.1),
                        }],
                    }],
                    opt: FormatOptions {
                        color: true,
                        ..Default::default()
                    },
                }
            };
            let dl = DisplayList::from(snippet);
            dl.fmt(f)
        } else {
            write!(f, "Error: {}", error.source)
        };

        if let Some(bt) = &error.backtrace {
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
