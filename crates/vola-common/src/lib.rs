//! Common diagnosis helper. This is mostly Span of nodes, as well their reporting.

use std::fmt::Display;

use backtrace::Backtrace;

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};

///Source-Code span information.
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

#[derive(Clone, Debug)]
pub struct CommonError<E: std::error::Error> {
    pub span: Span,
    pub node_line: String,
    pub source: E,
    pub backtrace: Option<Backtrace>,
}

impl<E: std::error::Error> CommonError<E> {
    pub fn new(source_code: String, span: Span, source: E) -> Self {
        CommonError {
            span,
            node_line: source_code,
            source,
            backtrace: if std::env::var("VOLA_BACKTRACE").is_ok() {
                Some(Backtrace::new())
            } else {
                None
            },
        }
    }
}

impl<E: std::error::Error> Display for CommonError<E> {
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
