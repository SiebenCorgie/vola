//! Common diagnosis helper. This is mostly Span of nodes, as well their reporting.

use std::fmt::Display;

use annotate_snippets::{
    display_list::{DisplayList, FormatOptions},
    snippet::{Annotation, AnnotationType, Slice, Snippet, SourceAnnotation},
};
use backtrace::Backtrace;
use miette::{SourceOffset, SourceSpan};
pub use reporter::ErrorReporter;
use serde::{Deserialize, Serialize};
use smallstr::SmallString;
use tree_sitter::Node;

pub use serde;
pub use serde_lexpr;

pub use miette;
pub use thiserror;

#[cfg(feature = "dot")]
pub mod dot;

mod reporter;

pub use reporter::{report, Reportable};

pub type FileString = SmallString<[u8; 32]>;

///Source-Code span information.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub file: FileString,
    ///Start line and collumn
    pub from: (usize, usize),
    ///End line and collumn
    pub to: (usize, usize),
    ///Start byte into the src file
    pub byte_start: usize,
    ///End byte into the src file
    pub byte_end: usize,
}

impl Span {
    pub fn empty() -> Self {
        Span {
            file: FileString::default(),
            from: (0, 0),
            to: (0, 0),
            byte_start: 0,
            byte_end: 0,
        }
    }

    pub fn with_file(mut self, file_name: &str) -> Self {
        self.file = FileString::from_str(file_name);
        self
    }
}

impl Into<SourceSpan> for Span {
    fn into(self) -> SourceSpan {
        SourceSpan::new(
            SourceOffset::from(self.byte_start),
            self.byte_end - self.byte_start,
        )
    }
}

impl<'a> From<&tree_sitter::Node<'a>> for Span {
    fn from(value: &tree_sitter::Node) -> Self {
        Span {
            file: FileString::default(),
            from: (value.start_position().row, value.start_position().column),
            to: (value.end_position().row, value.end_position().column),
            byte_start: value.start_byte(),
            byte_end: value.end_byte(),
        }
    }
}
