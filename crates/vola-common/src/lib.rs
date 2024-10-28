/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Common diagnosis helper. This is mostly Span of nodes, as well their reporting.

use std::fmt::Display;

use serde::{Deserialize, Serialize};
use smallstr::SmallString;

pub use serde;
pub use serde_lexpr;

pub use ariadne;
pub use thiserror;

#[cfg(feature = "dot")]
pub mod dot;

pub mod error;
mod reporter;
pub use reporter::{cache_file, report, reset_file_cache};

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

    ///Basically [Self::with_file], but for an option. Makes it easy to combine `Span::from(Node).with_file_myabe(parser_context.get_file())`.
    pub fn with_file_maybe(self, file_name: Option<&str>) -> Self {
        if let Some(file) = file_name {
            self.with_file(file)
        } else {
            self
        }
    }

    ///Returns a non-empty file string, if one was set
    pub fn get_file(&self) -> Option<&str> {
        if self.file.is_empty() {
            None
        } else {
            Some(&self.file)
        }
    }

    pub fn is_empty(&self) -> bool {
        self == &Self::empty()
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}:{} .. {}:{}",
            self.file, self.from.0, self.from.1, self.to.0, self.to.1
        )
    }
}

impl ariadne::Span for Span {
    type SourceId = std::path::Path;
    fn source(&self) -> &Self::SourceId {
        &std::path::Path::new(self.file.as_str())
    }
    fn start(&self) -> usize {
        self.byte_start
    }
    fn end(&self) -> usize {
        self.byte_end
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
