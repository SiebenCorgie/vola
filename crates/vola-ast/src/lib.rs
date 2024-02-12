//! # Vola-AST
//!
//! Vola's *A*bstract *S*yntax *T*ree.
//!
//!
//! As outlined in the design document, there are two types of AST. A CSG-AST, that represents the geometric operations, and a
//! algebraic AST, that defines those operations.
//!
//! The AST-Parsers job is to split both (somewhat like a preprocessor I guess?) into independent trees, that reference each other
//! _by name_.

use csg::{ExportFn, FieldDef};

mod alge;
mod common;
mod csg;
mod dot;
mod error;
mod parser;

pub use parser::{parse_file, parse_from_bytes, parse_string};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AstEntry {
    Comment,
    Entity,
    Concept,
    Operation,
    ImplBlock,
    FieldDefine(FieldDef),
    ExportFn(ExportFn),
}

#[derive(Clone, Debug)]
pub struct VolaAst {
    pub entries: Vec<AstEntry>,
}
