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

use alge::ImplBlock;
use common::CTArg;
use csg::{CSGConcept, CSGNodeDef, ExportFn, FieldDef};

mod alge;
mod common;
mod csg;
mod error;
mod parser;

#[cfg(feature = "dot")]
pub mod dot;

pub use parser::{parse_file, parse_from_bytes, parse_string};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use vola_common::Span;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AstEntry {
    Comment(Span),
    Concept(CSGConcept),
    CSGNodeDef(CSGNodeDef),
    ImplBlock(ImplBlock),
    FieldDefine(FieldDef),
    ExportFn(ExportFn),
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct TopLevelNode {
    pub span: Span,
    pub ct_args: Vec<CTArg>,
    pub entry: AstEntry,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct VolaAst {
    pub entries: Vec<TopLevelNode>,
}

#[cfg(feature = "serde")]
impl VolaAst {
    pub fn to_sexpr(&self) -> String {
        vola_common::serde_lexpr::to_string(self).unwrap_or("error".to_string())
    }
}
