/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
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

pub mod alge;
pub mod common;
pub mod csg;
mod error;
mod parser;

pub use error::ParserError;

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

impl AstEntry {
    ///Returns true for CSGNodeDef and Concept
    pub fn is_def_node(&self) -> bool {
        match self {
            Self::Concept(_) | Self::CSGNodeDef(_) => true,
            _ => false,
        }
    }

    pub fn is_impl_block(&self) -> bool {
        match self {
            Self::ImplBlock(_) => true,
            _ => false,
        }
    }

    pub fn is_field_def(&self) -> bool {
        match self {
            Self::FieldDefine(_) => true,
            _ => false,
        }
    }

    pub fn is_exportfn(&self) -> bool {
        match self {
            Self::ExportFn(_) => true,
            _ => false,
        }
    }
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
