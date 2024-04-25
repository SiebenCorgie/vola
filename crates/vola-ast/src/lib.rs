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

use ahash::AHashSet;
use alge::ImplBlock;
use common::CTArg;
use csg::{CSGConcept, CSGNodeDef, ExportFn, FieldDef};

pub mod alge;
pub mod common;
pub mod csg;
mod error;
mod module;
mod parser;
mod passes;

use error::AstError;
pub use error::ParserError;
use smallvec::smallvec;

use std::path::Path;

#[cfg(feature = "dot")]
pub mod dot;

pub use module::Module;
pub use parser::{parse_file, parse_from_bytes, parse_string};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use vola_common::{report, Span};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AstEntry {
    Comment(Span),
    Concept(CSGConcept),
    CSGNodeDef(CSGNodeDef),
    ImplBlock(ImplBlock),
    FieldDefine(FieldDef),
    ExportFn(ExportFn),
    Module(Module),
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

    pub fn is_module_import(&self) -> bool {
        if let Self::Module(_) = self {
            true
        } else {
            false
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

impl VolaAst {
    pub fn empty() -> Self {
        VolaAst {
            entries: Vec::with_capacity(0),
        }
    }

    ///Resloves all imported modules in `Self` relative to the given path.
    pub fn resolve_modules(&mut self, relative_to: &dyn AsRef<Path>) -> Result<(), AstError> {
        let mut seen_modules = AHashSet::default();
        //add our selfs as a _seen_ module
        let self_path = smallvec![crate::common::Ident(
            relative_to
                .as_ref()
                .file_stem()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned()
        )];
        seen_modules.insert(self_path);

        let base_path = relative_to.as_ref().parent().unwrap().to_path_buf();
        //go through the entry points and recursively parse the modules.
        //We currently do that with a simple restart-loop.
        'module_resolver: loop {
            for entry_idx in 0..self.entries.len() {
                if let TopLevelNode {
                    entry: AstEntry::Module(m),
                    span,
                    ..
                } = &self.entries[entry_idx]
                {
                    //ignore if we imported that already.
                    if seen_modules.contains(&m.path) {
                        //just delete the import and continue
                        self.entries.remove(entry_idx);
                        continue 'module_resolver;
                    }

                    //is an entry. Try to parse it, and if it worked, replace the idx with that.
                    let mut path = base_path.clone();
                    for p in &m.path {
                        path.push(p.0.clone());
                    }

                    path.set_extension("vola");

                    if !path.exists() {
                        let err = AstError::NoModuleFile {
                            path,
                            span: span.clone().into(),
                        };
                        report(err.clone(), span.get_file());
                        return Err(err);
                    }

                    let sub_ast = Self::resolve_module(&path)?;
                    assert!(seen_modules.insert(m.path.clone()));

                    //delete the module statement.
                    self.entries.remove(entry_idx);
                    //now replace the import with the ast's nodes.
                    //then restart the resolver loop
                    for (tlnode_idx, tlnode) in sub_ast.entries.into_iter().enumerate() {
                        self.entries.insert(entry_idx + tlnode_idx, tlnode);
                    }
                    //finally restart
                    continue 'module_resolver;
                }
            }

            //If we reached here, we had no modules anymore, so we can end
            break 'module_resolver;
        }

        Ok(())
    }
}

#[cfg(feature = "serde")]
impl VolaAst {
    pub fn to_sexpr(&self) -> String {
        vola_common::serde_lexpr::to_string(self).unwrap_or("error".to_string())
    }
}
