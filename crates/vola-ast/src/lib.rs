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
use alge::{AlgeFunc, ImplBlock};
use common::CTArg;
use csg::{CSGConcept, CSGNodeDef, ExportFn, FieldDef};

pub mod alge;
pub mod common;
pub mod csg;
mod error;
pub mod module;
mod passes;

pub use error::AstError;
use smallvec::{smallvec, SmallVec};

use std::path::Path;

#[cfg(feature = "dot")]
pub mod dot;

pub use module::Module;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use vola_common::{ariadne::Label, error::error_reporter, report, FileString, Span};

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AstEntry {
    Comment(Span),
    Concept(CSGConcept),
    CSGNodeDef(CSGNodeDef),
    ImplBlock(ImplBlock),
    FieldDefine(FieldDef),
    ExportFn(ExportFn),
    AlgeFunc(AlgeFunc),
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

    pub fn is_alge_fn(&self) -> bool {
        if let Self::AlgeFunc(_) = self {
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

pub trait VolaParser {
    fn parse_from_byte(
        &self,
        src_file: Option<FileString>,
        byte: &[u8],
    ) -> Result<VolaAst, AstError>;
}

impl VolaAst {
    pub fn new_from_file(
        file: &dyn AsRef<Path>,
        parser: &dyn VolaParser,
    ) -> Result<Self, AstError> {
        let root_file = file.as_ref().to_str().unwrap().into();
        let bytes = std::fs::read(file.as_ref()).map_err(|e| {
            report(
                vola_common::ariadne::Report::build(
                    vola_common::ariadne::ReportKind::Error,
                    "File not found",
                    0,
                )
                .with_message(format!("Could not find root file {:?}", file.as_ref()))
                .finish(),
            );
            AstError::IoError(e.to_string())
        })?;
        let mut root_ast = parser.parse_from_byte(Some(root_file), &bytes)?;

        //now resolve relative to the ast all submodules
        let _ = root_ast.resolve_modules(file, parser)?;

        Ok(root_ast)
    }
    pub fn new_from_bytes(bytes: &[u8], parser: &dyn VolaParser) -> Result<Self, AstError> {
        let root_ast = parser.parse_from_byte(None, &bytes)?;

        //can't use the file system in this case
        let contains_module = root_ast.entries.iter().find(|n| n.entry.is_module_import());
        if let Some(module) = contains_module {
            let err = AstError::NoRootFile;
            report(
                error_reporter(err.clone(), module.span.clone())
                    .with_label(
                        Label::new(module.span.clone())
                            .with_message("consider removing this import"),
                    )
                    .finish(),
            );
            return Err(AstError::NoRootFile);
        }

        Ok(root_ast)
    }

    pub fn empty() -> Self {
        VolaAst {
            entries: Vec::with_capacity(0),
        }
    }

    ///Resloves all imported modules in `Self` relative to the given path.
    pub fn resolve_modules(
        &mut self,
        relative_to: &dyn AsRef<Path>,
        parser: &dyn VolaParser,
    ) -> Result<(), AstError> {
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

                    let mut stem_path = m.path.clone();
                    let _ = stem_path.pop();
                    path.set_extension("vola");

                    if !path.exists() {
                        let err = AstError::NoModuleFile { path };
                        report(
                            error_reporter(err.clone(), span.clone())
                                .with_label(
                                    Label::new(span.clone())
                                        .with_message("Could not find this module's file"),
                                )
                                .finish(),
                        );
                        return Err(err);
                    }

                    let sub_ast = Self::resolve_module(&path, &stem_path, parser)?;
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
