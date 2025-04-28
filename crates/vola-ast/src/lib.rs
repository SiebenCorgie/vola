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

#![doc(html_logo_url = "https://gitlab.com/tendsinmende/vola/-/raw/main/resources/vola_icon.svg")]

use ahash::AHashSet;
use alge::Func;
use common::{CTArg, Comment};
use csg::{CSGConcept, CsgDef, ImplBlock};

pub use error::AstError;
use smallvec::smallvec;
use std::{error::Error, path::Path};
#[cfg(feature = "dot")]
pub mod dot;
pub use module::Module;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use vola_common::{FileString, Span, VolaError};

pub mod alge;
pub mod common;
pub mod csg;
mod error;
pub mod module;
mod passes;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AstEntry {
    Comment(Comment),
    Concept(CSGConcept),
    CsgDef(CsgDef),
    ImplBlock(ImplBlock),
    Func(Func),
    Module(Module),
}

impl AstEntry {
    ///Returns true for CSGNodeDef and Concept
    pub fn is_def_node(&self) -> bool {
        match self {
            Self::Concept(_) | Self::CsgDef(_) => true,
            _ => false,
        }
    }

    pub fn is_impl_block(&self) -> bool {
        match self {
            Self::ImplBlock(_) => true,
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

    pub fn is_fn(&self) -> bool {
        if let Self::Func(_) = self {
            true
        } else {
            false
        }
    }

    pub fn is_export_fn(&self) -> bool {
        if let Self::Func(f) = self {
            f.is_export
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

///Collection of [TopLevelNode] nodes.
///
/// To turn a program back into a source-string, use [ToString]. This won't respect the `span` parts of the nodes however.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct VolaAst {
    pub entries: Vec<TopLevelNode>,
}

pub trait VolaParser {
    type Error: Error;
    fn parse_from_byte(
        &self,
        src_file: Option<FileString>,
        byte: &[u8],
    ) -> Result<VolaAst, Vec<VolaError<Self::Error>>>;
}

impl VolaAst {
    pub fn new_from_file<E: Error>(
        file: &dyn AsRef<Path>,
        parser: &dyn VolaParser<Error = E>,
    ) -> Result<Self, Vec<VolaError<AstError>>> {
        let root_file = file.as_ref().to_str().unwrap().into();
        let bytes = std::fs::read(file.as_ref())
            .map_err(|e| vec![VolaError::new(AstError::IoError(e.to_string()))])?;
        let mut root_ast = parser
            .parse_from_byte(Some(root_file), &bytes)
            .map_err(|errs| {
                errs.into_iter()
                    .map(|e| AstError::from_parser_error(e))
                    .collect::<Vec<_>>()
            })?;

        //now resolve relative to the ast all submodules
        let _ = root_ast.resolve_modules(file, parser)?;

        Ok(root_ast)
    }

    pub fn new_from_bytes<E: Error>(
        bytes: &[u8],
        parser: &dyn VolaParser<Error = E>,
        workspace: impl AsRef<Path>,
    ) -> Result<Self, Vec<VolaError<AstError>>> {
        //build a pseudo file we use for error reporting
        let mut pseudo_file = workspace.as_ref().to_path_buf();
        pseudo_file.push("pseudo_source.vola");
        let root_file: FileString = pseudo_file.as_path().to_str().unwrap().into();

        let mut root_ast = parser
            .parse_from_byte(Some(root_file), &bytes)
            .map_err(|errs| {
                errs.into_iter()
                    .map(|e| AstError::from_parser_error(e))
                    .collect::<Vec<_>>()
            })?;
        let _ = root_ast.resolve_modules(&pseudo_file, parser)?;
        Ok(root_ast)
    }

    ///Parses `bytes` into [VolaAst], but does not resolve [AstEntry::Module]. Use either [VolaAst::new_from_bytes] to do that automatically, or [VolaAst::resolve_modules] to do that manually.
    ///
    /// On a side node, the file name "pseudo_source.vola" will be used, whenever a source file-name is needed.
    pub fn new_from_bytes_no_import<E: Error>(
        bytes: &[u8],
        parser: &dyn VolaParser<Error = E>,
    ) -> Result<Self, Vec<VolaError<AstError>>> {
        parser.parse_from_byte(None, &bytes).map_err(|errs| {
            errs.into_iter()
                .map(|e| AstError::from_parser_error(e))
                .collect::<Vec<_>>()
        })
    }

    ///Parses `file` into [VolaAst], but does not resolve [AstEntry::Module]. Use either [VolaAst::new_from_file] to do that automatically, or [VolaAst::resolve_modules] to do that manually.
    pub fn new_from_file_no_import<E: Error>(
        file: &dyn AsRef<Path>,
        parser: &dyn VolaParser<Error = E>,
    ) -> Result<Self, Vec<VolaError<AstError>>> {
        let root_file = file.as_ref().to_str().unwrap().into();
        let bytes = std::fs::read(file.as_ref()).map_err(|e| {
            let err = VolaError::new(AstError::IoError(e.to_string()));
            vec![err]
        })?;
        parser
            .parse_from_byte(Some(root_file), &bytes)
            .map_err(|errs| {
                errs.into_iter()
                    .map(|e| AstError::from_parser_error(e))
                    .collect::<Vec<_>>()
            })
    }

    pub fn empty() -> Self {
        VolaAst {
            entries: Vec::with_capacity(0),
        }
    }

    ///Resloves all imported modules in `Self` relative to the given path.
    pub fn resolve_modules<E: Error>(
        &mut self,
        relative_to: &dyn AsRef<Path>,
        parser: &dyn VolaParser<Error = E>,
    ) -> Result<(), Vec<VolaError<AstError>>> {
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
        let mut errors = Vec::new();
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
                    //Resolve the `super` path component to unix-style
                    //"super".
                    for p in &m.path {
                        if p.0.as_str() == "super" {
                            path.push("..");
                        } else {
                            path.push(p.0.clone());
                        }
                    }

                    let mut stem_path = m.path.clone();
                    let _ = stem_path.pop();
                    path.set_extension("vola");

                    if !path.exists() {
                        let err = VolaError::error_here(
                            AstError::NoModuleFile { path },
                            span.clone(),
                            "could not find this module's file",
                        );
                        errors.push(err);
                        continue;
                    }

                    let path = if let Ok(canonical) = path.canonicalize() {
                        canonical
                    } else {
                        path
                    };

                    let sub_ast = match Self::resolve_module(&path, &stem_path, parser) {
                        Ok(sub) => sub,
                        Err(mut e) => {
                            errors.append(&mut e);
                            continue;
                        }
                    };
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

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(())
        }
    }
}

#[cfg(feature = "serde")]
impl VolaAst {
    pub fn to_sexpr(&self) -> String {
        vola_common::serde_lexpr::to_string(self).unwrap_or("error".to_string())
    }
}
