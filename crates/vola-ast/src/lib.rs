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

use ahash::{AHashMap, AHashSet};
use alge::Func;
use common::{CTArg, Comment};
use csg::{CsgConcept, CsgDef, ImplBlock};

pub use error::AstError;
use smallvec::smallvec;
use std::{
    error::Error,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};
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

pub mod util;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum AstEntry {
    Comment(Comment),
    Concept(CsgConcept),
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
    ///Parses `file` using `parser`, and resolves all modules relative to this file.
    ///
    /// Internally uses [AstBuilder] to resolve all modules relative to `file`.
    pub fn new_from_file<E: Error>(
        file: &dyn AsRef<Path>,
        parser: &dyn VolaParser<Error = E>,
    ) -> Result<Self, Vec<VolaError<AstError>>> {
        let root_file = file.as_ref().to_str().unwrap().into();
        let bytes = std::fs::read(file.as_ref())
            .map_err(|e| vec![VolaError::new(AstError::IoError(e.to_string()))])?;
        let root_ast = parser
            .parse_from_byte(Some(root_file), &bytes)
            .map_err(|errs| {
                errs.into_iter()
                    .map(|e| AstError::from_parser_error(e))
                    .collect::<Vec<_>>()
            })?;

        let workspace = file
            .as_ref()
            .parent()
            .expect("Source file has no parent directory");
        AstBuilder::enter(root_ast, workspace, parser)
            .with_seen(file)
            .finish()
    }

    ///Parses `file` using `parser`, and resolves all modules relative to `workspace`.
    pub fn new_from_bytes<E: Error>(
        bytes: &[u8],
        parser: &dyn VolaParser<Error = E>,
        workspace: impl AsRef<Path>,
    ) -> Result<Self, Vec<VolaError<AstError>>> {
        //build a pseudo file we use for error reporting
        let mut pseudo_file = workspace.as_ref().to_path_buf();
        pseudo_file.push(Span::FALLBACK_FILE);
        let root_file: FileString = pseudo_file.as_path().to_str().unwrap().into();

        let root_ast = parser
            .parse_from_byte(Some(root_file), bytes)
            .map_err(|errs| {
                errs.into_iter()
                    .map(|e| AstError::from_parser_error(e))
                    .collect::<Vec<_>>()
            })?;

        AstBuilder::enter(root_ast, workspace, parser).finish()
    }

    ///Parses `bytes` into [VolaAst], but does not resolve [AstEntry::Module]. Use the resulting builder to point to additional resolvable directories, and resolving the modules at will.
    ///
    /// The default directory is `./`, i.e the execution directory. Consider using [set_workspace](AstBuilder::with_workspace) if thats not right.
    ///
    /// On a side node, the file name "no-span-source.vola" will be used, whenever a source file-name is needed.
    pub fn builder_from_bytes<'parser, E: Error>(
        bytes: &[u8],
        parser: &'parser dyn VolaParser<Error = E>,
    ) -> Result<AstBuilder<'parser, E>, Vec<VolaError<AstError>>> {
        let root = parser.parse_from_byte(None, bytes).map_err(|errs| {
            errs.into_iter()
                .map(|e| AstError::from_parser_error(e))
                .collect::<Vec<_>>()
        })?;

        Ok(AstBuilder::enter(root, Path::new("./"), parser))
    }

    ///Parses `file` into [VolaAst], but does not resolve [AstEntry::Module]. Use the resulting builder to point to additional resolvable directories, and resolving the modules at will.
    pub fn builder_from_file<'parser, E: Error>(
        file: &dyn AsRef<Path>,
        parser: &'parser dyn VolaParser<Error = E>,
    ) -> Result<AstBuilder<'parser, E>, Vec<VolaError<AstError>>> {
        let root_file = file.as_ref().to_str().unwrap().into();
        let bytes = std::fs::read(file.as_ref()).map_err(|e| {
            let err = VolaError::new(AstError::IoError(e.to_string()));
            vec![err]
        })?;
        let parsed = parser
            .parse_from_byte(Some(root_file), &bytes)
            .map_err(|errs| {
                errs.into_iter()
                    .map(|e| AstError::from_parser_error(e))
                    .collect::<Vec<_>>()
            })?;
        let workspace = file
            .as_ref()
            .parent()
            .expect("File had no parent directory!")
            .to_path_buf();
        Ok(AstBuilder::enter(parsed, workspace, parser).with_seen(file))
    }

    pub fn empty() -> Self {
        VolaAst {
            entries: Vec::with_capacity(0),
        }
    }
    /*
    ///Resloves all imported modules in `Self` using all context given to the AST.
    ///
    /// Consider using [with_context_dir](Self::with_context_dir) to add directories the resolver might use.
    pub fn resolve_modules<E: Error>(
        &mut self,
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

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(())
        }
    }
    */
}

#[cfg(feature = "serde")]
impl VolaAst {
    pub fn to_sexpr(&self) -> String {
        vola_common::serde_lexpr::to_string(self).unwrap_or("error".to_string())
    }
}

///An possibly unfinished [VolaAst] builder. Handles module resolving etc.
pub struct AstBuilder<'parser, E: Error> {
    root: VolaAst,
    ///The local workspace of this builder, i.e. the directory `self` of the root-ast resolves to.
    workspace: PathBuf,
    ///Look-aside for entries to external modules
    external_directories: AHashMap<String, PathBuf>,
    ///All _files_ where we _know_ that they are already integrated into `root`,
    lowered_files: AHashSet<PathBuf>,

    parser: &'parser dyn VolaParser<Error = E>,
}

impl<'parser, E: Error> AstBuilder<'parser, E> {
    ///Starts parsing a `file`, using the given parser
    fn enter(
        root_ast: VolaAst,
        workspace: impl AsRef<Path>,
        parser: &'parser dyn VolaParser<Error = E>,
    ) -> Self {
        AstBuilder {
            root: root_ast,
            workspace: workspace.as_ref().to_path_buf(),
            external_directories: AHashMap::with_capacity(0),
            lowered_files: AHashSet::with_capacity(0),
            parser,
        }
    }

    ///Marks `file` as seen, i.e. if encountered, it won't be imported (again).
    pub fn with_seen(mut self, file: impl AsRef<Path>) -> Self {
        let _ = self.lowered_files.insert(file.as_ref().to_path_buf());
        self
    }

    ///Sets the workspace directory of the builder, i.e. the directory the initial 'root_ast`'s `self` refers to.
    pub fn with_workspace(mut self, workspace: impl AsRef<Path>) -> Self {
        assert!(workspace.as_ref().is_dir());
        self.workspace = self.workspace.to_path_buf();
        self
    }

    ///Resolves a single iteration, i.e iterates the current top-level of the root-ast,
    ///and calls the resolver for each module.
    ///
    /// Returns true, if any additional modules where encountered, that need to be resolved.
    fn resolve_current(&mut self) -> Result<bool, Vec<VolaError<AstError>>> {
        todo!()
    }

    ///Resolves all `module` directives until no new imports are found.
    pub fn resolve_all(&mut self) -> Result<(), Vec<VolaError<AstError>>> {
        let start = Instant::now();

        let mut last_had_open_modules = true;

        //NOTE:
        while last_had_open_modules {
            if start.elapsed() > Duration::from_secs(10) {
                return Err(vec![VolaError::new(AstError::ResolverTimeout)]);
            }

            last_had_open_modules = self.resolve_current()?;
        }

        Ok(())
    }

    ///Finishes the builder by resolving all imports and returning the full AST
    pub fn finish(mut self) -> Result<VolaAst, Vec<VolaError<AstError>>> {
        self.resolve_all()?;
        Ok(self.root)
    }
}
