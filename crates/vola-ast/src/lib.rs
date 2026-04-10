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
            .map_err(|e| vec![VolaError::new(e)])?
            .with_seen(file)
            .map_err(|e| vec![VolaError::new(e)])?
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

        AstBuilder::enter(root_ast, workspace, parser)
            .map_err(|e| vec![VolaError::new(e)])?
            .finish()
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

        Ok(
            AstBuilder::enter(root, Path::new("./"), parser)
                .map_err(|e| vec![VolaError::new(e)])?,
        )
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

        let workspace = if workspace.as_path() == Path::new("") {
            log::warn!(
                "File {:?} has no real-parent directory, using \"./\"",
                file.as_ref()
            );
            Path::new("./").to_path_buf()
        } else {
            workspace
        };

        Ok(AstBuilder::enter(parsed, workspace, parser)
            .map_err(|e| vec![VolaError::new(e)])?
            .with_seen(file)
            .map_err(|e| vec![VolaError::new(e)])?)
    }

    pub fn empty() -> Self {
        VolaAst {
            entries: Vec::with_capacity(0),
        }
    }
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
    ///Adds a `directory` for `entry` that'll any occurence of `module entry::*` will be resolved to.
    ///
    /// If one is already given, overwrites it.
    ///
    /// If `directory` is a file, its parent directory will be used.
    pub fn set_external(&mut self, entry: String, directory: PathBuf) -> Result<(), AstError> {
        let dir = if directory.is_file() {
            directory.parent().unwrap().to_path_buf()
        } else {
            directory
        };

        let canonicalized = dir
            .canonicalize()
            .map_err(move |_e| AstError::CanonicalizationFailed(dir))?;
        let _ = self.external_directories.insert(entry, canonicalized);
        Ok(())
    }

    ///Starts parsing a `file`, using the given parser
    fn enter(
        root_ast: VolaAst,
        workspace: impl AsRef<Path>,
        parser: &'parser dyn VolaParser<Error = E>,
    ) -> Result<Self, AstError> {
        let workspace = workspace
            .as_ref()
            .canonicalize()
            .map_err(|_e| AstError::CanonicalizationFailed(workspace.as_ref().to_path_buf()))?
            .to_path_buf();

        Ok(AstBuilder {
            root: root_ast,
            workspace,
            external_directories: AHashMap::with_capacity(0),
            lowered_files: AHashSet::with_capacity(0),
            parser,
        })
    }

    ///Marks `file` as seen, i.e. if encountered, it won't be imported (again).
    pub fn with_seen(mut self, file: impl AsRef<Path>) -> Result<Self, AstError> {
        let _ = self.lowered_files.insert(
            file.as_ref()
                .canonicalize()
                .map_err(|_e| AstError::CanonicalizationFailed(file.as_ref().to_path_buf()))?
                .to_path_buf(),
        );
        Ok(self)
    }

    ///Sets the workspace directory of the builder, i.e. the directory the initial 'root_ast`'s `self` refers to.
    pub fn with_workspace(mut self, workspace: impl AsRef<Path>) -> Self {
        assert!(workspace.as_ref().is_dir());
        self.workspace = self
            .workspace
            .canonicalize()
            .expect("Could not canonicalize workspace")
            .to_path_buf();
        self
    }

    ///Resolves a single iteration, i.e iterates the current top-level of the root-ast,
    ///and calls the resolver for each module.
    ///
    /// Returns true, if any additional modules where encountered, that need to be resolved.
    fn resolve_current(&mut self) -> Result<bool, Vec<VolaError<AstError>>> {
        let mut errors = Vec::with_capacity(0);

        let mut newly_unresolved = false;

        let mut newly_discovered = Vec::new();

        fn canonicalize_and_valid(
            mut try_path: PathBuf,
            span: Span,
        ) -> Result<PathBuf, VolaError<AstError>> {
            try_path.set_extension("vola");
            match try_path.canonicalize() {
                Ok(valid) => {
                    if !valid.exists() || !valid.is_file() {
                        Err(VolaError::error_here(
                            AstError::NoModuleFile { path: valid },
                            span.clone(),
                            "The resolved file does not exist, or is not a file!",
                        ))
                    } else {
                        Ok(valid)
                    }
                }
                Err(e) => Err(VolaError::error_here(
                    AstError::CanonicalizationFailed(try_path),
                    span.clone(),
                    format!("Could not resolve this relative file: {e}"),
                )),
            }
        }

        for module in self
            .root
            .entries
            .extract_if(.., |tl| tl.entry.is_module_import())
        {
            if let TopLevelNode {
                span,
                ct_args: _,
                entry: AstEntry::Module(module),
            } = module
            {
                log::trace!("Resolving module: {} @ {}", module, span);

                if module.is_self() {
                    //We resolve those local to _workspace_
                    let try_path = module.make_directory_local(self.workspace.clone());
                    let valid_path = match canonicalize_and_valid(try_path, span.clone()) {
                        Ok(p) => p,
                        Err(e) => {
                            errors.push(e);
                            continue;
                        }
                    };

                    //if we already have seen that file, don't include it again
                    if self.lowered_files.contains(&valid_path) {
                        log::trace!("Seen {} @ {:?} already...", module, valid_path);
                        continue;
                    }

                    //Parse the file itself.
                    let sub_root_ast = match VolaAst::builder_from_file(&valid_path, self.parser) {
                        Err(e) => {
                            log::error!(
                                "Failed to parse sub-ast {}@{}: with {} errors",
                                module,
                                span,
                                e.len()
                            );
                            errors.extend(e.into_iter());
                            continue;
                        }
                        Ok(sub) => sub,
                    };

                    //NOTE: we don't resolve (i.e finish), because thats-handled below
                    let sub_root_ast = sub_root_ast.abort();

                    //now fix-up an modul's path to be not relative to `try-path` but `workspace`
                    // and push them into our AST
                    for mut tl in sub_root_ast.entries {
                        if let TopLevelNode {
                            span: _,
                            ct_args: _,
                            entry: AstEntry::Module(inner_module),
                        } = &mut tl
                        {
                            //seen a new module directive, therfore we'll need a restart :)
                            newly_unresolved = true;

                            //If this is a project-local path, switch out the path to the new _local_
                            // directory
                            if module.path[0].0 == "self" {
                                let new = inner_module.switch_root_module(&module);
                                log::trace!("Moved local {} to unit-local: {}", inner_module, new);
                                *inner_module = new;
                            }
                        }

                        newly_discovered.push(tl);
                    }

                    //finally tick this file as seen
                    let _ = self.lowered_files.insert(valid_path);
                } else {
                    //for those we check wether we know the origin, if not, we won't touch it.
                    // If we know the origin, we start a _sub-builder_ and fully resolve that in its own context.
                    // We then merge the builder into our own

                    log::trace!("Discovering external {}", module.origin());

                    if let Some(path) = self.external_directories.get(module.origin()) {
                        let try_path = module.make_directory_local(path.clone());
                        let valid_path = match canonicalize_and_valid(try_path, span.clone()) {
                            Ok(p) => p,
                            Err(e) => {
                                errors.push(e);
                                continue;
                            }
                        };

                        //if we already have seen that file, don't include it again
                        if self.lowered_files.contains(&valid_path) {
                            log::trace!("Seen {} @ {:?} already...", module, valid_path);
                            continue;
                        }

                        //Alright, entry-file is valide, start a sub-builder and let it handle itself
                        let mut sub_builder =
                            match VolaAst::builder_from_file(&valid_path, self.parser) {
                                Err(e) => {
                                    log::error!(
                                        "Failed to create sub-ast {}@{}: with {} errors",
                                        module,
                                        span,
                                        e.len()
                                    );
                                    errors.extend(e.into_iter());
                                    continue;
                                }
                                Ok(sub) => sub,
                            };

                        //hook-up our knowledge of external libraries, but omit the current one
                        let mut cleaned_external = self.external_directories.clone();
                        assert!(cleaned_external.remove(module.origin()).is_some());
                        sub_builder.external_directories = cleaned_external;
                        //add our knowledge of _known_ files, i.e. no cross-workspace cycles
                        sub_builder.lowered_files = self.lowered_files.clone();

                        match sub_builder.resolve_all() {
                            Err(e) => {
                                log::error!(
                                    "Failed to resolve sub-ast {} @ {}: with {} errors",
                                    module,
                                    span,
                                    e.len()
                                );
                                errors.extend(e.into_iter());
                                continue;
                            }
                            Ok(_) => {}
                        }

                        //now we can assume that the sub_builder is fully resolved. Therefore we can merge the sub_builder into this builder
                        let AstBuilder {
                            root,
                            workspace: _,
                            external_directories: _,
                            lowered_files,
                            parser: _,
                        } = sub_builder;

                        log::trace!(
                            "Extending root-ast {} @ {} with {} enties",
                            module,
                            span,
                            root.entries.len()
                        );
                        newly_discovered.extend(root.entries.into_iter());
                        //add the _used_ files

                        //now, in theory, lowered_files should be a strict super-set of self.lowered_files, assuming that no
                        // one removes any. However, right now we do a little extra work by merging. If this ever becomes a bottle-neck
                        // consider a copy, and audit the pipeline for this assumption.
                        for newly_seen in lowered_files {
                            let _ = self.lowered_files.insert(newly_seen);
                        }

                        //finally add the file itself to seen-files
                        let _ = self.lowered_files.insert(valid_path);
                    } else {
                        log::error!("Extenal {} has no set directory", module.origin());
                        //Don't know this crate, throw an error.
                        errors.push(VolaError::error_here(
                            AstError::UnknownOrigin(module.origin().to_string()),
                            span.clone(),
                            format!("Consider defining the directory of \"{}\"", module.origin()),
                        ));
                    }
                }
            } else {
                unreachable!()
            }
        }

        //add all discovered _full_ ASTs
        self.root.entries.append(&mut newly_discovered);

        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(newly_unresolved)
        }
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
        if self.external_directories.len() > 0 {
            log::info!(
                "Resolving AST with {} externals: \n{:?}",
                self.external_directories.len(),
                self.external_directories
            );
        } else {
            log::info!("Resolving AST with no externals");
        }

        self.resolve_all()?;
        Ok(self.root)
    }

    ///Aborts the _building_ of the AST, and returns whatever there is at the moment
    pub fn abort(self) -> VolaAst {
        self.root
    }
}
