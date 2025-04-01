/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

#![doc(html_logo_url = "https://gitlab.com/tendsinmende/vola/-/raw/main/resources/vola_icon.svg")]

//! # Volac
//!
//! The compiler crate. Ties together the parts of vola into a single compiler.
//! Can either be used as a library, or via the CLI using the `vola-cli` binary.
//!
//!

pub use rvsdg;
pub use vola_ast;
pub use vola_common;
pub use vola_opt;
pub use vola_tree_sitter_parser;

use backends::{BoxedBackend, StubBackend};
use std::path::{Path, PathBuf};
use vola_ast::VolaAst;

mod error;
pub use error::PipelineError;
use vola_common::{reset_file_cache, VolaError};
use vola_opt::Optimizer;

pub mod backends;

#[cfg(feature = "spirv")]
pub use vola_backend_spirv;

#[cfg(feature = "wasm")]
pub use vola_backend_wasm;

///Target the output of the pipeline is compiled to.
#[derive(Clone, Debug)]
pub enum Target {
    File(PathBuf),
    Buffer(Vec<u8>),
}

impl Target {
    pub fn unwrap_buffer(self) -> Vec<u8> {
        if let Self::Buffer(b) = self {
            b
        } else {
            panic!("Was no buffer!")
        }
    }

    pub fn unwrap_file(self) -> PathBuf {
        if let Self::File(f) = self {
            f
        } else {
            panic!("Was no file!")
        }
    }
    pub fn is_file(&self) -> bool {
        if let Self::File(_) = self {
            true
        } else {
            false
        }
    }

    pub fn file(file: &dyn AsRef<Path>) -> Self {
        Self::File(file.as_ref().to_path_buf())
    }

    pub fn buffer() -> Self {
        Self::Buffer(Vec::with_capacity(0))
    }

    //NOTE: only used by _some_ features.
    #[allow(dead_code)]
    fn update_from_buffer(&mut self, buffer: &[u8]) {
        match self {
            Self::File(f) => {
                if f.exists() {
                    std::fs::remove_file(&f).unwrap();
                };
                std::fs::write(&f, buffer).unwrap();
            }
            Self::Buffer(buf) => {
                *buf = buffer.to_vec();
            }
        }
    }

    //NOTE: only used by _some_ features.
    #[allow(dead_code)]
    pub(crate) fn target_file_name(&self, extension: &str) -> Option<PathBuf> {
        if let Self::File(filepath) = self {
            if filepath.extension().is_none() {
                let mut name = filepath.clone();
                name.set_extension(extension);
                Some(name)
            } else {
                Some(filepath.clone())
            }
        } else {
            None
        }
    }
}

///An executable compilation pipeline.
/// There are always four main steps, with possible sub steps:
/// 1. _somehow_ get an AST,
/// 2. parse `Optimizer` from an AST,
/// 3. optimize based on arguments
///     3.1 Apply High-Level CSG-Optimizations,
///     3.2 Specialize exports
///     3.3 (optional) optimize specialized exports
///     3.4 Specialize field accesses
///     3.5 (optional) optimize specialized field access λs
///     3.6 dispatch csg-trees
///     3.7 (optional) optimize specialized eval-λs
///     3.8 (optional) inline eval-λs
///     3.9 (optional) iff 3.8 happened, do cross-λ-optimizations
/// 4. Automatic differentiation
/// 5. Emit some format based on a configured backend.
pub struct Pipeline {
    pub backend: BoxedBackend,

    //If the early constant-node-fold is executed before specializing
    pub early_cnf: bool,
    //If the late, post specializing cnf is executed
    pub late_cnf: bool,
    //If the post-specializing cne is done
    pub late_cne: bool,

    pub validate_output: bool,
}

impl Pipeline {
    pub fn new() -> Self {
        Pipeline {
            backend: Box::new(StubBackend::default()),

            early_cnf: true,
            late_cnf: true,
            late_cne: true,
            validate_output: false,
        }
    }

    pub fn no_optimization(mut self) -> Self {
        self.early_cnf = false;
        self.late_cne = false;
        self.late_cnf = false;
        self
    }

    pub fn no_common_node_elemination(mut self) -> Self {
        self.early_cnf = false;
        self.late_cnf = false;
        self
    }

    pub fn with_backend(mut self, backend: BoxedBackend) -> Self {
        self.backend = backend;
        self
    }

    pub fn with_validation(mut self) -> Self {
        self.validate_output = true;
        self
    }

    ///Takes an already prepared AST and tries to turn it into a compiled program / module.
    pub fn execute_on_ast(
        &mut self,
        ast: VolaAst,
    ) -> Result<Target, Vec<VolaError<PipelineError>>> {
        let mut opt = Optimizer::new();
        //TODO: add all the _standard_library_stuff_. Would be nice if we'd had them
        //      serialized somewhere.
        opt.add_ast(ast).map_err(|errors| {
            errors
                .into_iter()
                .map(|e| e.to_error::<PipelineError>())
                .collect::<Vec<_>>()
        })?;

        if self.early_cnf {
            opt.full_graph_cnf()
                .map_err(|e| vec![VolaError::new(PipelineError::CnfError(e))])?;
        }
        opt.specialize_all_exports()
            .map_err(|e| e.into_iter().map(|e| e.to_error()).collect::<Vec<_>>())?;

        //At this point any used nodes are hooked up. Therfore clean up
        //any unused garbage
        opt.graph
            .dead_node_elimination()
            .map_err(|e| vec![VolaError::new(PipelineError::from(e))])?;

        if self.late_cnf {
            opt.full_graph_cnf()
                .map_err(|e| vec![VolaError::new(PipelineError::from(e))])?;
        }
        //NOTE: Inliner can be buggy on undefined edges, clean those up.
        opt.remove_unused_edges()
            .map_err(|e| vec![VolaError::new(PipelineError::from(e))])?;
        opt.inline_field_exports()
            .map_err(|e| vec![VolaError::new(PipelineError::from(e))])?;

        //do some _post_everyting_ cleanup
        if self.late_cne {
            opt.cne_exports()
                .map_err(|e| vec![VolaError::new(PipelineError::CneError(e))])?;
        }

        //dispatch autodiff nodes
        opt.dispatch_autodiff().map_err(|e| vec![e.to_error()])?;

        //Call _before-finalize-hook_.
        self.backend.opt_pre_finalize(&mut opt)?;

        opt.cleanup_export_lmd();

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_OPT_FINAL").is_ok() {
            opt.push_debug_state("Final Optimizer state");
        }

        if std::env::var("VOLA_DUMP_VIEWER").is_ok() {
            opt.dump_debug_state(&"OptState.bin");
        }

        let result = self.backend.execute(opt)?;

        if self.validate_output {
            self.backend
                .try_verify()
                .map_err(|e| vec![VolaError::new(PipelineError::ValidationFailed(e))])?;
        }

        Ok(result)
    }

    ///Tries to interpret `data` as a string in vola's language.
    ///Uses `workspace` to resolve any relative files.
    pub fn execute_on_bytes(
        &mut self,
        data: &[u8],
        workspace: impl AsRef<Path>,
    ) -> Result<Target, Vec<VolaError<PipelineError>>> {
        //NOTE: Always reset file cache, since the files we are reporting on might have changed.
        reset_file_cache();
        let mut parser = vola_tree_sitter_parser::VolaTreeSitterParser;
        let ast = VolaAst::new_from_bytes(data, &mut parser, workspace).map_err(|errors| {
            errors
                .into_iter()
                .map(|err| err.to_error::<PipelineError>())
                .collect::<Vec<VolaError<PipelineError>>>()
        })?;
        #[cfg(feature = "dot")]
        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_AST").is_ok() {
            vola_ast::dot::ast_to_svg(&ast, "ast.svg");
        }

        self.execute_on_ast(ast)
    }

    ///Tries to parse `file`, and turn that into a program, based on the pipeline conifguration.
    pub fn execute_on_file(
        &mut self,
        file: &dyn AsRef<Path>,
    ) -> Result<Target, Vec<VolaError<PipelineError>>> {
        //NOTE: Always reset file cache, since the files we are reporting on might have changed.
        reset_file_cache();
        let mut parser = vola_tree_sitter_parser::VolaTreeSitterParser;
        let ast = VolaAst::new_from_file(file, &mut parser).map_err(|errors| {
            errors
                .into_iter()
                .map(|err| err.to_error())
                .collect::<Vec<_>>()
        })?;

        #[cfg(feature = "dot")]
        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_AST").is_ok() {
            vola_ast::dot::ast_to_svg(&ast, "ast.svg");
        }

        self.execute_on_ast(ast)
    }
}

#[cfg(test)]
mod test {
    use std::panic::UnwindSafe;

    use static_assertions::assert_impl_all;

    use crate::Pipeline;
    #[test]
    fn impl_send() {
        assert_impl_all!(Pipeline: Send);
    }

    #[test]
    fn impl_sync() {
        assert_impl_all!(Pipeline: Sync);
    }
    #[test]
    fn impl_unwind_safe() {
        assert_impl_all!(Pipeline: UnwindSafe);
    }
}
