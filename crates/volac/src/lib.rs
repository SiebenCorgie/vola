/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # Volac
//!
//! The compiler crate. Ties together the parts of vola into a single compiler.
//! Can either be used as a library, or via the CLI using the `vola-cli` binary.
//!
//!

use backends::{PipelineBackend, Spirv};
use std::path::{Path, PathBuf};
use vola_ast::VolaAst;

mod error;
pub use error::PipelineError;
use vola_common::reset_file_cache;
use vola_opt::Optimizer;

pub mod backends;

///Target the output of the pipeline is compiled to.
#[derive(Clone, Debug)]
pub enum Target {
    File(PathBuf),
    Buffer(Vec<u8>),
}

impl Target {
    pub fn file(file: &dyn AsRef<Path>) -> Self {
        Self::File(file.as_ref().to_path_buf())
    }

    pub fn buffer() -> Self {
        Self::Buffer(Vec::with_capacity(0))
    }

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

    fn target_file_name(&self, extension: &str) -> Option<PathBuf> {
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
    pub backend: Box<dyn PipelineBackend>,

    //If the early constant-node-fold is executed before specializing
    pub early_cnf: bool,
    //If the late, post specializing cnf is executed
    pub late_cnf: bool,
    //If the post-specializing cne is done
    pub late_cne: bool,

    pub validate_output: bool,
}

impl Pipeline {
    pub fn new(output_file: &dyn AsRef<Path>) -> Self {
        Pipeline {
            backend: Box::new(Spirv::new(Target::file(output_file))),

            early_cnf: true,
            late_cnf: true,
            late_cne: true,
            validate_output: false,
        }
    }

    ///Creates a new _in_memory_ pipeline. This will not produce a file, but a buffer after compilation.
    pub fn new_in_memory() -> Self {
        Pipeline {
            backend: Box::new(Spirv::new(Target::buffer())),

            early_cnf: true,
            late_cnf: true,
            late_cne: true,
            validate_output: false,
        }
    }

    pub fn with_backend(mut self, backend: Box<dyn PipelineBackend + 'static>) -> Self {
        self.backend = backend;
        self
    }

    pub fn with_validation(mut self) -> Self {
        self.validate_output = true;
        self
    }

    ///Takes an already prepared AST and tries to turn it into a compiled program / module.
    pub fn execute_on_ast(&mut self, ast: VolaAst) -> Result<Target, PipelineError> {
        let mut opt = Optimizer::new();
        //TODO: add all the _standard_library_stuff_. Would be nice if we'd had them
        //      serialized somewhere.
        opt.add_ast(ast)?;

        if self.early_cnf {
            opt.full_graph_cnf()?;
        }
        //do mandatory type derivation
        opt.type_derive()?;

        opt.specialize_all_exports()?;
        if self.late_cnf {
            opt.full_graph_cnf()?;
        }
        opt.inline_field_exports()?;

        //do some _post_everyting_ cleanup
        if self.late_cne {
            opt.cne_exports().expect("Failed to execute CNE");
        }

        //dispatch autodiff nodes
        opt.dispatch_autodiff()?;

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
                .map_err(|e| PipelineError::ValidationFailed(e))?;
        }

        Ok(result)
    }

    ///Tries to interpret `data` as a string in vola's language
    pub fn execute_on_bytes(&mut self, data: &[u8]) -> Result<Target, PipelineError> {
        //NOTE: Always reset file cache, since the files we are reporting on might have changed.
        reset_file_cache();
        let mut parser = vola_tree_sitter_parser::VolaTreeSitterParser;
        let ast = VolaAst::new_from_bytes(data, &mut parser)?;
        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_AST").is_ok() {
            vola_ast::dot::ast_to_svg(&ast, "ast.svg");
        }

        self.execute_on_ast(ast)
    }

    ///Tries to parse `file`, and turn that into a program, based on the pipeline conifguration.
    pub fn execute_on_file(&mut self, file: &dyn AsRef<Path>) -> Result<Target, PipelineError> {
        //NOTE: Always reset file cache, since the files we are reporting on might have changed.
        reset_file_cache();
        let mut parser = vola_tree_sitter_parser::VolaTreeSitterParser;
        let ast = VolaAst::new_from_file(file, &mut parser)?;

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_DUMP_AST").is_ok() {
            vola_ast::dot::ast_to_svg(&ast, "ast.svg");
        }

        self.execute_on_ast(ast)
    }
}
