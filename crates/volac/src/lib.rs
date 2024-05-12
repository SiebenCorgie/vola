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

use std::{
    io::Write,
    path::{Path, PathBuf},
    process::Stdio,
};
use vola_ast::VolaAst;

mod error;
pub use error::PipelineError;
use vola_backend_spirv::{rspirv::binary::Assemble, SpirvConfig};
use vola_common::reset_file_cache;
use vola_opt::Optimizer;

#[derive(Debug, Clone, Copy)]
pub enum CraneliftTarget {
    X86,
    Wasm,
}

#[derive(Debug, Clone, Copy)]
pub enum Backend {
    Spirv,
    Cranelift(CraneliftTarget),
}

impl Backend {
    pub fn suffix(&self) -> &str {
        match self {
            Backend::Spirv => "spv",
            Backend::Cranelift(CraneliftTarget::X86) => "bin",
            Backend::Cranelift(CraneliftTarget::Wasm) => "wasm",
        }
    }
}

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

    fn target_file_name(&self, format: &Backend) -> Option<PathBuf> {
        if let Self::File(filepath) = self {
            if filepath.extension().is_none() {
                let mut name = filepath.clone();
                name.set_extension(format.suffix());
                Some(name)
            } else {
                Some(filepath.clone())
            }
        } else {
            None
        }
    }

    ///tries to use the `spirv-val` command (if installed) to verify the code
    pub fn try_verify(&self) -> Result<(), String> {
        match self {
            //start the validator on the src
            Self::File(path) => {
                let output = std::process::Command::new("spirv-val")
                    .arg(path)
                    .output()
                    .map_err(|e| e.to_string())?;
                //push output stream
                std::io::stdout().write_all(&output.stdout).unwrap();
                std::io::stderr().write_all(&output.stderr).unwrap();
                if !output.status.success() {
                    Err(format!("Failed to validate module with: {}", output.status))
                } else {
                    Ok(())
                }
            }
            //stream the buffer on stdin.
            Self::Buffer(b) => {
                let mut command = std::process::Command::new("spirv-val")
                    .stdin(Stdio::piped())
                    .spawn()
                    .unwrap();
                let buffcpy = b.clone();
                let mut command_in = command.stdin.take().unwrap();
                std::thread::spawn(move || {
                    command_in
                        .write_all(&buffcpy)
                        .expect("Failed to write to stdin");
                    command_in.flush().unwrap();
                });
                //now wait for it to end and output
                let output = command.wait_with_output().map_err(|e| e.to_string())?;

                std::io::stdout().write_all(&output.stdout).unwrap();
                std::io::stderr().write_all(&output.stderr).unwrap();
                if !output.status.success() {
                    Err(format!("Failed to validate module with: {}", output.status))
                } else {
                    Ok(())
                }
            }
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
///     3.9 (optional) iff 2.8 happened, do cross-λ-optimizations
/// 4. Emit some format based on a configured backend.
pub struct Pipeline {
    ///The format this pipeline compiles to
    pub target_format: Backend,
    pub target: Target,
}

impl Pipeline {
    pub fn new(output_file: &dyn AsRef<Path>) -> Self {
        Pipeline {
            target_format: Backend::Spirv,
            target: Target::file(output_file),
        }
    }

    ///Creates a new _in_memory_ pipeline. This will not produce a file, but a buffer after compilation.
    pub fn new_in_memory() -> Self {
        Pipeline {
            target_format: Backend::Spirv,
            target: Target::buffer(),
        }
    }

    ///Takes an already prepared AST and tries to turn it into a compiled program / module.
    pub fn execute_on_ast(&self, ast: VolaAst) -> Result<Target, PipelineError> {
        let mut opt = Optimizer::new();
        //TODO: add all the _standard_library_stuff_. Would be nice if we'd had them
        //      serialized somewhere.
        opt.add_ast(ast)?;

        //FIXME: This shouldn't be mandatory instead the dispatcher should be able to handle other ContextVariables
        //       then the ones used while dispatching
        opt.inline_alge_fn()?;

        //do mandatory type derivation
        opt.type_derive()?;

        //do mandatory dispatch of all exports
        opt.dispatch_all_exports()?;

        //do some _post_everyting_ cleanup
        opt.cleanup_export_lmd();

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_FINAL_OPT").is_ok() {
            opt.push_debug_state("Final Optimizer state");
        }

        if std::env::var("VOLA_DUMP_VIEWER").is_ok() {
            opt.dump_debug_state(&"OptState.bin");
        }

        //finally use the expected backend
        match self.target_format {
            Backend::Spirv => {
                let spvconfig = SpirvConfig::default();
                let mut backend = vola_backend_spirv::SpirvBackend::new(spvconfig);

                backend.intern_module(&opt)?;

                backend.legalize().unwrap();

                if std::env::var("VOLA_DUMP_ALL").is_ok()
                    || std::env::var("VOLA_SPIRV_FINAL").is_ok()
                {
                    backend.push_debug_state("Final SPIR-V Graph");
                }

                let spvmodule = backend
                    .build()
                    .expect("Failed to build SPIR-V module from backend graph.");

                if std::env::var("VOLA_DUMP_VIEWER").is_ok() {
                    backend.dump_debug_state(&"SpirvState.bin");
                }
                let words = spvmodule.assemble();
                let bytes = bytemuck::cast_slice(&words);

                let mut target = self.target.clone();
                if let Target::File(f) = &mut target {
                    if let Some(new_path) = self.target.target_file_name(&self.target_format) {
                        *f = new_path;
                    }
                    println!("Emitting SPIR-V as {f:?}");
                }
                target.update_from_buffer(bytes);
                Ok(target)
            }
            _ => panic!("Backend {:?} not implemented yet 🫠", self.target_format),
        }
    }

    ///Tries to interpret `data` as a string in vola's language
    pub fn execute_on_bytes(&self, data: &[u8]) -> Result<Target, PipelineError> {
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
    pub fn execute_on_file(&self, file: &dyn AsRef<Path>) -> Result<Target, PipelineError> {
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
