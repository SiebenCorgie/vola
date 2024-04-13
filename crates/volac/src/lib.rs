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

use std::path::{Path, PathBuf};
use vola_ast::VolaAst;

mod error;
pub use error::PipelineError;
use vola_backend_spirv::{rspirv::binary::Assemble, SpirvConfig};
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
    pub target_file: PathBuf,
}

impl Pipeline {
    fn target_file_name(&self) -> PathBuf {
        if self.target_file.extension().is_none() {
            let mut name = self.target_file.clone();
            name.set_extension(self.target_format.suffix());
            name
        } else {
            self.target_file.clone()
        }
    }

    ///Takes an already prepared AST and tries to turn it into a compiled program / module.
    pub fn execute_on_ast(&self, ast: VolaAst) -> Result<PathBuf, PipelineError> {
        let mut opt = Optimizer::new();
        //TODO: add all the _standard_library_stuff_. Would be nice if we'd had them
        //      serialized somewhere.
        opt.add_ast(ast)?;
        //do mandatory type derivation
        opt.type_derive()?;

        //do mandatory dispatch of all exports
        opt.dispatch_all_exports()?;

        //do some _post_everyting_ cleanup
        opt.cleanup_export_lmd();

        //finally use the expected backend
        match self.target_format {
            Backend::Spirv => {
                let spvconfig = SpirvConfig::default();
                let mut backend = vola_backend_spirv::SpirvBackend::new(spvconfig);
                let target_name = self.target_file_name();
                println!("Emitting SPIR-V as {target_name:?}");
                backend.intern_module(&opt)?;

                backend.dump_svg("eyo.svg", false);
                backend.legalize().unwrap();
                let spvmodule = backend.build();
                //now write to file

                if target_name.exists() {
                    std::fs::remove_file(&target_name)?;
                }
                let words = spvmodule.assemble();
                let bytes = bytemuck::cast_vec(words);
                std::fs::write(&target_name, bytes)?;

                Ok(target_name)
            }
            _ => panic!("Backend {:?} not implemented yet 🫠", self.target_format),
        }
    }

    ///Tries to interpret `data` as a string in vola's language
    pub fn execute_on_bytes(&self, data: &[u8]) -> Result<PathBuf, PipelineError> {
        let ast = vola_ast::parse_from_bytes(data).map_err(|(_, mut err)| {
            println!("there where {} errors while parsing data!", err.len());
            err.remove(0)
        })?;
        self.execute_on_ast(ast)
    }

    ///Tries to parse `file`, and turn that into a program, based on the pipeline conifguration.
    pub fn execute_on_file(&self, file: &Path) -> Result<PathBuf, PipelineError> {
        let ast = vola_ast::parse_file(file).map_err(|(_, mut err)| {
            println!("There where {} errors while parsing {file:?}", err.len());
            err.remove(0)
        })?;
        self.execute_on_ast(ast)
    }
}
