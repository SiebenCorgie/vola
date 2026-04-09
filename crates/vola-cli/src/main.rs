/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # Vola-cli
//!
//! CLI interface to `volac`

#![doc(html_logo_url = "https://gitlab.com/tendsinmende/vola/-/raw/main/resources/vola_icon.svg")]

use clap::{Parser, ValueEnum};
use std::path::PathBuf;
use vola_lib::spirv::vola_backend_spirv::SpirvConfig;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum Format {
    ///Emits Vulkan specific SPIR-V
    Spirv,
    ///Emits a web-assembly module
    Wasm,
    ///Stops after optimizing the code, does-not emit anything
    Stub,
}

#[derive(Parser, Debug)]
#[command(name = "vola-cli")]
#[command(version, about, long_about = "Vola's CLI interface!")]
struct Args {
    ///only formats the given file, without any compilation
    #[arg(long, default_value_t = false)]
    format_file: bool,

    ///disables optimizations
    #[arg(long, default_value_t = false)]
    no_opt: bool,

    ///Disables only the constant-node-folding passes
    #[arg(long, default_value_t = false)]
    no_cnf: bool,

    ///Disables only the common-node-elimination passes
    #[arg(long, default_value_t = false)]
    no_cne: bool,

    ///Turns on validation of the output fragment. Note that spirv-tools or wasm-tools must be installed.
    #[arg(long, default_value_t = false)]
    validate: bool,

    ///Writes the optimizer state to file, if an error ocures.
    #[arg(long, short, default_value_t = false)]
    write_state_on_error: bool,

    ///Specifies the emitted format.
    #[arg(long, short, value_enum, default_value_t = Format::Spirv)]
    format: Format,

    ///The source file that gets compiled.
    #[arg()]
    src_file: PathBuf,

    ///Name of the output file. Extension will be added based on the format, if none is present.
    #[arg(default_value = "out")]
    output_name: PathBuf,

    ///Adds the given path as context to the compiler. Adding `some/path/to/stdlib` will
    /// resolve any included module that starts with `stdlib` by searching that folder.
    ///
    /// If a file is given, the parent-directory is used.
    #[arg(short = 'c', long = "context")]
    context: Vec<PathBuf>,
}

fn main() {
    pretty_env_logger::init();
    let mut args = Args::parse();

    if args.format_file {
        match vola_ast::VolaAst::builder_from_file(
            &args.src_file,
            &vola_tree_sitter_parser::VolaTreeSitterParser,
        ) {
            Ok(ast_builder) => {
                let ast = ast_builder.abort();
                let formated = vola_fmt::Formater::format_ast(&ast);
                std::fs::write(&args.src_file, formated.to_string()).unwrap();
                return;
            }
            Err(e) => {
                println!("Could not format {:?}", args.src_file);
                for e in e {
                    e.report();
                }

                return;
            }
        }
    }

    let extension = match args.format {
        Format::Spirv => "spirv",
        Format::Wasm => "wasm",
        Format::Stub => "stub",
    };

    if args.output_name.extension().is_none() {
        //update the extension, if it wasn't set by the filename
        args.output_name.set_extension(extension);
    }

    let mut module = vola_lib::OptModule::new();

    let externals = args.context.into_iter().map(|dir| {
        //make actual dir
        let dir = if dir.is_file() {
            dir.parent().unwrap().to_path_buf()
        } else {
            dir
        };

        let name = dir
            .file_name()
            .map(|osdir| {
                osdir
                    .to_str()
                    .expect("Could not turn dir-name into context-entry name")
                    .to_owned()
            })
            .expect("Could not turn directory into filename");
        (name, dir)
    });

    let ast = match vola_lib::passes::LowerAst::from_file(&args.src_file, externals) {
        Ok(ast) => ast,
        Err(e) => {
            for e in e {
                e.report();
            }
            return;
        }
    };
    if let Err(e) = module.apply_pass(ast) {
        for e in e.errors {
            e.report();
        }
        return;
    }

    //TODO use the arg flags to _safely_ disable some passes.

    if let Err(e) = module.standard_pipeline() {
        for e in e.errors {
            e.report();
        }
        return;
    }

    //Depending on the chosen backend, launch one of the given backends
    match args.format {
        Format::Spirv => {
            let config = SpirvConfig::default();

            let mut backend = vola_lib::spirv::SpirvModule::new(config);
            if let Err(e) = backend.lower_opt(module) {
                println!("Failed to lower Optimizer to SPIRV-Backend: {e}");
                return;
            }

            match backend.build(args.validate) {
                Ok(code) => match std::fs::write(&args.output_name, code) {
                    Ok(_) => {
                        println!("Wrote to {:?}", args.output_name);
                    }

                    Err(e) => {
                        println!("Failed to write SPIR-V to file: {e}");
                    }
                },
                Err(e) => {
                    println!("Failed to emit SPIRV: {e}");
                    return;
                }
            }
        }
        Format::Wasm => {
            let mut backend = vola_lib::wasm::WasmModule::new();
            if let Err(e) = backend.lower_opt(module) {
                println!("Failed to lower Optimizer to WASM-Backend: {e}");
                return;
            }

            match backend.build(args.validate) {
                Ok(code) => match std::fs::write(&args.output_name, code) {
                    Ok(_) => println!("Wrote to {:?}", args.output_name),
                    Err(e) => {
                        println!("Failed to write WASM to file: {e}");
                    }
                },
                Err(e) => {
                    println!("Failed to emit WASM: {e}");
                    return;
                }
            }
        }
        Format::Stub => {}
    }
}
