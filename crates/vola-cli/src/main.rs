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
use volac::{
    backends::{BoxedBackend, Native, Spirv, StubBackend, Wasm},
    vola_ast::VolaAst,
    Target,
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum Format {
    ///Emits Vulkan specific SPIR-V
    Spirv,
    ///The platform native format. For instance x86 for your Linux flavour.
    Native,
    ///Emits a web-assembly module
    WASM,
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

    ///Disables only the common-node-elemination passes
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
}

fn main() {
    pretty_env_logger::init();
    let mut args = Args::parse();

    if args.format_file {
        match volac::vola_ast::VolaAst::new_from_file_no_import(
            &args.src_file,
            &volac::vola_tree_sitter_parser::VolaTreeSitterParser,
        ) {
            Ok(ast) => {
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

    let (backend, extension): (BoxedBackend, _) = match args.format {
        Format::Spirv => (
            Box::new(Spirv::new(Target::file(&args.output_name))),
            "spirv",
        ),
        Format::Native => (
            Box::new(Native::new(Target::file(&args.output_name))),
            "bin",
        ),
        Format::WASM => (Box::new(Wasm::new(Target::file(&args.output_name))), "wasm"),
        Format::Stub => (Box::new(StubBackend::default()), "stub"),
    };

    if args.output_name.extension().is_none() {
        //update the extension, if it wasn't set by the filename
        args.output_name.set_extension(extension);
    }

    //configure volac based on the args and execute
    let mut pipeline = volac::Pipeline {
        backend,
        late_cne: !args.no_opt && !args.no_cne,
        late_cnf: !args.no_opt && !args.no_cnf,
        early_cnf: !args.no_opt && !args.no_cnf,
        validate_output: args.validate,
        write_state_on_error: false,
    };

    if args.write_state_on_error {
        pipeline = pipeline.write_on_error();
    }

    match pipeline.execute_on_file(&args.src_file) {
        Err(errors) => {
            for error in errors {
                error.report();
            }
        }
        Ok(_) => {}
    }
}
