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
    backends::StubBackend,
    backends::{BoxedBackend, Native, Spirv, Wasm},
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

    ///Specifies the emitted format.
    #[arg(long, short, value_enum, default_value_t = Format::Spirv)]
    format: Format,

    ///The source file that gets compiled.
    #[arg()]
    src_file: PathBuf,

    ///Name of the output file. Extension will be added based on the format, if none is present.
    #[arg()]
    output_name: PathBuf,
}

fn main() {
    pretty_env_logger::init();
    let args = Args::parse();

    let backend: BoxedBackend = match args.format {
        Format::Spirv => Box::new(Spirv::new(Target::file(&args.output_name))),
        Format::Native => Box::new(Native::new(Target::file(&args.output_name))),
        Format::WASM => Box::new(Wasm::new(Target::file(&args.output_name))),
        Format::Stub => Box::new(StubBackend::default()),
    };

    //let backend = Box::new(StubBackend::default());

    //configure volac based on the args and execute
    let mut pipeline = volac::Pipeline {
        backend,
        late_cne: !args.no_opt && !args.no_cne,
        late_cnf: !args.no_opt && !args.no_cnf,
        early_cnf: !args.no_opt && !args.no_cnf,
        validate_output: args.validate,
    };

    match pipeline.execute_on_file(&args.src_file) {
        Err(e) => {
            eprintln!("CLI Error: {e:?}");
        }
        Ok(_) => {}
    }
}
