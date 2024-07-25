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

use std::path::PathBuf;

use clap::{Parser, ValueEnum};
use volac::{Backend, CraneliftTarget};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Debug)]
enum Format {
    Spirv,
    X86,
    WASM,
}

#[derive(Parser, Debug)]
#[command(name = "vola-cli")]
#[command(version, about, long_about = "Vola's CLI interface!")]
struct Args {
    ///disables optimizations
    #[arg(short, long, default_value_t = false)]
    no_opt: bool,

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

    let target_format = match args.format {
        Format::Spirv => Backend::Spirv,
        Format::X86 => Backend::Cranelift(CraneliftTarget::X86),
        Format::WASM => Backend::Cranelift(CraneliftTarget::Wasm),
    };

    //configure volac based on the args and execute
    let pipeline = volac::Pipeline {
        target_format,
        target: volac::Target::File(args.output_name),
        late_cne: !args.no_opt,
        late_cnf: !args.no_opt,
        early_cnf: !args.no_opt,
    };

    match pipeline.execute_on_file(&args.src_file) {
        Err(e) => {
            eprintln!("CLI Error: {e:?}");
        }
        Ok(_) => {}
    }
}
