//! File converter utility. Currently converts open-scad files to vola-files using vola's AST representation.
use std::{ffi::OsStr, path::PathBuf};

use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "vola-convert")]
#[command(version, about, long_about = "Vola file converter")]
struct Args {
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

    if args.src_file.extension() != Some(&OsStr::new("scad")) {
        log::warn!("'{:?}' might no be a OpenScad-file", args.src_file);
    }

    let parsed = match vola_tree_sitter_openscad::parse_file(&args.src_file) {
        Ok(k) => k,
        Err(e) => {
            for err in e {
                err.report();
            }
            panic!("Could not parse file!");
        }
    };
    let formated = vola_fmt::Formater::format_ast(&parsed);
    std::fs::write(args.output_name, formated.to_string()).unwrap();
}
