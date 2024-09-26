use core::str;
use std::{error::Error, process::Command};

fn main() -> Result<(), Box<dyn Error>> {
    //export the dir of the runtime wasm file
    //TODO: make this nicer
    println!(
        "cargo::rustc-env=RUNTIME_DIR={}{}",
        std::env::var("OUT_DIR").expect("Expected CargoTargetDir to be present"),
        "/../../../../wasm32-unknown-unknown/release/wasm_runtime.wasm"
    );

    println!("cargo::rerun-if-changed=../wasm-runtime");
    //probe for cargo
    match Command::new("cargo").arg("--version").output() {
        Ok(output) => {
            if !output.status.success() {
                return Err(format!(
                    "Somthing failed while probing for carge: {}",
                    str::from_utf8(&output.stderr).unwrap_or("could not parse")
                )
                .into());
            }

            let retstr = str::from_utf8(&output.stdout).unwrap_or("could not parse");
            assert!(retstr.contains("cargo"));
        }
        Err(e) => {
            return Err(format!("Failed to probe for \"cargo\": {e}").into());
        }
    }

    //Passed the cargo test, execute wasm builder
    match Command::new("cargo")
        .arg("build")
        .arg("--target")
        .arg("wasm32-unknown-unknown")
        .arg("-p")
        .arg("wasm-runtime")
        .arg("--release")
        .output()
    {
        Ok(output) => {
            //If not success, print err and out
            if !output.status.success() {
                let stderr = str::from_utf8(&output.stderr).unwrap_or("failed to parse stderr");
                let stdout = str::from_utf8(&output.stdout).unwrap_or("failed to parse stdout");

                return Err(format!(
                    "Failed to build wasm-runtime:\nErr:\n{stderr}\n\nOut:\n{stdout}"
                )
                .into());
            }
        }
        Err(e) => return Err(format!("failed to build wasm-runtime: {e}").into()),
    }

    Ok(())
}
