use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    //export the dir of the runtime wasm file
    //see
    // https://rust-lang.github.io/rfcs/3028-cargo-binary-dependencies.html#reference-level-explanation
    let dylibloc = std::env::var("CARGO_CDYLIB_FILE_WASM_RUNTIME")?;
    println!("cargo::rustc-env=RUNTIME_DIR={}", dylibloc);
    Ok(())
}
