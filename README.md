<div align="center">

# Vola

**VO**lume **LA**nguage: A research [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) that evaluates volume functions.

Examples of such functions are [Signed Distance Functions](https://en.wikipedia.org/wiki/Signed_distance_function) or [Unit Gradient Fields](https://www.blakecourter.com/2023/05/18/field-notation.html).
The implementation will focus on SDFs at first.

[![dependency status](https://deps.rs/repo/gitlab/tendsinmende/vola/status.svg)](https://deps.rs/repo/gitlab/tendsinmende/vola)
[![MIT](https://img.shields.io/badge/License-MIT-blue)](LICENSE-MIT)
[![APACHE](https://img.shields.io/badge/License-Apache_2.0-blue)](LICENSE-APACHE)

</div>

## Disclaimer 

The whole thing is highly experimental at the moment. Don't use it for anything serious!

## Goal

We try to create a language and intermediate distance function (DF) representation that makes it easy to write, and programmatically generate such distance fields. The final compiler should make 
it possible to patch code that targets GPU execution at runtime in an efficient manor.


## Techstack / Packages

- [Treesitter](https://github.com/tree-sitter/tree-sitter) based grammar + parser
- [MLIR](https://mlir.llvm.org/) based compilation to SPIR-V
- [SPV-Patcher](https://gitlab.com/tendsinmende/spv-patcher) for runtime shader code patching

Note: The techstack is not set in stone. We might switch to a hand written parser, or introduce custom non-MLIR compiler steps if needed.

## Packages

- `vola-parser`: Treesitter based parser. Also contains the language grammar
- `volac`: The MLIR -> SPIR-V compiler.

## Building
### Dependencies
- `vola-parser`: [Treesitter](https://tree-sitter.github.io/tree-sitter/creating-parsers#dependencies) if you want to rebuild / change the parser:
  - Node.js
  - A C Compiler
- `volac`:
  - LLVM / MLIR 16
  - Rust nightly (set by the `rust-toolchain.toml`)

## Getting started

### Runtime SDF patching
**Currently not implemented!**
Application that runtime patches a SDF function of a SPIR-V shader from time to time
``` shell
cargo run --bin runtime-patch
```

### Dry Run
**Currently not implemented!**
Single shot dry run that takes a DF file and compiles it to a SPIR-V function.

``` shell
cargo run --bin dry-run
```

## Support

The language's grammar is located in `crates/vola-parser/grammar.js`

Some thoughts / documentation regarding the language can be found in `docs/`.

If you are interested in the runtime SPIR-V patching, have a look at [spv-patcher](https://gitlab.com/tendsinmende/spv-patcher).


## Roadmap

**Milestone 0**: MVP

- [ ] Simple grammar to get started
- [ ] Parser to MLIR (Melior) binding
- [ ] Simple MLIR -> SPIR-V path (no optimisations, just transforming)
- [ ] SPIR-V test app (`dry-run`) that tests the whole text-file -> SPIR-V chain
- [ ] Vulkan test app (`runtime-patch`) that tests runtime patching of actually executed shaders.

**Milestone 1**

- [ ] Refine grammar to account for goal specified in `docs/goals.md`
- [ ] Find a way to do (static?) derivative calculation on SDF values possibly based on [Enzyme](https://enzyme.mit.edu/)


## License

Licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
