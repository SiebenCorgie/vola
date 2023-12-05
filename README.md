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

An example field that describes a sphere translated by one unit on `X` might look like this:

```
prim sphere(radius){
  def s;
  s.@ = length(@ - radius);
  s
}

op translate<p>(translation){
  @ -= translation;
  p
}

//Smallest version
field my_sdf(){
  translate<sphere(5.0)>([1.0, 0.0, 0.0])
}

//Or, more readable
field my_sdf(){
  //Define a sphere with radius 0;
  def my_sphere = sphere(5.0);
  //Use OP "translate" on sdf "my_sphere" with the argument vec3 = [1,0,0].
  def translated_sphere = translate<my_sphere>([1.0, 0.0, 0.0]);
  //Return the sphere.
  translated_sphere
}
```

For more examples either have a look at the syntax [test corpus](crates/tree-sitter-vola/corpus) or the [idea.md](https://gitlab.com/tendsinmende/vola/-/blob/main/docs/ideas.md?ref_type=heads#syntax-examples) file.


## Techstack / Packages

- [Treesitter](https://github.com/tree-sitter/tree-sitter) based grammar + parser
- ~[MLIR](https://mlir.llvm.org/) based compilation to SPIR-V~
- [Cranelift](https://cranelift.dev/) based CPU targeting
- [SPIR-T](https://github.com/EmbarkStudios/spirt) based SPIR-V / GPU targeting
- [SPV-Patcher](https://gitlab.com/tendsinmende/spv-patcher) for runtime shader code patching

Note: The techstack is not set in stone. We might switch to a hand written parser, or introduce custom non-MLIR compiler steps if needed.

## Packages

- `tree-sitter-vola`: Treesitter based parser. Also contains the language grammar
- `vola-ast`: The Abstract-Syntax-Tree representation of any Vola program. Can either be build from a file (using `tree-sitter-vola`) or 
by using this as a library. Servers as interface between the Vola frontend, and any middle- / backend.
- `vola-hir`: HighLevel-Intermediate-Representation. Conceptually similar to Rust's [THIR](https://rustc-dev-guide.rust-lang.org/thir.html). Used for type resolving, and matching and building a crude data-dependency graph.
- `rvsdg`: A generic [RVSDG](https://dl.acm.org/doi/abs/10.1145/3391902) implementation. 
- `vola-common`: Factors out common components for Vola's compiler stages. These are mostly debugging / error-reporting related.
- `volac`: Vola-Compiler. An executable that ties together all vola stages into a single executable.

## Building

### non-cargo dependencies

- `tree-sitter-vola`: [Treesitter](https://tree-sitter.github.io/tree-sitter/creating-parsers#dependencies) if you want to rebuild / change the parser:
  - [Trees-Sitter CLI](https://crates.io/crates/tree-sitter-cli)
  - Node.js
  - A C Compiler
- `vola-ast`: none
- `vola-hir`: none
- `rvsdg`: none

## Getting started

### Runtime SDF patching
**Currently not implemented!**
Application that runtime patches a SDF function of a SPIR-V shader from time to time
``` shell
cargo run --bin runtime-patch
```

### Dry Run
**Currently not implemented!**
Single shot dry run that takes a vola file and compiles it to a SPIR-V function (if possible).

If no file is provided, `examples/dry-run/default.vola` will be used.
``` shell
cargo run --bin dry-run -- path/to/vola/file
```

### Debugging
You can set `VOLA_BACKTRACE=1` to print a backtrace whenever an error is reported. There are `cargo test` units in place, as well as tree-sitter tests. Those should always 
work.

Some packages have a `dot` feature. This lets you create [DOT](https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29) graphs that can then be rendered into SVGs or similar formats. This really helps debugging graph/tree related problems.

## Support

The language's grammar is located in `crates/vola-parser/grammar.js`

Some thoughts / documentation regarding the language can be found in `docs/`.

If you are interested in the runtime SPIR-V patching, have a look at [spv-patcher](https://gitlab.com/tendsinmende/spv-patcher).


## Roadmap

**Milestone 0**: MVP

- [x] Simple grammar to get started
- [ ] Simple middle end pass (static-dispatch, type resolving etc.)
- [ ] middle-end => SPIR-T pass for gpu code emission. 
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
