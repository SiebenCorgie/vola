<div align="center">

<img src="resources/vola_logo.svg" alt="Vola">

**VO**lume **LA**nguage: A research [DSL](https://en.wikipedia.org/wiki/Domain-specific_language) that evaluates volume functions.

Examples of such functions are [Signed Distance Functions](https://en.wikipedia.org/wiki/Signed_distance_function) or [Unit Gradient Fields](https://www.blakecourter.com/2023/05/18/field-notation.html).
The implementation will focus on SDFs at first.

[![dependency status](https://deps.rs/repo/gitlab/tendsinmende/vola/status.svg)](https://deps.rs/repo/gitlab/tendsinmende/vola)
[![MPL 2.0](https://img.shields.io/badge/License-MPL_2.0-blue)](LICENSE)

</div>

## Disclaimer

The whole thing is highly experimental at the moment. Don't use it for anything serious!

## Goal

We try to create a language and intermediate representation for distance function (DF) that makes it easy to write, and programmatically generate such distance fields. The final compiler should make
it possible to patch code that targets GPU execution at runtime in an efficient manor.

An example field that describes a sphere translated by one unit on `X` might look like this:

```
entity Sphere(radius: real);
operation Translate(trans: vec3);
concept SDF3D: vec3 -> real;

//Implements the SDF3D Concept for the Sphere
impl Sphere for SDF3D(at){
     length(at) - radius
}
//Implements the translation operation on one operand for SDF3D.
impl Translate<sub> for SDF3D(at){
    at = at - trans;
    eval sub.SDF3D(at)
}

export fn myField(pos: vec3) -> real{
    csg field = Translate([1, 0, 0]){
        Sphere(1.0)
    };

    //Returns the result of `field` evaluated for the
    //signed distance field concept SDF3D at the location
    // `pos`.
    eval field.SDF3D(pos)
}
```

For more examples either have a look at the syntax [test corpus](https://gitlab.com/tendsinmende/tree-sitter-vola/-/tree/main/corpus) or the [idea.md](https://gitlab.com/tendsinmende/vola/-/blob/main/docs/ideas.md?ref_type=heads#syntax-examples) file.


## Techstack / Packages

- [Treesitter](https://github.com/tree-sitter/tree-sitter) based grammar + parser
- [WASM](https://github.com/rustwasm/walrus)/[Cranelift](https://cranelift.dev/) based CPU targeting
- [Rspriv](https://github.com/gfx-rs/rspirv) SPIR-V generation for the first MVP
- [SPV-Patcher](https://gitlab.com/tendsinmende/spv-patcher) for runtime shader code patching

Note: The techstack is not set in stone. We might switch to a hand written parser, or take in any dependencies that reduce the workload somehow.

## Packages

- [tree-sitter-vola](https://gitlab.com/tendsinmende/tree-sitter-vola): Treesitter based parser. Also contains the language grammar.
- `vola-ast`: The Abstract-Syntax-Tree representation of any Vola program. Can either be build from a file (using `tree-sitter-vola`) or
by using this as a library. Servers as interface between the Vola frontend, and any middle- / backend.
- `vola-opt`: The RVSDG based optimizer
- `vola-backend-spirv`: SPIR-V backend
- `vola-backend-wasm`: WASM backend based on [Walrus](https://github.com/rustwasm/walrus).
- `wasm-runtime`: Companion crate to the WASM backend. Depends on cargo and the `wasm32-unknown-unknown` toolchain.
- `volac`: The compiler library. Mostly takes care of executing passes of the various parts _in order_.
- `vola-cli`: Thin CLI interface around `volac`
- `rvsdg`: A generic [RVSDG](https://dl.acm.org/doi/abs/10.1145/3391902) implementation.
- `vola-common`: Factors out common components for Vola's compiler stages. These are mostly debugging / error-reporting related.

## Status

📖 Reworking the Syntax 📖

## Building & Running

Make sure that you are on rust-nightly (eg. `rustup default nightly`). Then just build ✨ `cargo build` ✨.

### vola-cli

To compile some file to a SPIR-V file, use the `vola-cli` package. By default it'll output a `output_file_name.spv` in the SPIR-V format.
``` shell
cargo run --bin vola-cli -- path/to/some/file.vola output_file_name
```

### Caveat

The wasm backend currently needs the `bindeps` nightly cargo feature. Either append `-Z bindeps` to your cargo command, or copy the `.cargo/config.toml` file to your project's workspace.

### Testing

There is a test runner in `tests`. It uses all integration tests in `tests/ui` on each backend and reports the results. Run it via
``` shell
cargo run --bin test-runner --release
```
or a specific file via
``` shell
cargo run --bin test-runner -- -f tests/ui/some_file.vola
```

Unit tests are handeled by Rust's native test framework. Just run
```shell
cargo test
```
to execute them.

### Rendering

There is a repository that implements a renderer over at [vola-sdf-renderer](https://gitlab.com/tendsinmende/vola-sdf-renderer). It is not included here, since it has some heavy dependencies and is not compiler related.

### Debugging
You can set `VOLA_BACKTRACE=1` to print a backtrace whenever an error is reported. There are `cargo test` units in place, as well as tree-sitter tests. Those should always
work.

Some packages have a `dot` feature. This lets you create [DOT](https://en.wikipedia.org/wiki/DOT_%28graph_description_language%29) graphs that can then be rendered into SVGs or similar formats. This really helps debugging graph/tree related problems.

## Support

The language's grammar is located in [its own repository](https://gitlab.com/tendsinmende/tree-sitter-vola/-/blob/main/grammar.js).

Some thoughts / documentation regarding the language can be found in `docs/`.

If you are interested in the runtime SPIR-V patching, have a look at [spv-patcher](https://gitlab.com/tendsinmende/spv-patcher).


## Roadmap

- ✨More [stdlib](https://gitlab.com/tendsinmende/vola-sdf-stdlib) features ✨
- Refactor optimization passes based on [egg](https://github.com/egraphs-good/egg)
- Interval arithmetic, similar to auto differentiation by _reinterpreting_ given code.

## License

Licensed under

Mozilla Public License Version 2.0 ([LICENSE](LICENSE) or <https://www.mozilla.org/en-US/MPL/2.0/>)


### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the MPL-2.0 license, without any additional terms or conditions.
