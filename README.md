<div align="center">

# Vola

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
entity Sphere(radius: s);
operation Translate(trans: vec3);
concept SDF3D: vec3 -> s;

//Implements the SDF3D Concept for the Sphere
impl Sphere for SDF3D(at){
     length(at) - radius
}
//Implements the translation operation on one operand for SDF3D.
impl Translate<sub> for SDF3D(at){
    at = at - trans;
    eval sub.SDF3D(at)
}

export myField(pos: vec3){
    csg field = Translate([1, 0, 0]){
        Sphere(1.0)  
    };
    
    //Returns the result of `field` evaluated for the 
    //signed distance field concept SDF3D at the location
    // `pos`.
    field.SDF3D(pos)
}
```

For more examples either have a look at the syntax [test corpus](https://gitlab.com/tendsinmende/tree-sitter-vola/-/tree/main/corpus) or the [idea.md](https://gitlab.com/tendsinmende/vola/-/blob/main/docs/ideas.md?ref_type=heads#syntax-examples) file.


## Techstack / Packages

- [Treesitter](https://github.com/tree-sitter/tree-sitter) based grammar + parser
- [Cranelift](https://cranelift.dev/) based CPU targeting
- [SPIR-T](https://github.com/EmbarkStudios/spirt) based SPIR-V / GPU targeting (in the future)
- [Rspriv](https://github.com/gfx-rs/rspirv) SPIR-V generation for the first MVP
- [SPV-Patcher](https://gitlab.com/tendsinmende/spv-patcher) for runtime shader code patching

Note: The techstack is not set in stone. We might switch to a hand written parser, or take in any dependencies that reduce the workload somehow.

## Packages

- [tree-sitter-vola](https://gitlab.com/tendsinmende/tree-sitter-vola): Treesitter based parser. Also contains the language grammar.
- `vola-ast`: The Abstract-Syntax-Tree representation of any Vola program. Can either be build from a file (using `tree-sitter-vola`) or 
by using this as a library. Servers as interface between the Vola frontend, and any middle- / backend.
- `vola-opt`: The RVSDG based optimizer
- `vola-backend-spirv`: SPIR-V backend
- `volac`: The compiler library. Mostly takes care of executing passes of the various parts _in order_.
- `vola-cli`: Thin CLI interface around `volac`
- `rvsdg`: A generic [RVSDG](https://dl.acm.org/doi/abs/10.1145/3391902) implementation. 
- `vola-common`: Factors out common components for Vola's compiler stages. These are mostly debugging / error-reporting related.

## Status

_Working on the first, powerful implementation aka. MVB or Milestone 0_.

## Building

✨ _Just run `cargo build`_ ✨

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

The language's grammar is located in [its own repository](https://gitlab.com/tendsinmende/tree-sitter-vola/-/blob/main/grammar.js).

Some thoughts / documentation regarding the language can be found in `docs/`.

If you are interested in the runtime SPIR-V patching, have a look at [spv-patcher](https://gitlab.com/tendsinmende/spv-patcher).


## Roadmap

**Milestone 0**: MVP

- [x] Simple grammar to get started
- [x] Simple middle end pass (static-dispatch, type resolving etc.)
- [ ] middle-end => SPIR-T pass for gpu code emission. 
- [ ] SPIR-V test app (`dry-run`) that tests the whole text-file -> SPIR-V chain
- [ ] Vulkan test app (`runtime-patch`) that tests runtime patching of actually executed shaders.

**Milestone 1**

- [ ] Refine grammar to account for goal specified in `docs/goals.md`
- [ ] Find a way to do (static?) derivative calculation on SDF values possibly based on [Enzyme](https://enzyme.mit.edu/)


## License

Licensed under

Mozilla Public License Version 2.0 ([LICENSE](LICENSE) or <https://www.mozilla.org/en-US/MPL/2.0/>)


### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
