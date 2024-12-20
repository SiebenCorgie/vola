

### General ideas
- Any *numeric value* is f32 by default
- op / prim value resolution can be changed scope wide via attribute `#[numeric_res(16b)]` for f16.


### Domains
We have two levels, the simple _program_ level inside `fn`. And the _csg_ level, inside `impl` blocks.

`fn` can be understood as a normal _shader-like_ program. `impl` in contrast defines the resolution of any node of a `CSG` tree inside a `eval` expression. You use that for instance, to describe how the `SDF` of a `Sphere` entity is calculated, when `eval`-uating it in `eval Sphere.SDF(xyz)`.

### Syntax examples:

```

//Shows how an algebraic function is defined. This is basically the same
//as doing `let x = {...};` at any point in the program.
fn some_function(arg0: real, arg1: vec3) -> vec3{
    let a = arg0 * 4.0;
    arg1 - [a; 3]
}

**TODO** Add more syntax examples. For now, have a look at `tests/ui/*` which always contains up-to-date examples.
