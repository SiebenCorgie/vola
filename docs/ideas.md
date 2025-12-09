## Domains
We have two levels, the simple _program_ level inside `fn`. And the _csg_ level, inside `impl` blocks.

`fn` can be understood as a normal _shader-like_ program. `impl` in contrast defines the resolution of any node of a `CSG` tree inside a `eval` expression. You use that for instance, to describe how the `SDF` of a `Sphere` entity is calculated, when `eval`-uating it in `eval Sphere.SDF(xyz)`.

### Example
First we define that there is a concept `Sdf` that evalues a `real` value based on a `vec3` argument:
```rust
concept Sdf: vec3 -> real;
```

Then we define a sphere, and how that sphere is evaluated in terms of an SDF:
```rust
entity Sphere(radius: real);
impl Sphere for Sdf(p){
    length(p) - radius
}
```

Finally we can create a sphere `csg` entity and evaluate its SDF:
```rust
export fn myfn(position: vec3) -> real(){
    csg my_sphere = Sphere(2.0);
    eval my_sphere.Sdf(position)
}
```

Note that `my_sphere` could intead be an arbitrary CSG-DAG, i.e. a complex object. Have a look at the tests directory for examples.


## Syntax examples:
_At some point we will have a MdBook-Style introduction. So far however, you'll have to have a look at the tests (`tests/ui/*`) and the following short examples._

**Binding an expression to a variable**
```rust
let a = 4 + 10;
```

**A Loop**
```rust
let a = 4.0;
for i in 0..10{
    a = a + 1;
}
assert(a == 14)
```

**A simple function**
```rust
//Shows how an algebraic function is defined. This is basically the same
//as doing `let x = {...};` at any point in the program.
fn some_function(arg0: real, arg1: vec3) -> vec3{
    let a = arg0 * 4.0;
    arg1 - [a; 3]
}
```

**Exporting a entrypoint**
```rust
export fn my_entrypoint(a: real, b: vec3) -> vec3{
    //NOTE: a is _splat_ to a 3-component vector with This
    // syntax.
    b + [a; 3]
}
```
