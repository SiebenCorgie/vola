
# Vola

## Content

- [Ideas](ideas.md) (outdated)
- [Goals](goals.md) (outdated)
- [Automatic Differentiation](automatic-differentiation.md)


# Intro

Vola is an experimental languages and compiler. Its goal is to find a pleasant way to work with distance functions. It tries to expose a human understandable model of the way distance functions work and can be combined. This model is then translated by the compiler into machine-executable code. The advantage being, that the distance function never has to be discretized into resolution-dependent approximations like voxels or triangles.


# Trying it out

Todo: Implement a simple web-page that uses the WASM-backend to generate images of a live-edited vola-module.

# Example

Moving a sphere in 3d:
```vola
entity Sphere(radius: s);
operation Translate(trans: vec3);
concept Sdf3d: vec3 -> real;

//Implements the SDF3D Concept for the Sphere
impl Sphere for Sdf3d(at){
     length(at) - radius
}
//Implements the translation operation on one operand for SDF3D.
impl Translate<sub> for Sdf3d(at){
    at = at - trans;
    eval sub.SDF3D(at)
}

export fn myField(pos: vec3) -> real{

    ///The CSG tree that describes our scene
    csg field = Translate([1, 0, 0]){
        Sphere(1.0)
    };

    //Returns the result of `field` evaluated for the
    //signed distance field concept Sdf3d at the location
    // `pos`.
    eval field.Sdf3d(pos)
}
```


In practice, there is a [standard library](https://gitlab.com/tendsinmende/vola-sdf-stdlib/) that implements many entities and operations for you. So the example above looks more like this:

```vola
//import all stdlib entities and operations
module stdlib::prelude;

export fn myField(pos: vec3) -> real{
  csg field = Tran3d([1.0, 0.0, 0.0]){
    Sphere(1.0)
  };

  eval field.Sdf3d(pos)
}

```


**For more usage examples, have a look at all the integration tests in [tests](tests/ui).**


# Basic Understanding

### The CSG-Tree

### Concepts

### Usage in your own application
