### Goals
High-level goals of the compiler / language


- Fast mutation of distance field code in user defined shaders
- Compiletime derivative calculation for possible Libschitz calculation for [segment tracing](https://onlinelibrary.wiley.com/doi/epdf/10.1111/cgf.13951) if wanted. If wanted means whenever
`field.d(x)`, `field.d(y)` etc. are accessed.
- Good library support to build Vola AST at runtime within a (Rust) program. Since you might want to build a non-language based frontend (aka a 3D-modeller).
