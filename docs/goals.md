### Goals
High-level goals of the compiler / language


- Fast mutation of distance field code in user defined shaders
- Compiletime derivative calculation for possible Libschitz calculation for [segment tracing](https://onlinelibrary.wiley.com/doi/epdf/10.1111/cgf.13951) if wanted. If wanted means whenever 
`field.d(x)`, `field.d(y)` etc. are accessed.
- Good library support to build Vola AST at runtime within a (Rust) program. Since you might want to build a non-language based frontend (aka a 3D-modeller).



### Undecided

#### Do we want loops?
Pro: 
- Easy repetition (possibly kept to `for` loops)
- Can rewrite `for` as `Sum[i; 0..x]{x + 4}`, or `Mul[i; 0..10]{i + 10}`. Basically using math notation.

Contra:
- Repetition should be defined using (custom) repeat `Op` if possible (using modulo)
- If we give possibility for loops, people might be inclined to use *normal-language* style programming instead of 
mapping to a better fitting paradigm.

Resolution: 
- checkout if there are any formulas that depend on non SUM/MUL style loops (single result, finite). Possibly block `loop` and `while` keyword,
but not implement it.
- Maybe just allow tail-recursion as language feature and implement loops that way.

### Do we want If?
Pro: 
- Known for user
- pretty intuitive: `If @ < 10 {use sphere} else {use box}`

Contra:
- Introduces diverging control flow 
- If we don't have `if` and `loop` (with break, normal `loop[0..x]` is okay), we can be sure all threads execute 
the same code all the time. Which allows us to easily share memory across threads, do vectorization etc.
- We can emulate if `lerp`-style (calculate both, then select)

Resolution: 
- Possibly comes down to implementing the if syntax once lerp style, once with a real jump, 
then profiling.
