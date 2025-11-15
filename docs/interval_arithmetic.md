# Interval Arithmetic

Vola supports interval analysis for arithmetic expressions.

You signal the compiler such analysis via the `bound` function that is composed as: `bound($value, $dynamic_value, $interval)`

Operations are generally evaluated conservatively. If no tight bound can be calculated, conservative intervals are created instead. Any interval contains the tight interval of the expression, but might be bigger[^0].

[^0]: See [T. Hickey's](https://fab.cba.mit.edu/classes/S62.12/docs/Hickey_interval.pdf) introduction to interval arithmetics.

### Example

```
let a = sin(x);
let ai = bound(ai, x, [-INF..INF]);
assert(ai.start == -1.0);
assert(ai.end == 1.0);
```

# Restrictions

The analysis only works on strictly arithmetic expressions, I.e. not CSG values. Note, that you can use the result of an evaluated CSG expression with respect to concept.

# Types

- `$value` can be any none-csg expression/value.
- `$dynamic_value` must be a scalar-shaped value (`real`, `int`, `bool` etc.).
- `$interval` must have the same type as `$dynamic_value`.
- The return type is a interval shaped value of `$value`'s type.

# General usage

All[^1] supported operations on arithmetic expressions are also supperted in intervals of the same type. It generally makes sense to express ranges as intervals. For instance a AABB trivially makes sense as `interval<vec3>`. Any bounding box transformation can then be expressed as operations over this value, like addition for offsetting, multiplication for growing etc.

[^1]: We support all arithmetic operators, however, boolean operations are still a little spotty.


# On _why we can't have nice things_

Using interval arithmetic introduces an interesting problem related to rounding. Depending on which part of an interval a floating-point operation is applied to, rounding _should_ be reconfigured.
Now, there is a standard (IEE1788) for interval arithmetic on IEEE754 floats (the common floats). However, we are mostly targeting GPUs with Vola. The problem being that we can be lucky if a GPU implements the actual IEEE754 standard. SPIRV can set the [rounding mode](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_fp_rounding_mode), but IIRC only program-wide, so that's out of the question. There is a [LLVM discussion](https://github.com/llvm/llvm-project/issues/87050) for this kind of problem, it might be worth checking that out at some point.

For now we just _don't_ do any rounding consideration. If this ever becomes an actual issue, though, we might just add a conservative _epsilon_ value.

# Considered research

- [IEE-1788 presentation](https://kam.mff.cuni.cz/conferences/swim2015/slides/revol.pdf): Discusses all the edge cases that should be taken care of in a _real_ implementation.
