# Interval Arithmetic

Vola supports interval analysis for arithmetic expressions.

You signal the compiler such analysis via the `bound` function that is composed as: `bound($value, $dynamic_value, $interval)`


### Example
´´´
let a = sin(x);
let ai = bound(ai, x, [-INF..INF]);
assert(ai.start == -1.0);
assert(ai.end == 1.0);
´´´

# Restrictions

The analysis only works on strictly arithmetic expressions, I.e. not CSG values. Note, that you can use the result of an evaluated CSG expression with respect to concept.

# Types

- `$value` can be any none-csg expression/value.
- `$dynamic_value` must be a scalar-shaped value (`real`, `int`, `bool` etc.).
- `$interval` must have the same type as `$dynamic_value`.
- The return type is a interval shaped value of `$value`'s type.

# Considered research

- [IEE-1788 presentation](https://kam.mff.cuni.cz/conferences/swim2015/slides/revol.pdf): Discusses all the edge cases that should be taken care of in a _real_ implementation.
