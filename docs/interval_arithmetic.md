# Interval Arithmetic

Vola supports interval analysis for arithmetic expressions.

You signal the compiler such analysis via the `bound` function that is composed as: `bound($value, $dynamic_value, $lower_bound, $upper_bound)`


### Example
´´´
let a = sin(x);
let ai = bound(ai, x, -INF, INF);
assert(ai.start == -1.0);
assert(ai.end == 1.0);
´´´

# Restrictions

The analysis only works on strictly arithmetic expressions, I.e. not CSG values. Note, that you can use the result of an evaluated CSG expression with respect to concept.

# Types

- `$value` can be any none-csg expression/value.
- `$dynamic_value` must be a scala-shaped value (`real`, `int`, `bool` etc.).
- `$lower_bound`/`$upper_bound` must have the same type as `$dynamic_value`.
- The return type is a interval shaped value of `$dynamic_value`'s scalar type.
