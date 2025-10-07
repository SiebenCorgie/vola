# Reserved

## Values

- `INF`: Infinity
- `PI`: π
- `EULER`: e

## Keywords

- `eval`: Evaluates a CSG value with respect to a concept.
- `let`: binds a expression to a identifier.
- `impl`
- `for`
- `fn`

## Types

- `csg`
- `int`
- `real`
- `bool`
- `vec`
- `mat`
- `tensor`
- `interval`
- `quaternion`

## Functions

- `dot`: Dot product of two vetors
- `cross`: Cross product of two vectors
- `length` Euclidean length of a vector
- `sqrt`: square-root
- `exp`: natural exponent of a number (i.e. `exp(x)` is the same as $e^x$ )
- `pow`: first parameter raised to the power of the second.
- `min`: minimum of two value
- `max`: maximum of two values
- `mix` / `lerp`: linear interpolation of two values based on a third, follows the GLSL convention.
- `mod`: Modulo operator
- `clamp`: Constrains a number within a bound of two other numbers.
- `fract`: Returns the fractional part of a value.
- `abs`: Returns the positive of a value.
- `round`: Neareast full integer to a number
- `ceil`: Round up
- `floor`: Round down
- `sin` / `cos` / `tan` / `asin` / `acos` / `sinh` / `cosh` / `tanh`/ `asinh` / `acosh` / `atanh`:
- `atan`: called as `atan(x_over_y)`
- `atan2`: A version of atan, where numerator an denumerator are specified.
- `inverse`: Inverts a matrix
- `diff`: Builds the differential of a value with respect to a value, see [Automatic Differentiation](automatic-differentiation.md)
- `bound`: Builds the interval bound for a value, a dynamic value, an the dynamic_value`s bound. See [Interval Arithmetic](interval_arithmetic.md).
