
### General ideas
- Any *numeric value* is f32 by default
- op / prim value resolution can be changed scope wide via attribute `#[numeric_res(16b)]` for f16.
- can append / change any field via `prim.attrib = xyz`
- `@` is always the "Current evaluation location in n-Dimensions".

- `prim.@` is defined as the signed distance value of any prim.


### Domains
We have three domains we can program.

`prim`: Defines a primitive based on arguments and an evaluation location `@`
`op`: Operation, changes either `@`, prim parameter, or `prim.@` location value.
`field`: scope in which a whole field (tree of ops, where each leaf is a prim) is defined. Can somewhat be considered an `entry_point` in SPIR-V speak.


### Random stuff
- `prim`: is always: functional, pure
- `op`: must be finite recursion (inlineable)
- syntax sugar for iteration `sum`, `mul`.
- `field` 
