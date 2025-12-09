### Goals

High-level goals of the compiler / language:

- Creation of complex operation [CSG](https://en.wikipedia.org/wiki/Constructive_solid_geometry)-[DAGs](https://en.wikipedia.org/wiki/Directed_acyclic_graph) that make up an object
- Such DAGs can describe many properties through the interpreation of _concepts_ on such CSG objects
  - i.e interpretation under _color_, _roughness_, _density_ etc.
- Fast / easy mutation of such structures
- Effortless execution on both CPU and GPU targets


### Non-Goals

- General purpose language
- Support for custom data-types / higher-order types etc.
  - We have a simple, domain specific type-system. This makes the implementation of complex features such as [AutoDiff](automatic-differentiation.md) easy.
