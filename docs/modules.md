# Modules

Files can import additional functionality through *modules*. This mechanism is conceptually similar to the `import` statement in Python or `use` in Rust. During resolution, all referenced modules are loaded and deduplicated.

## Example

The following example imports a standard library module:

```
// Imports an SDF CSG definition and the Sdf3d concept
module stdlib::sphere;

// Use both in a function
export fn mysdf(at: vec3) -> real {
    csg c = Sphere(1.0);
    eval c.Sdf3d(at)
}
```


## Paths

## `self` Paths

A path beginning with `self` is resolved relative to the current file.

For example, given two files `a.vola` and `b.vola` located in the same directory, the following statement in `a.vola`:

```
module self::b;
```

imports the contents of `b.vola` into `a.vola`.


## External Paths

Paths that do not begin with `self` are treated as external.

In the earlier example, `stdlib::sphere` refers to a module located in a directory associated with `stdlib`. The resolver searches configured *search paths* to locate the corresponding file.

Search paths can be configured programmatically (e.g., via an `AstBuilder`) or through the CLI. When using `vola-cli`, a search path can be added with:

```
-c <path/to/name>
```


## Parent Navigation with `super`

The `super` path element allows navigation to a parent directory, similar to `../` in Unix-style paths.

Given the following project structure:

```
.
└── my-project/
    ├── stdlib/
    │   ├── a.vola
    │   ├── b.vola
    │   └── some_dir/
    │       └── another_file.vola
    └── my-scenes/
        └── scene.vola
```

The file `scene.vola` can import `another_file.vola` using:

```
module self::super::stdlib::some_dir::another_file;
```
