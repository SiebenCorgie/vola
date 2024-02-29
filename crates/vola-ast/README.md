<div align="center">

# Vola-AST

The **A**bstract **S**yntax **T**ree definition for vola.

The Vola-AST aims to be a generic description of CSG-Trees made up from operations and entities, as well as implementations of those Ops and Prims for certain concepts, like SDF, Color, or any other concept you can think of.

[![dependency status](https://deps.rs/repo/gitlab/tendsinmende/vola/status.svg)](https://deps.rs/repo/gitlab/tendsinmende/vola)
[![MPL 2.0](https://img.shields.io/badge/License-MPL_2.0-blue)](LICENSE)

</div>


## Dependencies

The Vola-AST currently only depends on [tree-sitter-vola](https://gitlab.com/tendsinmende/tree-sitter-vola). This is the standard grammar / syntax definition used for a textual representation. However, it should be possible to implement other 
frontends like [openSCAD](https://openscad.org/) as well.

## Usage

The main structures are defined in `src/lib.rs` and most `src/*` files. The tree-sitter parser is implemented in `src/parser.rs` and all sibling modules.


## Hints

- There is a `dot` feature in the `dot` module, which lets you translate an AST to a DOT-Graph and safe it as SVG.
- export `VOLA_BACKTRACE=1` to enable backtraces whenever the AST parser fails. Thats really helpful whenever the parser panics, instead of correctly reporting an error.
