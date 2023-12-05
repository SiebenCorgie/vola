//! # Vola's High-level Intermediate Representation
//!
//! This is our main _custom_ compiler layer, where all the ✨magic✨ happens.
//! For now the the layer has the following responsibilities:
//!
//! 1. Transform VolaAST into RVSDG
//! 2. resolve types
//! 3. build the signed-distance + attributes struct for the SDF result
//! 4. function dispatch for aliasing types on the same function: i.e `f(1f32);` and `f(1i32);` dispatch into `f_f32(1.0)` and `f_i32(1)`.
//!
//!

pub use edge::HirEdge;
pub use ops::HirOp;
use rvsdg::Rvsdg;

mod ast;
mod edge;
mod err;
mod ops;
mod types;

pub type VolaHir = Rvsdg<HirOp, HirEdge>;
