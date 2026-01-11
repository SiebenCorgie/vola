//! # Pattern rewriting for the [RVSDG](rvsdg)
//! Inspired by MLIR's pattern rewriting but intentionally kept much simpler.
//! This aims to be a simple framework to target a set of simple rewrites at a
//! chunk of code. An optimization driver decides when and how to apply the
//! rewriting in order to arrive at better code.
//!
//! The framework is based on three central elements:
//!
//! - [Benefit](benefit): signals the driver how much the benefit of the application of a rewrite produces.
//! - [Pattern](pattern): A trait that must be implemented by some pattern. Possibly assisted by proc-macros in the future.
//! - [Driver](driver): The pattern driver takes care of the application of [Pattern](pattern::PatternRewrite)s in a given [Rvsdg](rvsdg::Rvsdg).

pub mod benefit;
pub mod driver;
pub mod pattern;
pub use benefit::{Benefit, CodeSize, Speed};
pub use driver::{Apply, DriverRecursion, TopoGreedyRewriter};
pub use pattern::PatternRewrite;
