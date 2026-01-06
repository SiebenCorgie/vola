/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Analyses and transformations implemented as functions on [crate::Optimizer] or full-fledged tools.
//!
//! We distinguish analysis and transformations. While the former are read only, and usually lightweight, the latter _transform_ the graph in a meaningful way.
//!
//! There are _lowering_ transformations, most of them required to arrive at a target specific graph-state, and _optimizations_, that do in-place transformations without lowering the abstraction of nodes.
//!
//! We consider a graph in the _target_ state, if all (live) _simple_ nodes are either of the `typelevel`, `imm` or `alge` dialect (i.e. trivially lowered to storage or arithmetic/boolean opertions).
//! To arrive at this state there are three required lowering passes:
//!
//! 1. [Specialize](crate::Optimizer::specialize_all_exports)
//! 2. [AutoDiff](crate::Optimizer::dispatch_autodiff)
//! 3. [IntervalExtension](crate::Optimizer::interval_extension)/[IntervalLowering](crate::Optimizer::interval_to_tuple)
//!
//! Those take care of lowering any high-level operation to lowerlevel representations.

pub mod activity;
mod autodiff_dispatch;
mod cleanup;
pub use cleanup::Cleanup;
mod constant_folding;
mod inliner;
pub use inliner::{InlineAll, InlineExports, Inliner};
mod imm_scalarizer;
pub use imm_scalarizer::ImmScalarize;
mod initial_type_check;
mod interval_dispatch;
pub mod lazy_type;
mod lower_ast;
mod pattern_rewrite;
mod recursion_detection;
mod specializer;
mod type_edges;
pub use type_edges::TypeEdges;
//mod type_derive;
