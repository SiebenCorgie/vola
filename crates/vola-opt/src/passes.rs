/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! Optimizer passes module.
//!
//! Right now we focus on _construction_ passes. Which means passes, that make it possible
//! to go from the RVSDG to a SPIR-V representation _at all_.
//!
//! Later on we'll probably start introducing optimizations and compiler-based inference of properties.

mod cleanup_backend;
mod constant_folding;
mod field_inliner;
mod inline_alge_fn;
mod specializer;
mod type_derive;
