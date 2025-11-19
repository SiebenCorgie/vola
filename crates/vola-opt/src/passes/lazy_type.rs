/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Implements lazy type discovery on the graph.
//!
//! The whole algorithm operates under the following key-assumption:
//!
//! 1. Any argument of a λ-Node has a valid type-tag
//! 2. Any result of a λ-Node has a valid type-tag
//! 3. Simple-Nodes with no inputs always successfuly output a type on [DialectNode::try_derive_type]
//! 4. All inputs of a simple/apply node are in fact connected.
//!
//! If those assumptions are holding true, the type-discovery algorithm always returns a type for any given out- or input.

use rvsdg::{
    attrib::AttribLocation,
    edge::{InportLocation, OutportLocation},
};
use thiserror::Error;
use vola_common::{Span, VolaError};

use crate::{common::Ty, Optimizer};

mod initial;

#[derive(Debug, Error, Clone)]
pub enum TypeError {
    ///On type collisions in the graph. Generally indicates a bug in some compiler pass.
    #[error("Type collision on {location}:\nset:        {was}\nderived to: {derived}")]
    Collision {
        location: AttribLocation,
        was: Ty,
        derived: Ty,
    },
    #[error("A key assumption was violated at {0}")]
    AssumptionViolation(AttribLocation),
}

impl Optimizer {
    ///Returns the type of `outport` without setting any edge-types in the process.
    pub fn get_out_type(&self, outport: OutportLocation) -> Result<Ty, VolaError<TypeError>> {
        todo!()
    }

    ///Returns the type of `inport` without setting any edge-types in the process.
    pub fn get_in_type(&self, inport: InportLocation) -> Result<Ty, VolaError<TypeError>> {
        if let Some(src) = self.graph.inport_src(inport) {
            self.get_out_type(src)
        } else {
            let span = self.find_span(inport).unwrap_or(Span::empty());
            Err(VolaError::error_here(
                TypeError::AssumptionViolation(inport.into()),
                span,
                "undefined value",
            ))
        }
    }

    ///Returns the type of `outport`. Sets all known type state on unset-edges along the way.
    pub fn get_out_type_mut(
        &mut self,
        outport: OutportLocation,
    ) -> Result<Ty, VolaError<TypeError>> {
        todo!()
    }

    ///Returns the type of `outport`. Sets all known type state on unset-edges along the way.
    pub fn get_in_type_mut(&mut self, inport: InportLocation) -> Result<Ty, VolaError<TypeError>> {
        if let Some(src) = self.graph.inport_src(inport) {
            self.get_out_type_mut(src)
        } else {
            let span = self.find_span(inport).unwrap_or(Span::empty());
            Err(VolaError::error_here(
                TypeError::AssumptionViolation(inport.into()),
                span,
                "undefined value",
            ))
        }
    }
}
