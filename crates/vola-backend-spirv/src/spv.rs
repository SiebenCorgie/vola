/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Defines the SPIR-V dialect nodes for the backend.

use crate::BackendSpirvError;

pub struct SpvType;

///A single SPIR-V dialect node.
pub trait SpvNode {
    fn name(&self) -> String {
        format!("SPIR-V Node")
    }
}
