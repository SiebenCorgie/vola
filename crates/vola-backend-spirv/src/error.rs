/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use vola_common::{
    miette::{self, Diagnostic},
    thiserror::{self, Error},
    Reportable,
};

impl Reportable for BackendSpirvError {}

#[derive(Debug, Error, Clone, Diagnostic)]
pub enum BackendSpirvError {
    #[error("{text}")]
    Any { text: String },
}
