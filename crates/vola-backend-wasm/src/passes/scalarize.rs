/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Scalarizer that transforms any non-extern defined op that does vector-element wise operations into a
//! set of scalar operations.

use crate::{WasmBackend, WasmError};
