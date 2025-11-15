/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::{region::RegionLocation, NodeRef};
use vola_common::{Span, VolaError};

use crate::{interval::lower_intervals::LowerIntervals, OptError};

impl<'opt> LowerIntervals<'opt> {
    pub(crate) fn lower_theta(&mut self, node: NodeRef) -> Result<(), VolaError<OptError>> {
        //For theta nodes, we basically make sure that any
        // interval that crosses the theta's boundary is mapped to a tuple
        //
        // This means assembling any inputs (loop-argument / loop-output) into a tuple
        // and disassembling the loop-arguments/loop-results via indexing and recording those mappings.
        //
        // After that we'd rewrite the port's types as well as the edge-types.
        todo!()
    }

    pub(crate) fn lower_gamma(&mut self, node: NodeRef) -> Result<(), VolaError<OptError>> {
        //Similar to the theta implemenation we just assemble/disassemble inputs/outputs.
        todo!()
    }
}
