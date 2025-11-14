/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::{region::RegionLocation, NodeRef};
use vola_common::{Span, VolaError};

use crate::{alge::buildin::BuildinOp, interval::lower_intervals::LowerIntervals, OptError};

impl<'opt> LowerIntervals<'opt> {
    pub(crate) fn lower_buildin(
        &mut self,
        region: RegionLocation,
        span: Span,
        node: NodeRef,
        op: BuildinOp,
    ) -> Result<(), VolaError<OptError>> {
        todo!()
    }
}
