/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::attrib::AttribLocation;
use vola_common::VolaError;

use crate::{OptError, Optimizer};

///Makes sure that all live value's edges are indeed type-set.
pub struct TypeEdges<'opt> {
    pub(crate) optimizer: &'opt mut Optimizer,
}

impl<'opt> TypeEdges<'opt> {
    pub fn setup(optimizer: &'opt mut Optimizer) -> Self {
        TypeEdges { optimizer }
    }

    pub fn execute(self) -> Result<(), VolaError<OptError>> {
        #[cfg(feature = "log")]
        log::info!("Type Edges");

        let liveness = self
            .optimizer
            .graph
            .liveness_region(self.optimizer.graph.toplevel_region());
        for (loc, liveness) in liveness.flags.into_iter() {
            if !liveness {
                continue;
            }
            if let AttribLocation::InPort(inport) = loc {
                self.optimizer
                    .get_in_type_mut(inport)
                    .map_err(|e| e.to_error())?;
            }
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_TYPE_EDGES").is_ok() {
            self.optimizer.push_debug_state("type-edges");
        }

        Ok(())
    }
}
