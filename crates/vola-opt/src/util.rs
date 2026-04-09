/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::{NodeRef, SmallColl};
use vola_common::VolaError;

use crate::{OptError, Optimizer, common::Ty};

pub mod macros_graph;

impl Optimizer {
    ///Returns the type signature of each argument, as well as the return type of each result
    pub fn lambda_signature(
        &self,
        lambda: NodeRef,
    ) -> Result<(SmallColl<Ty>, SmallColl<Ty>), VolaError<OptError>> {
        let mut args = SmallColl::default();
        for arg in self.graph.argument_ports(lambda.as_region(0)) {
            let ty = self
                .get_out_type(arg)
                .map_err(|e| e.to_error::<OptError>())?;
            args.push(ty);
        }

        let mut results = SmallColl::default();
        for res in self.graph.result_ports(lambda.as_region(0)) {
            let ty = self
                .get_in_type(res)
                .map_err(|e| e.to_error::<OptError>())?;
            results.push(ty);
        }

        Ok((args, results))
    }
}
