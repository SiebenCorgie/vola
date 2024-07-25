/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use rvsdg::util::cnf::CnfError;

use crate::Optimizer;

impl Optimizer {
    pub fn full_graph_cnf(&mut self) -> Result<(), CnfError> {
        let folded = self.graph.constant_fold()?;

        #[cfg(feature = "log")]
        log::info!("folded {} nodes", folded.len());

        println!("Folded {} nodes", folded.len());

        Ok(())
    }
}
