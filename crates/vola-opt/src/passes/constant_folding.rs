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
        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_BEFORE_CNF").is_ok() {
            self.push_debug_state("before CNF");
        }

        let folded = self.graph.constant_fold()?;

        #[cfg(feature = "log")]
        log::info!("folded {} nodes", folded.len());

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_AFTER_CNF").is_ok() {
            self.push_debug_state("after CNF");
        }
        Ok(())
    }
}
