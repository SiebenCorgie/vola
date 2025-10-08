/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use std::collections::VecDeque;

use ahash::AHashSet;
use rvsdg::NodeRef;
use vola_common::VolaError;

use crate::{interval::Interval, OptError, Optimizer};

/// Pass that handles the [interval-extension](https://en.wikipedia.org/wiki/Interval_arithmetic#Interval_extensions_of_general_functions) for any call to
/// `bound()` in the source program.
pub struct IntervalExtension<'opt> {
    optimizer: &'opt Optimizer,
}

impl<'opt> IntervalExtension<'opt> {
    pub fn setup(optimizer: &'opt Optimizer) -> Self {
        IntervalExtension { optimizer }
    }

    pub fn extend_all(mut self) -> Result<(), VolaError<OptError>> {
        let all_entry_points = self.collect_entry_points()?;
        println!("Found {} eps", all_entry_points.len());
        todo!()
    }

    ///Collects all entry-points and verifies that they obey all rules. Returns the bottom-up list of entry-points to the extension.
    fn collect_entry_points(&mut self) -> Result<Vec<NodeRef>, VolaError<OptError>> {
        let mut all_nodes = Vec::with_capacity(0);

        for live_node in self.optimizer.graph.walk_reachable() {
            if self.optimizer.is_node_type::<Interval>(live_node) {
                all_nodes.push(live_node);
            }
        }

        //Since the walk_reachable is already BFS ordered, we just
        // have to reverse the order and are finished.
        all_nodes.reverse();
        Ok(all_nodes)
    }
}

impl Optimizer {
    pub fn interval_extension(&mut self) -> Result<(), VolaError<OptError>> {
        IntervalExtension::setup(self).extend_all()
    }
}
