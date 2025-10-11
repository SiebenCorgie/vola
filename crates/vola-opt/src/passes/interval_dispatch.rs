/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::NodeRef;
use vola_common::{Span, VolaError};

use crate::{interval::IntervalExtension, OptError, Optimizer};

/// Pass that handles the [interval-extension](https://en.wikipedia.org/wiki/Interval_arithmetic#Interval_extensions_of_general_functions) for any call to
/// `bound()` in the source program.
pub struct IntervalExtensionPass<'opt> {
    optimizer: &'opt Optimizer,
}

impl<'opt> IntervalExtensionPass<'opt> {
    pub fn setup(optimizer: &'opt Optimizer) -> Self {
        IntervalExtensionPass { optimizer }
    }

    pub fn extend_all(mut self) -> Result<(), VolaError<OptError>> {
        let all_entry_points = self.collect_entry_points()?;

        for ep in all_entry_points {
            let ep_span = self.optimizer.find_span(ep.into()).unwrap_or(Span::empty());

            if let Err(e) = self.expand_entry(ep) {
                return Err(e.with_label(ep_span, "While extending this interval"));
            }
        }

        Ok(())
    }

    ///Collects all entry-points and verifies that they obey all rules. Returns the bottom-up list of entry-points to the extension.
    fn collect_entry_points(&mut self) -> Result<Vec<NodeRef>, VolaError<OptError>> {
        let mut all_nodes = Vec::with_capacity(0);

        for live_node in self.optimizer.graph.walk_reachable() {
            if self.optimizer.is_node_type::<IntervalExtension>(live_node) {
                all_nodes.push(live_node);
            }
        }

        //Since the walk_reachable is already BFS ordered, we just
        // have to reverse the order and are finished.
        all_nodes.reverse();
        Ok(all_nodes)
    }

    fn expand_entry(&mut self, _entry_point: NodeRef) -> Result<(), VolaError<OptError>> {
        Err(VolaError::new(OptError::Any {
            text: "Interval arithmetic not yet implemented".to_owned(),
        }))
    }
}

impl Optimizer {
    pub fn interval_extension(&mut self) -> Result<(), VolaError<OptError>> {
        IntervalExtensionPass::setup(self).extend_all()
    }
}
