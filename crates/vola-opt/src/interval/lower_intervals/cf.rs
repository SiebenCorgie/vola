/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::{
    edge::{InputType, OutputType},
    region::RegionLocation,
    NodeRef,
};
use vola_common::VolaError;

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

        let loop_body = RegionLocation {
            node,
            region_index: 0,
        };

        //Regardless of early-exit or not, push the loop body
        self.region_queue.push_front(loop_body);

        let parent_region = self.optimizer.graph[node].parent.unwrap();

        //now go through each lv and split/assemble them if they are interval typed
        for lv in 0..self.optimizer.graph[node]
            .node_type
            .unwrap_theta_ref()
            .loop_variable_count()
        {
            if !self
                .optimizer
                .find_type(node.input(lv))
                .unwrap()
                .is_interval()
            {
                continue;
            }

            //unwrap the lv-input...
            self.itt_inport(parent_region, node.input(lv));
            //... and the associated lv-arg
            self.itt_outport(
                loop_body,
                node.as_outport_location(OutputType::Argument(lv)),
            );
            //now do the same for the result...
            self.itt_inport(loop_body, node.as_inport_location(InputType::Result(lv)));
            //... and its associated output
            self.itt_outport(
                parent_region,
                node.as_outport_location(OutputType::Output(lv)),
            );
        }

        Ok(())
    }

    pub(crate) fn lower_gamma(&mut self, node: NodeRef) -> Result<(), VolaError<OptError>> {
        //Similar to the theta implemenation we just assemble/disassemble inputs/outputs.
        todo!()
    }
}
