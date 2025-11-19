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
    ///Lowers theta node by changing any interval-typed loop variables to tuple. Returns true
    /// if actually any nodes where changed.
    pub(crate) fn lower_theta(&mut self, node: NodeRef) -> Result<bool, VolaError<OptError>> {
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
        let mut changed_any = false;
        for lv in 0..self.optimizer.graph[node]
            .node_type
            .unwrap_theta_ref()
            .loop_variable_count()
        {
            //ignore loop variables that are not in use, i.e where
            // the input is not connected and/or the result is not in use
            if self.optimizer.graph[node.input(lv)].edge.is_none()
                && self.optimizer.graph[node.output(lv)].edges.is_empty()
            {
                continue;
            }
            if !self
                .optimizer
                .get_in_type_mut(node.input(lv))
                .unwrap()
                .is_interval()
            {
                continue;
            }
            changed_any = true;

            //unwrap the lv-input...
            self.itt_inport(parent_region, node.input(lv));
            //... and the associated lv-arg
            // NOTE: Inner outputs, i.e the lv_argument must always connect
            //       in case of an immutable value thought, it connects with itself
            self.itt_outport(
                loop_body,
                node.as_outport_location(OutputType::Argument(lv)),
            );
            //now do the same for the result...
            self.itt_inport(loop_body, node.as_inport_location(InputType::Result(lv)));
            //... and its associated output
            //Ignore unconneted branches, this happens a lot and generates useless indexing code..
            let lv_output = node.as_outport_location(OutputType::Output(lv));
            if !self.optimizer.graph[lv_output].edges.is_empty() {
                self.itt_outport(parent_region, lv_output);
            }
        }

        Ok(changed_any)
    }

    ///Lowers gamma node by changing any interval-typed entry/exit variables to tuple. Returns true
    /// if actually any nodes where changed.
    pub(crate) fn lower_gamma(&mut self, node: NodeRef) -> Result<bool, VolaError<OptError>> {
        let region_count = self.optimizer.graph[node].regions().len();
        let parent_region = self.optimizer.graph[node].parent.unwrap();

        //push each branch region
        for region_index in 0..region_count {
            self.region_queue
                .push_front(RegionLocation { node, region_index });
        }

        let mut changed_any = false;

        //Similar to the theta implemenation we just assemble/disassemble inputs/outputs.
        for ev in 0..self.optimizer.graph[node]
            .node_type
            .unwrap_gamma_ref()
            .entry_var_count()
        {
            let ev_input = node.as_inport_location(InputType::EntryVariableInput(ev));

            if self.optimizer.graph[ev_input].edge.is_none() {
                continue;
            }

            if !self
                .optimizer
                .get_in_type_mut(ev_input)
                .unwrap()
                .is_interval()
            {
                continue;
            }
            changed_any = true;

            //allright, has a entry variable, unwrap the input, and the argument in each branch
            self.itt_inport(parent_region, ev_input);
            for region_index in 0..region_count {
                let branch_region = RegionLocation { node, region_index };
                let port = node.as_outport_location(OutputType::EntryVariableArgument {
                    branch: region_index,
                    entry_variable: ev,
                });
                //Ignore unconneted branches, this happens a lot and generates useless indexing code..
                if self.optimizer.graph[port].edges.is_empty() {
                    continue;
                }
                self.itt_outport(branch_region, port);
            }
        }

        //Now do the same for the exit-variables
        for ex in 0..self.optimizer.graph[node]
            .node_type
            .unwrap_gamma_ref()
            .exit_var_count()
        {
            let ex_output = node.as_outport_location(OutputType::ExitVariableOutput(ex));
            if self.optimizer.graph[ex_output].edges.is_empty() {
                continue;
            }
            if !self
                .optimizer
                .get_out_type_mut(ex_output)
                .unwrap()
                .is_interval()
            {
                continue;
            }
            changed_any = true;

            //is interval-var, therfore hand the result of each branch, and then the output itself
            for region_index in 0..region_count {
                let branch_region = RegionLocation { node, region_index };
                let result = node.as_inport_location(InputType::ExitVariableResult {
                    branch: region_index,
                    exit_variable: ex,
                });
                self.itt_inport(branch_region, result);
            }
            self.itt_outport(parent_region, ex_output);
        }

        Ok(changed_any)
    }
}
