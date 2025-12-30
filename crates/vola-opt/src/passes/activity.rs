/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//!# Activity analysis
//!
//! Produces a lookup-table of _active_ nodes (and ports) with regard to an `wrt` element and a `value`.
//!
//! A node or port is connected _active_ if it is
//!
//! 1. a predecessor to `value`
//! 2. `wrt`, or any successor of `wrt` is part of `value`
//!
//! The analysis handles theta and gamma nodes as well as function calls (with intra-λ inspection).
//!
//! It also handles indexing. I.e. If `wrt` is a value `a.x.y` only this value (and its parents `a.x` / `a`), will be considered _active_, not `a.x.x` or `a.y` etc.
//!
//! This analysis is helpful whenever a pass needs to modify nodes only if they are _active_ (i.e. changed) by a single value. Examples are the `wrt` value of a differentiation, or the input interval when doing interval-extension on functions.

pub use crate::autodiff::activity::Activity;
use rvsdg::edge::{InportLocation, OutportLocation};
use vola_common::VolaError;

use crate::{OptError, Optimizer};

pub struct ActivityAnalysis<'opt> {
    optimizer: &'opt Optimizer,
}

impl Optimizer {
    ///Returns the activity of all predecessors of `value` with respect to `wrt`. See the [activity module](super::activity) for more information.
    pub fn activity_analysis(
        &mut self,
        value: InportLocation,
        wrt: InportLocation,
    ) -> Result<Activity, VolaError<OptError>> {
        let analysis = ActivityAnalysis::setup(self);

        analysis.execute(value, wrt)
    }
}

impl<'opt> ActivityAnalysis<'opt> {
    pub fn setup(optimizer: &'opt Optimizer) -> Self {
        //TODO: Here we could setup all the intermediate data and stuff for performance reasons.
        Self { optimizer }
    }

    ///Returns the activity of all predecessors of `value` with respect to `wrt`. See the [activity module](super::activity) for more information.
    pub fn execute(
        self,
        value: InportLocation,
        wrt: InportLocation,
    ) -> Result<Activity, VolaError<OptError>> {
        // This is a three-stage process.
        // 1. Find the actual value producer that is _active_. The dialects permit none-transforming nodes like _index_

        let region = self.optimizer.graph[value.node].parent.unwrap();
        let wrt_src = self.optimizer.graph.inport_src(wrt).unwrap();

        let expr_src = self.optimizer.graph.inport_src(value).unwrap();

        let (activity_seed_nodes, wrt_producer) = self.optimizer.trace_activity_seed(wrt_src);

        //NOTE: right now we just build the activity for all nodes that are predecessors to the expression
        //
        //TODO: We _could_ also build that thing lazyly. At least the Activity _thingy_ would permit that. But
        //      In that case we'd have to update all passes that _assume_ all active nodes set to querry instead.

        let mut predecerssors = self
            .optimizer
            .graph
            .walk_predecessors_in_region(expr_src.node)
            .collect::<Vec<_>>();
        //Push ourselfs into that node as well
        predecerssors.push(expr_src);

        let mut activity = Activity {
            active: activity_seed_nodes,
            wrt_producer,
        };

        //Set all argument ports to this region, that are not already active as inactive. This is needed
        //later on in the `Activity::is_active_port()` to handle inactive-arguments correctly
        for argument in self.optimizer.graph[region.node].argument_types(region.region_index) {
            let port_loc = OutportLocation {
                node: region.node,
                output: argument,
            };
            if activity.active.get(&port_loc.into()).is_none() {
                activity.active.set(port_loc.into(), false);
            }
        }

        //Now just querry the _activity_ state once for each output, which lets the helper
        //build that state.
        for pred in self
            .optimizer
            .graph
            .walk_predecessors_in_region(expr_src.node)
            .chain([expr_src].into_iter())
        {
            let _val = activity.is_active_port(self.optimizer, pred);
        }

        Ok(activity)
    }
}
