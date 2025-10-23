/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use std::collections::VecDeque;

use ahash::AHashMap;
use rvsdg::{edge::OutportLocation, NodeRef};
use vola_common::VolaError;

use crate::{typelevel::ConstantIndex, OptError, Optimizer};

/// Pass that replace any interval-operation with _normal_ operations over tuple elements.
///
/// For instance it'll replace an operation `a + b` with `a`/`b` being intervals with `(a.start + b.start, a.end + b.end)`.
///
/// Those rules can become more involved for specific rules here. The main problem here is the monotonic property of an operation. If we know it
/// we can, for monoton increasing or decreasing function, trivially apply the transformation (see the `a+b` example).
/// For non monotonic functions however, we need to _know_ (or analyse) extrem points. We do that whenever possible, or fall back to conservative bounds. For instance if the input interval `x` in `sin(x)` is constant, we can produce an exact interval, otherwise we fall-back to `[-1..1]`
pub struct LowerIntervals<'opt> {
    optimizer: &'opt mut Optimizer,

    ///Maps a interval port to the already resolved lower and upper bound of some _primitive_ type
    mapping: AHashMap<OutportLocation, (OutportLocation, OutportLocation)>,
}

impl<'opt> LowerIntervals<'opt> {
    pub fn setup(optimizer: &'opt mut Optimizer) -> Self {
        LowerIntervals {
            optimizer,
            mapping: AHashMap::default(),
        }
    }

    pub fn execute(mut self) -> Result<(), VolaError<OptError>> {
        //Execution of this lowering simply iterates the live-code
        // in topological order, for any node that produces an interval, we call
        // the _handle_ routine.
        //
        // Two observations: Since we iterate the _before transformation_ state, we won't have
        // to consider _created_ nodes. Since we iterate it in topological order, we can assume that, for any node
        // that consumes some interval, an equivalent lowered representation already exists in `mapping`.
        let live_nodes = self
            .optimizer
            .graph
            .live_nodes(self.optimizer.graph.toplevel_region());
        let topoord = self
            .optimizer
            .graph
            .topological_order_nodes(live_nodes.into_iter());
        //TODO: Maybe we want to use the per-port liveness analysis here? There we could _not process_ dead
        //      outputs. But not sure if the additional granularity is really needed.
        for node in topoord {
            //if the node produces any interval edges, handle it
            for port in self.optimizer.graph.outports(node) {
                if self
                    .optimizer
                    .find_type(port)
                    .map_or(false, |t| t.is_interval())
                {
                    self.handle_value(port)?
                }
            }

            //For constant-index we have to check, whether it needs to be reduced to just
            // forwarding a flattened value
            if self.optimizer.is_node_type::<ConstantIndex>(node) {
                self.handle_index(node)?
            }
        }

        Ok(())
    }

    ///Replaces the interval of `value` with the tuple of `(start, end).
    ///
    /// Note that this traces the whole expression till no interval operations are found.
    fn handle_value(&mut self, value: OutportLocation) -> Result<(), VolaError<OptError>> {
        //If this port is already in the mapping, bail.
        if self.mapping.contains_key(&value) {
            return Ok(());
        }

        //Load the interval mappings for the dependencies of this node
        todo!();
        //Get the monotonity property, depending on that, spawn new nodes and hook-up the primitive
        // intervals
        todo!();
        //Now insert our outputs to the mapping and return.
        todo!()
    }

    ///Checks whether this indexing was flattened before and can just forward the actual value to its consumer.
    fn handle_index(&mut self, node: NodeRef) -> Result<(), VolaError<OptError>> {
        todo!()
    }
}
