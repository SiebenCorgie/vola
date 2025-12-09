/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use std::collections::VecDeque;

use rvsdg::edge::OutportLocation;

use vola_common::{Span, VolaError};

use crate::{
    common::Ty,
    passes::lazy_type::{ResolveState, TypeError},
    OptEdge, Optimizer,
};

impl Optimizer {
    ///Immutable, iterative type discorvery on graph
    pub(super) fn type_discover(
        &self,
        outport: OutportLocation,
    ) -> Result<Ty, VolaError<TypeError>> {
        //Now, generally we have two possibilities:
        // 1. The edge of this port is allready set, in that case, just return the type of the edge.
        //    This path get away without any allocation and should be the _default_ case if the
        //    graph is reasonably often typed out.
        // 2. The edge is not-yet type. In that case we initialise the back-tracking algorithm. It'll back-track the node's
        //    ancestors till it finds type-information on any path. If that happens, it'll rebuild the type information of
        //    stack till we arrive back at our chosen value.

        let src_span = self.find_span(outport).unwrap_or(Span::empty());

        #[cfg(feature = "log")]
        log::trace!("TypeDeriveImmut[{outport}]");

        //So lets try the unified edge first. This _should_ be the common path
        if let Ok(opt_edge) = self.unified_outport_type(outport) {
            match opt_edge {
                OptEdge::State => {
                    return Err(VolaError::error_here(
                        TypeError::IsState(outport.into()),
                        src_span,
                        "here",
                    ))
                }
                OptEdge::Value { ty } => {
                    if let Some(t) = ty.get_type() {
                        return Ok(t);
                    }
                }
            }
        }

        //Sooo... accessing the cache did not work lets init the exploration queue
        let (mut lookat_queue, mut local_type_lookup) =
            self.type_exploration(src_span.clone(), outport)?;

        //Done with exploring, we can now assume that the last elements in the queue are either type-set ports,
        //or ports, where we can access type-information.
        //Reverse iterate the queue and record the type-information for each port

        //At this point we have a reverse queue of _things_ we still need to explore, as well as a
        // lookup table that is initialized for any leaf-node
        lookat_queue.reverse();
        let mut explore_queue = VecDeque::from(lookat_queue);

        while let Some(port) = explore_queue.pop_front() {
            //try to lookup the port's type. If that possible, just don't do anyhing.
            // If its not possible, lookup all input's types, and call the node's local
            // type-derive. this _should_ succeed.
            if let Some(Some(_t)) = local_type_lookup.get(&port) {
                //already resolved.
                continue;
            }

            //Not yet resolved, therefore do the resolving now.
            match self
                .try_local_port_resolve(port, &local_type_lookup)
                .map_err(|e| {
                    let span = self.find_span(port.node).unwrap_or(src_span.clone());
                    VolaError::error_here(e, span, "Type derive stuck while calculating this type")
                })? {
                ResolveState::ResolvedTo(ty) => {
                    let old = local_type_lookup.insert(port, Some(ty));
                    //make sure it wasn't set before, and set it
                    assert_eq!(old, Some(None))
                }
                ResolveState::WaitingFor(this) => {
                    #[cfg(feature = "log")]
                    log::trace!(
                        "TypeDerive[{outport}]: Defer {port} type derive, waiting for {this}"
                    );

                    let Some(index) = explore_queue
                        .iter()
                        .enumerate()
                        .find_map(|(idx, element)| if element == &this { Some(idx) } else { None })
                    else {
                        panic!("{port} waits for {this}, but its not part of the queue anymore...");
                    };
                    //Reinsert the port after the wait-for port was seen.
                    explore_queue.insert(index + 1, port);
                }
            }
        }

        //finally we should be able to just read-out the type!
        local_type_lookup
            .get(&outport)
            .expect("Initial port should be type-set")
            .clone()
            .ok_or(VolaError::error_here(
                TypeError::Stuck(outport.into()),
                src_span,
                "could not deduce final type",
            ))
    }
}
