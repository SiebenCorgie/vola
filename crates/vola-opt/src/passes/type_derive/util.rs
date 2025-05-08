/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

use rvsdg::{edge::OutportLocation, NodeRef};
use vola_common::Span;

use crate::{common::Ty, OptError, Optimizer};

impl Optimizer {
    //Tries to get the type of the port, If that is not possible, tries to derive a type. Panics if that is not possible, since the graph
    //should always be able to derive a type for any output.
    pub(crate) fn get_or_derive_type(
        &mut self,
        output: OutportLocation,
        ignore_dead_nodes: bool,
    ) -> Ty {
        if let Some(t) = self.find_type(&output.into()) {
            return t;
        }

        match self.type_derive_and_propagate(output.node, ignore_dead_nodes) {
            Ok(_) => {}
            Err(e) => {
                if self.config.dump_on_error {
                    self.push_debug_state(&format!("FailedTypeDerive[{}]", output));
                    self.dump_debug_state("failed_type_derive.bin");
                }
                panic!("Could not type-derive, this is probably a bug in the Pipeline: {e}");
            }
        }
        match self.find_type(&output.into()) {
            Some(t) => t,
            None => {
                if self.config.dump_on_error {
                    self.push_debug_state(&format!("{} could not derive type", output));
                    self.dump_debug_state("failed_type_derive.bin");
                }
                panic!("Could not generate type");
            }
        }
    }

    ///Handles type derive and propagation of a node that is added while canonicalizing.
    pub(crate) fn type_derive_and_propagate(
        &mut self,
        node: NodeRef,
        ignore_dead_nodes: bool,
    ) -> Result<(), OptError> {
        //We first try to just do a per-node derivation. Most of our canonicalizations just replace with _one_ level
        //of nodes.
        //However, this'll fail, if the canonicalization adds _many_ nodes.
        //in that case we'll start a full pass.
        if let Ok(typed_ports) = self.try_node_type_derive(node, ignore_dead_nodes) {
            for (ty, outport) in typed_ports {
                self.typemap.set(outport.into(), ty.clone());
                //push type on port into edges
                for edg in self.graph[outport].edges.clone() {
                    self.graph[edg].ty.set_type(ty.clone());
                }
            }

            return Ok(());
        }
        //simple node derive failed, try to do type derive in region
        let parent_region = self.graph[node].parent.unwrap();
        let span = self
            .find_span(parent_region.into())
            .unwrap_or(Span::empty());
        let region_result = self.derive_region(parent_region, span, ignore_dead_nodes);

        if region_result.is_ok() {
            return Ok(());
        }

        //iff that fails too, we have to do a full type-derive pass.
        //We ignore if that fails, because
        //this helper is possible working on an
        // unfinished graph
        let _ = self.type_derive(false);
        Ok(())
    }
}
