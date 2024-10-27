/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use crate::{OptError, Optimizer};
use rvsdg::region::RegionLocation;

impl Optimizer {
    ///Inlines all apply nodes that are currently alive. Good if a backend doesn't implement call-def
    pub fn inline_all(&mut self) -> Result<(), OptError> {
        let topo_ord = self
            .graph
            .topological_order_region(self.graph.toplevel_region());
        for node in topo_ord {
            assert!(!self.graph[node].node_type.is_apply());
            if self.graph[node].regions().len() > 0 {
                for region_index in 0..self.graph[node].regions().len() {
                    self.inline_all_region(RegionLocation { node, region_index })?;
                }
            }
        }

        Ok(())
    }

    ///Inlines all apply nodes in this `region` recursively.
    pub fn inline_all_region(&mut self, region: RegionLocation) -> Result<(), OptError> {
        for node in self.graph.topological_order_region(region) {
            //Inline all sub regions
            if self.graph[node].regions().len() > 0 {
                for region_index in 0..self.graph[node].regions().len() {
                    self.inline_all_region(RegionLocation { node, region_index })?;
                }
            }

            if self.graph[node].node_type.is_apply() {
                //if its an applynode itself, inline the producer, then inline it here
                let apply_node_call_port = node.input(0);
                let prod = self.graph.find_producer_inp(apply_node_call_port).unwrap();
                assert!(self.graph.node(prod.node).node_type.is_lambda());
                //recursively inline anything in this producer λ
                self.inline_all_region(RegionLocation {
                    node: prod.node,
                    region_index: 0,
                })?;
                //ninline += 1;
                //now inline ourselfs
                let paths = self.graph.inline_apply_node(node).unwrap();
                for p in paths {
                    if let Err(e) = self.type_path(&p) {
                        log::trace!("Could not type inlined-path: {e}");
                    }
                }
            }
        }

        Ok(())
    }
}
