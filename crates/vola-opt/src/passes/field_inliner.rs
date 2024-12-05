/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use crate::{OptError, Optimizer};
use rvsdg::{region::RegionLocation, SmallColl};

impl Optimizer {
    ///Recursively inlines any apply-node thats _within_ a field_export.
    pub fn inline_field_exports(&mut self) -> Result<(), OptError> {
        let mut errs = SmallColl::new();
        for exp in self.exported_functions() {
            self.inline_region(exp)?;
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("VOLA_DUMP_POST_INLINE_EXPORT").is_ok()
        {
            self.push_debug_state("post inline fields");
        }

        if let Some(e) = errs.pop() {
            Err(e)
        } else {
            Ok(())
        }
    }

    //Recursive helper that explores all Apply nodes, and inlines their call-defs base on our [convention](Optimizer::should_inline_apply), before
    //inlining itself.
    fn inline_region(&mut self, region: RegionLocation) -> Result<(), OptError> {
        //recursively go through all sub regions (that are no λs, so gamma and theta for now)
        //and call the inliner in there as well
        for node in self.graph.live_nodes_in_region(region) {
            if node == region.node || !self.graph.region(&region).unwrap().nodes.contains(&node) {
                continue;
            }
            let (subregcount, is_valid) = {
                let node = self.graph.node(node);
                (
                    node.regions().len(),
                    node.node_type.is_gamma() || node.node_type.is_theta(),
                )
            };

            if is_valid && subregcount > 0 {
                for regidx in 0..subregcount {
                    self.inline_region(RegionLocation {
                        node,
                        region_index: regidx,
                    })?;
                }
            }
        }
        //NOTE: We only inline connected apply nodes, since we'd outherwise might touch _undefined_
        //      parts of the region.
        //
        // Also note, that we first explore all nodes with sub regions
        // so that any inlined-apply node in this region is already
        // inlined as far as-possible.
        for node in self.graph.live_nodes_in_region(region) {
            //If node is an apply node, inline its producer to this location
            if self.graph.node(node).node_type.is_apply() {
                //we shall not inline according to heuristic.
                if !self.should_inline_apply(node) {
                    continue;
                }

                let apply_node_call_port = node.input(0);
                let src = self.graph.inport_src(apply_node_call_port).unwrap();
                let prod = self.graph.find_callabel_def(src).unwrap();
                assert!(self.graph.node(prod.node).node_type.is_lambda());
                //recursively inline anything in this producer λ
                self.inline_region(RegionLocation {
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

        //for good measures, remove all unused CVs after importing _everything_
        //self.graph.remove_unused_context_variables(region.node);
        Ok(())
    }
}
