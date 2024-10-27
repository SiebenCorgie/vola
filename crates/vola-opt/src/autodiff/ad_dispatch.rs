/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # AD-Dispatch pass
//!
//! Simple AD wrapper pass that takes care of finding, and dispatching `AutoDiff` nodes.
//!
//! TODO: At some point the dispatcher will also take care of the heuristics based on which either forward or backward
//!       mode AD are decided. But right now that is not implemented.

use ahash::AHashSet;
use rvsdg::{region::RegionLocation, NodeRef, SmallColl};
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{autodiff::AutoDiff, OptError, Optimizer};

impl Optimizer {
    pub fn dispatch_autodiff(&mut self) -> Result<(), OptError> {
        let mut dispatch_nodes = SmallColl::new();
        //Regardless, always dead-node elemination before that pass, since most of the algorithms assume that anything that is connected is
        //also alive.
        self.graph.dead_node_elimination()?;

        //First top-down exploration of AD nodes.
        self.enque_ad_nodes_region(self.graph.toplevel_region(), &mut dispatch_nodes);

        //Pre explore all touced regions.
        let touched_regions = dispatch_nodes
            .iter()
            .map(|n| self.graph[*n].parent.unwrap())
            .collect::<AHashSet<_>>();

        for node in dispatch_nodes {
            //TODO: Do heuristics to decide if we want forward / backward or hybrid
            //      AD.
            //
            //      Potential glues are:
            //      - Expression size (might make sense to do forward for multiple wrt args, if expr is small)
            //      - Reocuring wrt-args (fold wrt-args if possible?)
            //      - simply the wrt-arg-count
            //      - expr-type-size

            if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_PRE_AD").is_ok() {
                self.push_debug_state(&format!("pre-autodiff-{node}"));
            }

            let span = self.find_span(node.into()).unwrap_or(Span::empty());
            if let Err(e) = self.forward_ad(node) {
                report(
                    error_reporter(e.clone(), span.clone())
                        .with_label(
                            Label::new(span)
                                .with_message("While forward differentiating this node"),
                        )
                        .finish(),
                );

                return Err(e);
            }

            if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_POST_AD").is_ok() {
                self.push_debug_state(&format!("post-autodiff-{node}"));
            }
        }

        //before returning, re-derive types of all regions that we (might) have touched.
        //
        //This has two purposes:
        // 1. All following passes have access to the post-ad code's type info
        // 2. We'd catch any type-erros in the ad's pass

        for region in touched_regions {
            let span = self.find_span(region.into()).unwrap_or(Span::empty());
            if let Err(e) = self.derive_region(region, span.clone()) {
                report(
                    error_reporter(e.clone(), span)
                        .with_note("Failed to type derive region after auto-differentiation")
                        .finish(),
                );

                return Err(e);
            }
        }

        if std::env::var("VOLA_DUMP_ALL").is_ok()
            || std::env::var("DUMP_POST_AD_TYPE_DERIVE").is_ok()
        {
            self.push_debug_state(&format!("post-autodiff-type-derive"));
        }

        Ok(())
    }

    //Recursive exploration of all sub region AD nodes.
    fn enque_ad_nodes_region(&self, region: RegionLocation, list: &mut SmallColl<NodeRef>) {
        for node in self.graph.topological_order_region(region) {
            let is_add = if self.is_node_type::<AutoDiff>(node) {
                true
            } else {
                false
            };

            if self.graph[node].regions().len() > 0 {
                for region in 0..self.graph[node].regions().len() {
                    self.enque_ad_nodes_region(
                        RegionLocation {
                            node,
                            region_index: region,
                        },
                        list,
                    );
                }
            }

            if is_add {
                list.push(node);
            }
        }
    }
}
