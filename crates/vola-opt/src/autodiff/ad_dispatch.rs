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
use rvsdg::SmallColl;
use vola_common::{ariadne::Label, error::error_reporter, report, Span};

use crate::{autodiff::AutoDiff, OptError, Optimizer};

impl Optimizer {
    pub fn dispatch_autodiff(&mut self) -> Result<(), OptError> {
        let mut dispatch_nodes = SmallColl::new();

        for noderef in self.graph.live_nodes(self.graph.toplevel_region()) {
            if self.graph.node(noderef).node_type.is_simple() {
                if self.is_node_type::<AutoDiff>(noderef) {
                    dispatch_nodes.push(noderef);
                }
            }
        }

        //pre-explore which regions we'll touch, and do dead-node elemination on those, since some
        //exploration depends on those
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
}
