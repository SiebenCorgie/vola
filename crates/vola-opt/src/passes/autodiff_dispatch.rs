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

use std::collections::VecDeque;

use rvsdg::{NodeRef, region::RegionLocation};
use vola_common::{Span, VolaError};

use crate::{
    OptError, Optimizer,
    autodiff::{AutoDiff, ad_forward::ForwardAd},
};

///AutoDiff pass handles the canonicalization and differentiation of
/// auto-diff nodes.
pub struct AutoDiffPass<'opt> {
    opt: &'opt mut Optimizer,
    ///Collects currently waiting-to-be-dispatched autodiff nodes
    waiting_entry_nodes: VecDeque<NodeRef>,
}

impl<'opt> AutoDiffPass<'opt> {
    pub fn setup(opt: &'opt mut Optimizer) -> Self {
        AutoDiffPass {
            opt,
            waiting_entry_nodes: VecDeque::default(),
        }
    }

    ///Runs auto-differentiation on live auto-diff nodes in the graph
    pub fn autodiff_all(&mut self) -> Result<(), VolaError<OptError>> {
        self.waiting_entry_nodes.clear();
        //Regardless, always dead-node elimination before that pass, since most of the algorithms assume that anything that is connected is
        //also alive.
        self.opt
            .graph
            .dead_node_elimination()
            .map_err(|e| VolaError::new(e.into()))?;

        //First top-down exploration of AD nodes.
        self.enque_ad_nodes_region(self.opt.graph.toplevel_region());

        #[cfg(feature = "log")]
        log::info!("Dispatch AutoDiff for {}", self.waiting_entry_nodes.len());

        //NOTE: the collection is _in-topo-order_, which is why we have to pop from front.
        while let Some(node) = self.waiting_entry_nodes.pop_front() {
            self.autodiff_node(node)?;
        }
        Ok(())
    }

    ///Runs AutoDiff for `autodiff_node`. Does nothing, if the node is not a [AutoDiff] node.
    pub fn autodiff_node(&mut self, autodiff_node: NodeRef) -> Result<(), VolaError<OptError>> {
        //TODO: Do heuristics to decide if we want forward / backward or hybrid
        //      AD.
        //
        //      Potential glues are:
        //      - Expression size (might make sense to do forward for multiple wrt args, if expr is small)
        //      - Reocuring wrt-args (fold wrt-args if possible?)
        //      - simply the wrt-arg-count
        //      - expr-type-size

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_PRE_AD").is_ok() {
            self.opt
                .push_debug_state(&format!("pre-autodiff-{autodiff_node}"));
        }

        let span = self.opt.find_span(autodiff_node).unwrap_or(Span::empty());

        ForwardAd::setup(self.opt)
            .autodiff_entry(autodiff_node)
            .map_err(|e| e.with_label(span, "While forward autodiff this"))?;

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("DUMP_POST_AD").is_ok() {
            self.opt
                .push_debug_state(&format!("post-autodiff-{autodiff_node}"));
        }

        Ok(())
    }

    ///Recursive exploration of all sub region AD nodes and enques them in the waiting list
    fn enque_ad_nodes_region(&mut self, region: RegionLocation) {
        for node in self.opt.graph.topological_order_region(region) {
            let is_add = self.opt.is_node_type::<AutoDiff>(node);

            if !self.opt.graph[node].regions().is_empty() {
                for region in self.opt.graph.iter_regions(node) {
                    self.enque_ad_nodes_region(region);
                }
            }

            if is_add {
                self.waiting_entry_nodes.push_back(node);
            }
        }
    }
}
