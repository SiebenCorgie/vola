/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! General purpose pattern rewrites and passes.

use rvsdg::region::RegionLocation;
use rvsdg_pattern_rewrite::{driver::RewriteableGraph, Speed, TopoGreedyRewriter};

use crate::{OptEdge, OptNode, Optimizer};

pub mod arith;
pub mod branch;
pub mod canonicalization;

impl RewriteableGraph for Optimizer {
    type Edge = OptEdge;
    type Node = OptNode;
    fn graph(&self) -> &rvsdg::Rvsdg<Self::Node, Self::Edge> {
        &self.graph
    }
    fn graph_mut(&mut self) -> &mut rvsdg::Rvsdg<Self::Node, Self::Edge> {
        &mut self.graph
    }
}

impl Optimizer {
    ///Standard number of restarts the gp-rewrite will try for each region.
    const RESTARTS: usize = 8;
    ///Applies a bespoke set of rewrite patterns to the graph which aims to strike a good balance of codesize
    /// and performance (for GPUs).
    pub fn pattern_rewrite_all(&mut self) {
        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_PRE_REWRITE").is_ok() {
            self.push_debug_state("pre apply rewrite");
        }

        self.rewrite_region(self.graph.toplevel_region());

        if std::env::var("VOLA_DUMP_ALL").is_ok() || std::env::var("VOLA_POST_REWRITE").is_ok() {
            self.push_debug_state("post apply rewrite");
        }
    }

    fn add_fold_pattern(
        mut rewriter: TopoGreedyRewriter<OptNode, OptEdge, Optimizer, Speed>,
    ) -> TopoGreedyRewriter<OptNode, OptEdge, Optimizer, Speed> {
        rewriter.register(arith::FoldBinarySimple);
        rewriter.register(arith::FoldMuliplication);
        rewriter
    }

    fn add_cf_pattern(
        mut rewriter: TopoGreedyRewriter<OptNode, OptEdge, Optimizer, Speed>,
    ) -> TopoGreedyRewriter<OptNode, OptEdge, Optimizer, Speed> {
        rewriter.register(branch::SpecializeStaticBranch);

        rewriter
    }

    ///Applies the bespoke set of patterns (see [pattern_rewrite_all](Self::pattern_rewrite_all)) to `region` and any contained sub-regions.
    pub fn rewrite_region(&mut self, region: RegionLocation) {
        let mut rewriter = rvsdg_pattern_rewrite::TopoGreedyRewriter::default()
            .with_recursion(rvsdg_pattern_rewrite::DriverRecursion::TopDown)
            .with_restart(rvsdg_pattern_rewrite::driver::DriverRestart::Bound(
                Self::RESTARTS,
            ));

        rewriter = Self::add_cf_pattern(rewriter);
        rewriter = Self::add_fold_pattern(rewriter);

        rewriter.run(self, region);
    }
}
