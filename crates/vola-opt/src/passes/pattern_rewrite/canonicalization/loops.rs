/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2026 Tendsin Mende
 */

use rvsdg_pattern_rewrite::{driver::RewriteableGraph, CodeSize, PatternRewrite};

use crate::{OptEdge, OptNode, Optimizer};

///Tries to unroll a theta-node, or panics if thats not possible.
pub struct UnrollOrPanic;
impl PatternRewrite<OptNode, OptEdge, Optimizer, CodeSize> for UnrollOrPanic {
    fn matches(&self, ctx: &Optimizer, node: rvsdg::NodeRef) -> bool {
        ctx.graph()[node].node_type.is_theta()
    }
    fn benefit(&self) -> &CodeSize {
        &CodeSize(1)
    }
    fn name(&self) -> &str {
        "UnrollOrPanic"
    }
    fn apply(&self, ctx: &mut Optimizer, node: rvsdg::NodeRef) {
        let unroll_count = ctx
            .loop_count(node)
            .expect("Could not find out the iteration count of the loop.");

        #[cfg(feature = "log")]
        log::info!("Unrolling Loop {node} for {unroll_count} iterations");

        ctx.graph
            .unroll_replace_theta(node, unroll_count)
            .expect("Failed to unroll loop");
    }
}
