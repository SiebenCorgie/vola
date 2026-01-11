/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Implements all AD canonicalization.

use rvsdg::NodeRef;
use rvsdg_pattern_rewrite::{driver::Canonicalizer, CodeSize};

use crate::{
    autodiff::AutoDiff,
    passes::pattern_rewrite::canonicalization::{
        AproxAbs, AproxClamp, AproxMinMax, AproxMix, LowerLength, UnrollMul, UnrollOrPanic,
    },
    OptEdge, OptError, OptNode, Optimizer,
};

///Takes care of canonicalizing value into a form that can be
/// differentiated by the AutoDiff passes.
pub struct AdCanonicalizer<'opt> {
    opt: &'opt mut Optimizer,
    ///The actual pass applying the rewrite patterns. Always choses the canonicalization
    /// with the smallest code-size.
    canonicalizer_pass: Canonicalizer<OptNode, OptEdge, Optimizer, CodeSize>,
}

impl<'opt> AdCanonicalizer<'opt> {
    pub fn setup(opt: &'opt mut Optimizer) -> Self {
        #[cfg(feature = "log")]
        log::info!("Setup");

        //Setup the canonicalizer pass
        let mut canonicalizer_pass = Canonicalizer::default()
            .follow_calls(true)
            .follow_context(true);

        //Add all the AD related canonicalization pattern
        //we can't handle loops atm
        canonicalizer_pass.register(UnrollOrPanic);
        // we only handle _simple_ multiplication, therefor
        // unroll all others
        canonicalizer_pass.register(UnrollMul);
        canonicalizer_pass.register(LowerLength);

        //If we allow aproximation in order to AD undiff expressions
        if opt.config.autodiff.canonicalize_undiff {
            #[cfg(feature = "log")]
            log::info!("Activate aproximations");
            canonicalizer_pass.register(AproxAbs);
            canonicalizer_pass.register(AproxMinMax);
            canonicalizer_pass.register(AproxMix);
            canonicalizer_pass.register(AproxClamp);
        }

        AdCanonicalizer {
            opt,
            canonicalizer_pass,
        }
    }

    ///Tries to canonicalize the AD `entrypoint` into only nodes that are differentiatable.
    ///
    /// Returns on the first non-canonicalizable node.
    ///
    /// Handles dispatching into called functions, and sub regions of control-flow.
    pub fn canonicalize(mut self, entrypoint: NodeRef) -> Result<Self, OptError> {
        if !self.opt.is_node_type::<AutoDiff>(entrypoint) {
            return Err(OptError::Internal(
                "AD Entrypoint was not of type AutoDiff".to_string(),
            ));
        }

        //is an actual entrypoint, therfore canonicalize the source expression.
        let Some(expr_src) = self
            .opt
            .graph
            .inport_src(entrypoint.as_inport_location(AutoDiff::expr_input()))
        else {
            return Err(OptError::AutoDiffError(super::AutoDiffError::EmptyExprArg));
        };

        #[cfg(feature = "log")]
        log::info!("Start AD canonicalizing at {expr_src}");

        //Run the canon pass on that expr-src
        self.canonicalizer_pass
            .canonicalize_value(&mut self.opt, expr_src);

        Ok(self)
    }
}
