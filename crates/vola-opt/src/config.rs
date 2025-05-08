/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Config options for the optimizer

#[derive(Debug, Clone)]
pub struct ConfigAutoDiff {
    ///If true, transforms non-differentiatable nodes into differentiatabel patterns.
    pub canonicalize_undiff: bool,

    ///If true, aborts AutoDiff if a non-differentiatable pattern or node is encountered (in a mathematical sense).
    ///
    /// Otherwise AutoDiff will try _to make it work_ by applying transformations that _mostly_ keep semantics, but might change edge cases.
    pub abort_on_undiff: bool,

    ///Configures `c` for the smooth, differentiatable approximation for abs: `f(x) -> sqrt(f(x)^2 + c)`.
    pub smooth_abs_c: f64,
}

impl Default for ConfigAutoDiff {
    fn default() -> Self {
        ConfigAutoDiff {
            canonicalize_undiff: true,
            abort_on_undiff: false,
            smooth_abs_c: 0.000001,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Config {
    pub autodiff: ConfigAutoDiff,
    //True if the compiler should dump its state on error
    pub dump_on_error: bool,
}
