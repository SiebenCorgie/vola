//! Type derivation pass.
//!
//! The pass is, who would have guessed, some form of λ-calculus.
//!
//! In practice we try to find a way from all _known_ types to a fully typed λ-node (not to be confused with the λ-calculus, yes naming is hard).
//! We do that by calling the [DialectNode](crate::DialectNode)'s `try_derive_type()` function.
//!
//! This lets each node-type implement its own derivation rule.
//!
//! The pass itself just tries to find a fix-point style resolution, which basically comes down to calling try-derive on all _just changed nodes_ till
//! either all are resolved, or nothing changes and we end in an _unresolved_ state.

use rvsdg::region::RegionLocation;

use crate::{error::OptError, Optimizer};

//NOTE: At the moment we rely on `eval` expressions being already tagged, as well as all inputs to an λ-Node being tagged as well.
// This basically lets us "push-down" all definitions. The only somewhat _hard_ nodes are the eval-nodes, since those will be replaced
// by call-sites at some point. However, since we knew the `concept` being used at that call site, we at-least know the return type, so we call walk
// over those.
//
impl Optimizer {
    ///Runs the type resolution pass on all nodes.
    pub fn type_derive(&mut self) -> Result<(), OptError> {
        //NOTE, we only need to run that on λ-Regions. There are 3 kinds for that
        // 1. ImplBlocks
        // 2. FieldDefs
        // 3. ExportFns
        //
        // Each of those has slightly different requirements.
        //
        // - Impl block _just_ uses the standard λ-resolution algorithm
        // - FieldDef: Uses the λ-resolution, then checks that the last result-connected node is a CSGTree node.
        // - ExportFn: Uses the λ-resolution, makes sure that all result connected nodes are one of the algebraic types.
        //
        //
        // Similar to the add_ast routine we _try_ all resolves, and collect errors before returning.

        let mut error_count = 0;
        let implblocks = self
            .concept_impl
            .values()
            .map(|v| v.lambda_region)
            .collect::<Vec<_>>();
        for implblock in implblocks {
            if let Err(_err) = self.derive_lambda(implblock) {
                error_count += 1;
            }
        }

        if error_count > 0 {
            Err(OptError::Any {
                text: format!("Type derivation did not end successfully!"),
            })
        } else {
            Ok(())
        }
    }

    fn verify_imblblock_output(&self, lambda: RegionLocation) -> Result<(), OptError> {
        Ok(())
    }

    fn derive_lambda(&mut self, lambda: RegionLocation) -> Result<(), OptError> {
        todo!()
    }
}
