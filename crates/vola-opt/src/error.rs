/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use crate::{
    //autodiff::AutoDiffError,
    autodiff::AutoDiffError,
    common::Ty,
};
use rvsdg::{
    err::GraphError,
    util::{dead_node_elimination::DneError, inline::InlineError, unroll::UnrollError},
};
use vola_common::thiserror::{self, Error};

///Runtime optimizer errors. Note that at this point errors are pretty specific and mostly can't be recovered from.
/// So we opt to use generic descriptions, instead of specific errors.
#[derive(Debug, Error, Clone)]
pub enum OptError {
    #[error("{text}")]
    Any { text: String },

    #[error("Failed to dispatch field for {opname} and concept {concept}: {errstring}!")]
    DispatchAnyError {
        concept: String,
        opname: String,
        errstring: String,
    },

    #[error("CSG-Tree structure issue: {0}")]
    CsgStructureIssue(String),

    #[error("Internal compiler error: {0}\nPlease file an issue!")]
    Internal(String),

    #[error("Cannot convert AstType {srcty:?} to a valid optimizer type")]
    TypeConversionError { srcty: vola_ast::common::Ty },

    #[error("At least {0} errors occurred while running optimizer.")]
    ErrorsOccurred(usize),

    #[error("Type derivation failed in λ-Node")]
    TypeDeriveFailed { errorcount: usize },

    #[error("Type derive failed: {text}")]
    TypeDeriveError { text: String },

    #[error("Type resolution error, set to {set}, but was derived to {derive}")]
    TypeResolutionErrorSet { set: Ty, derive: Ty },
    #[error("Type resolution error, derived once to {a}, now is {b}")]
    TypeResolutionErrorDerive { a: Ty, b: Ty },

    #[error("Failed to inline call: {error}")]
    InlineFailed { error: InlineError },

    #[error("Dead node elimination failed: {0}")]
    DneFailed(#[from] DneError),

    #[error("Internal RVSDG graph error: {0}")]
    InternalGraphError(#[from] GraphError),

    #[error("Could not find anything that was typeset on the path")]
    NotTypeOnPath,

    #[error("Failed to generate identitiy-implementation: {0}")]
    AIIFailed(String),

    #[error(transparent)]
    AutoDiffError(#[from] AutoDiffError),
    #[error(transparent)]
    UnrollError(#[from] UnrollError),
}
