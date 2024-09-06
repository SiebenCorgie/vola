use rvsdg::{
    err::GraphError,
    util::{dead_node_elimination::DneError, inline::InlineError, unroll::UnrollError},
};
/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use vola_common::{
    ariadne::Label,
    error::error_reporter,
    report,
    thiserror::{self, Error},
    Reportable,
};

use crate::{autodiff::AutoDiffError, common::Ty};

impl Reportable for OptError {}

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

impl OptError {
    pub fn report_no_concept(span: &vola_common::Span, concept_name: &str) -> Self {
        let err = OptError::Any {
            text: format!("Could not find concept \"{}\" in scope!", concept_name),
        };

        report(
            error_reporter(err.clone(), span.clone())
                .with_label(Label::new(span.clone()).with_message("Consider defining this concept"))
                .finish(),
        );

        err
    }

    pub fn report_argument_missmatch(
        def_span: &vola_common::Span,
        right_count: usize,
        wrong_span: &vola_common::Span,
        is_count: usize,
    ) -> Self {
        let err = OptError::Any {
            text: format!(
                "call takes {} arguments, but {} are supplied",
                right_count, is_count
            ),
        };

        report(
            error_reporter(err.clone(), wrong_span.clone())
                .with_label(
                    Label::new(def_span.clone())
                        .with_message(&format!("defined with {} arguments", right_count)),
                )
                .with_label(
                    Label::new(wrong_span.clone())
                        .with_message(&format!("This should have {} arguments", right_count)),
                )
                .finish(),
        );
        err
    }

    pub fn report_variable_not_found(def_span: &vola_common::Span, searched_for: &str) -> Self {
        let err = OptError::Any {
            text: format!("Could not find \"{}\" in scope!", searched_for),
        };
        report(
            error_reporter(err.clone(), def_span.clone())
                .with_label(Label::new(def_span.clone()).with_message("found here"))
                .finish(),
        );
        err
    }
}
