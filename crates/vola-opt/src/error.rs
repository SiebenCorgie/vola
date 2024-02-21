use vola_common::{
    miette::{self, Diagnostic, SourceSpan},
    thiserror::{self, Error},
    Reportable,
};

impl Reportable for OptError {}

///Runtime optimizer errors. Note that at this point errors are pretty specific and mostly can't be recovered from.
/// So we opt to use generic descriptions, instead of specific errors.
#[derive(Debug, Error, Clone, Diagnostic)]
pub enum OptError {
    #[error("{text}")]
    Any { text: String },
    #[error("{text}")]
    AnySpanned {
        #[label("{span_text}")]
        span: SourceSpan,
        text: String,
        span_text: String,
    },
    #[error("{text}")]
    AnySpannedWithSource {
        ///Label that is tagged at the source's location. Must reside in the same file as `span`.
        #[label("{source_text}")]
        source_span: SourceSpan,
        source_text: String,
        ///Error description
        text: String,
        #[label("{span_text}")]
        span: SourceSpan,
        span_text: String,
    },
}
