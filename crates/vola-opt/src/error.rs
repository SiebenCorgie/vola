use vola_common::{
    miette::{self, Diagnostic, SourceSpan},
    report,
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

    #[error("{err}")]
    PostSpanned {
        err: Box<OptError>,
        #[label("here")]
        span: SourceSpan,
    },

    #[error("Cannot convert AstType {srcty:?} to a valid optimizer type")]
    TypeConversionError { srcty: vola_ast::common::Ty },

    #[error("At least {0} errors occurred while running optimizer.")]
    ErrorsOccurred(usize),

    #[error("Type derivation failed in λ-Node")]
    TypeDeriveFailed {
        errorcount: usize,
        #[label("failed with {errorcount} errors in this region")]
        span: SourceSpan,
    },
}

impl OptError {
    ///Converts `self` into an spanned error, if it isn't already.
    pub fn into_spanned(self, span: &vola_common::Span) -> Self {
        match self {
            OptError::AnySpanned { .. }
            | OptError::AnySpannedWithSource { .. }
            | OptError::TypeDeriveFailed { .. } => self,
            e => OptError::PostSpanned {
                err: Box::new(e),
                span: span.clone().into(),
            },
        }
    }

    pub fn report_no_concept(span: &vola_common::Span, concept_name: &str) -> Self {
        let err = OptError::AnySpanned {
            span: span.clone().into(),
            text: format!("Could not find concept \"{}\" in scope!", concept_name),
            span_text: "Consider defining this concept".to_owned(),
        };
        report(err.clone(), span.get_file());
        err
    }

    pub fn report_argument_missmatch(
        def_span: &vola_common::Span,
        right_count: usize,
        wrong_span: &vola_common::Span,
        is_count: usize,
    ) -> Self {
        let err = OptError::AnySpannedWithSource {
            source_span: def_span.clone().into(),
            source_text: format!("defined with {} arguments", right_count),
            text: format!(
                "call takes {} arguments, but {} are supplied",
                right_count, is_count
            ),
            span: wrong_span.clone().into(),
            span_text: format!("This should have {} arguments.", right_count),
        };

        report(err.clone(), def_span.get_file());
        err
    }

    pub fn report_variable_not_found(def_span: &vola_common::Span, searched_for: &str) -> Self {
        let err = OptError::AnySpanned {
            span: def_span.clone().into(),
            text: format!("Could not find \"{}\" in scope!", searched_for),
            span_text: format!("found here"),
        };
        report(err.clone(), def_span.get_file());
        err
    }
}
