use std::{error::Error, fmt::Debug};

use ariadne::{Label, Report, ReportBuilder};
use smallvec::{smallvec, SmallVec};

use crate::{report, Span};

pub fn error_reporter<'a>(err: impl ToString, span: Span) -> ReportBuilder<'a, Span> {
    let builder = Report::build(
        ariadne::ReportKind::Error,
        std::path::Path::new(span.file.as_str()),
        span.byte_start,
    )
    .with_config(ariadne::Config::default().with_index_type(ariadne::IndexType::Byte))
    .with_message(err);

    builder
}

pub fn warning_reporter<'a>(err: impl ToString, span: Span) -> ReportBuilder<'a, Span> {
    let builder = Report::build(
        ariadne::ReportKind::Warning,
        std::path::Path::new(span.file.as_str()),
        span.byte_start,
    )
    .with_config(ariadne::Config::default().with_index_type(ariadne::IndexType::Byte))
    .with_message(err);

    builder
}

///Common error type for Vola modules. Allows you to build a base error from any
/// type `E: Error`. Once build, the error can be augmented with additional context that will be
/// printed when using [VolaError::report].
///
/// It also allows you to covert any `VolaError<A>` to `VolaError<B>`, if `A` implements `Into<B>`.
/// You are encouraged to use [thiserror] to derive your `E` type, and use [VolaError] only to embedded your error.
pub struct VolaError<E: Error> {
    pub error: E,
    pub source_span: Option<Span>,
    ///All labels that might be attached to the error.
    pub labels: SmallVec<[Label<Span>; 4]>,
}

impl<E: Error> VolaError<E> {
    pub fn new(error: E) -> Self {
        VolaError {
            error,
            source_span: None,
            labels: SmallVec::new(),
        }
    }

    ///Creates an error that reports `message` at the given `span`.
    pub fn error_here(error: E, span: Span, message: impl ToString) -> Self {
        Self {
            error,
            source_span: Some(span.clone()),
            labels: smallvec![Label::new(span).with_message(message)],
        }
    }

    ///Pushes a simple _info_ label to the error
    pub fn with_label(mut self, span: Span, message: impl ToString) -> Self {
        self.labels.push(Label::new(span).with_message(message));
        self
    }

    ///Pushes a warning-label to the error
    pub fn with_warning(mut self, span: Span, message: impl ToString) -> Self {
        self.labels.push(
            Label::new(span)
                .with_message(message)
                .with_color(ariadne::Color::Yellow),
        );
        self
    }

    ///Marks the `span` as an additional error message
    pub fn with_error(mut self, span: Span, message: impl ToString) -> Self {
        self.labels.push(
            Label::new(span)
                .with_message(message)
                .with_color(ariadne::Color::Red),
        );
        self
    }

    ///Converts `self` into an `Err(VolaError<Error>)`, where `Err` can be converted from `E`.
    pub fn into_err<T, Err: From<E> + Error>(self) -> Result<T, VolaError<Err>> {
        Err(self.to_error::<Err>())
    }

    ///Converts `VolaError<E>` into `VolaError<Error>`, where `E` can be converted into `Err`.
    pub fn to_error<Err: From<E> + Error>(self) -> VolaError<Err> {
        VolaError {
            error: self.error.into(),
            source_span: self.source_span,
            labels: self.labels,
        }
    }

    ///Reports the full error
    pub fn report(&self) {
        let mut reporter = if let Some(source_span) = &self.source_span {
            Report::build(
                ariadne::ReportKind::Error,
                std::path::Path::new(source_span.file.as_str()),
                source_span.byte_start,
            )
            .with_config(ariadne::Config::default().with_index_type(ariadne::IndexType::Byte))
            .with_message(self.error.to_string())
        } else {
            Report::build(
                ariadne::ReportKind::Error,
                std::path::Path::new("unknown file"),
                0,
            )
            .with_config(ariadne::Config::default().with_index_type(ariadne::IndexType::Byte))
            .with_message(self.error.to_string())
        };

        reporter = reporter.with_labels(self.labels.clone());

        report(reporter.finish())
    }
}

impl<E: Error> Debug for VolaError<E> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(src_span) = &self.source_span {
            if let Some(file) = src_span.get_file() {
                write!(
                    f,
                    "{} [{}:{}..{}:{}]: {}",
                    file,
                    src_span.from.0,
                    src_span.from.1,
                    src_span.to.0,
                    src_span.to.1,
                    self.error
                )
            } else {
                write!(
                    f,
                    "[{}:{}..{}:{}]: {}",
                    src_span.from.0, src_span.from.1, src_span.to.0, src_span.to.1, self.error
                )
            }
        } else {
            write!(f, "{}", self.error)
        }
    }
}
