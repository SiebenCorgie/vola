use ariadne::{Report, ReportBuilder};

use crate::{Reportable, Span};

pub fn error_reporter<'a>(err: impl Reportable, span: Span) -> ReportBuilder<'a, Span> {
    let builder = Report::build(
        ariadne::ReportKind::Error,
        std::path::Path::new(span.file.as_str()),
        span.byte_start,
    )
    .with_config(ariadne::Config::default().with_index_type(ariadne::IndexType::Byte))
    .with_message(err);

    builder
}
