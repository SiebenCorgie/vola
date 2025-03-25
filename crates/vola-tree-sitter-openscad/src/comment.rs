use tree_sitter::Node;
use vola_ast::common::Comment;
use vola_common::{ariadne::Label, error::error_reporter, report};

use crate::ParserCtx;

pub fn comment(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Comment {
    match node.utf8_text(data) {
        Ok(t) => Comment {
            span: ctx.span(&node),
            content: t.trim_start_matches("//").to_owned(),
        },
        Err(e) => {
            ctx.deep_errors.push(e.into());
            report(
                error_reporter("Could not parse comment", ctx.span(&node))
                    .with_label(Label::new(ctx.span(&node)).with_message("Here"))
                    .finish(),
            );
            Comment {
                span: ctx.span(&node),
                content: "Could not parse comment".to_owned(),
            }
        }
    }
}
