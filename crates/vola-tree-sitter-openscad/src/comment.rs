use tree_sitter::Node;
use vola_ast::common::Comment;
use vola_common::{VolaError, ariadne::Label, error_reporter, report};

use crate::{ParserCtx, error::ParserError};

pub fn comment(ctx: &mut ParserCtx, data: &[u8], node: &Node) -> Comment {
    match node.utf8_text(data) {
        Ok(t) => Comment {
            span: ctx.span(&node),
            content: t.trim_start_matches("//").to_owned(),
        },
        Err(e) => {
            ctx.deep_errors.push(VolaError::error_here(
                ParserError::MalformedNode("Could not parse comment".to_owned()),
                ctx.span(&node),
                "here",
            ));
            Comment {
                span: ctx.span(&node),
                content: "Could not parse comment".to_owned(),
            }
        }
    }
}
