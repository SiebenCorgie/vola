use vola_common::{ErrorReporter, CommonError, Span};

use crate::{AstEntry, error::ParserError};

use super::FromTreeSitter;




impl FromTreeSitter for AstEntry{
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized {

        match node.kind(){

            _ => {
                let err = ParserError::UnknownAstNode(node.kind().to_owned());
                reporter.push_error(CommonError::new(Span::from(node), err));
                Err(ParserError::UnknownAstNode(node.kind().to_owned()))
            }
        }
    }
}
