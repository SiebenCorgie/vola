use std::path::Path;

use crate::{error::AstError, VolaAst};

impl VolaAst {
    pub(crate) fn resolve_module(file: &dyn AsRef<Path>) -> Result<VolaAst, AstError> {
        match crate::parse_file(file) {
            Ok(ast) => Ok(ast),
            Err(e) => Err(AstError::ParsingError {
                path: file.as_ref().to_path_buf(),
                err: e
                    .1
                    .get(0)
                    .cloned()
                    .unwrap_or(crate::ParserError::UnknownError(
                        "Unknown module parser error".to_owned(),
                    )),
            }),
        }
    }
}
