use std::path::Path;

use vola_common::FileString;

use crate::{error::AstError, VolaAst, VolaParser};

impl VolaAst {
    pub(crate) fn resolve_module(
        file: &dyn AsRef<Path>,
        parser: &dyn VolaParser,
    ) -> Result<VolaAst, AstError> {
        let file_name: FileString = file.as_ref().to_str().unwrap().into();

        let bytes = std::fs::read(file.as_ref()).map_err(|e| AstError::IoError(e.to_string()))?;
        parser.parse_from_byte(Some(file_name), &bytes)
    }
}
