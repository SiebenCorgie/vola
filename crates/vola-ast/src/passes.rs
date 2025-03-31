use std::{error::Error, path::Path};

use smallvec::SmallVec;
use vola_common::{FileString, VolaError};

use crate::{common::Ident, error::AstError, AstEntry, VolaAst, VolaParser};

impl VolaAst {
    pub(crate) fn resolve_module<E: Error>(
        file: &dyn AsRef<Path>,
        stem_path: &[Ident],
        parser: &dyn VolaParser<Error = E>,
    ) -> Result<VolaAst, Vec<VolaError<AstError>>> {
        let file_name: FileString = file.as_ref().to_str().unwrap().into();

        let bytes = std::fs::read(file.as_ref())
            .map_err(|e| vec![VolaError::new(AstError::IoError(e.to_string()))])?;
        let mut parsed = parser
            .parse_from_byte(Some(file_name), &bytes)
            .map_err(|errs| {
                errs.into_iter()
                    .map(|e| AstError::from_parser_error(e))
                    .collect::<Vec<_>>()
            })?;

        //prepend the stem_path to all submodules that are imported
        for tl in parsed.entries.iter_mut() {
            if let AstEntry::Module(m) = &mut tl.entry {
                let mut lpath: SmallVec<_> = stem_path.iter().cloned().collect();
                lpath.append(&mut m.path);
                m.path = lpath;
            }
        }

        Ok(parsed)
    }
}
