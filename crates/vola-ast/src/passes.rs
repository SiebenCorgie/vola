use std::path::Path;

use smallvec::{SmallVec, ToSmallVec};
use vola_common::FileString;

use crate::{common::Ident, error::AstError, AstEntry, TopLevelNode, VolaAst, VolaParser};

impl VolaAst {
    pub(crate) fn resolve_module(
        file: &dyn AsRef<Path>,
        stem_path: &[Ident],
        parser: &dyn VolaParser,
    ) -> Result<VolaAst, AstError> {
        let file_name: FileString = file.as_ref().to_str().unwrap().into();

        let bytes = std::fs::read(file.as_ref()).map_err(|e| AstError::IoError(e.to_string()))?;
        let mut parsed = parser.parse_from_byte(Some(file_name), &bytes)?;

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
