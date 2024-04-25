use crate::common::Ident;
use serde::{Deserialize, Serialize};
use smallvec::SmallVec;
use vola_common::Span;

///Sub module decleration.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module {
    ///The path from the modules root to the file.
    ///should work similar to the rust path, but in vola's case this
    ///is always _entry_file_local_, execpt for pathes that start with `std`.
    pub path: SmallVec<[Ident; 3]>,
    pub span: Span,
}

impl Module {
    pub fn is_stdlib(&self) -> bool {
        if self.path.len() > 0 {
            &self.path[0].0 == "std"
        } else {
            false
        }
    }
}
