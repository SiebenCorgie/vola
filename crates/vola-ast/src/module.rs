use std::{fmt::Display, path::PathBuf, process::id};

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
    ///is always _entry_file_local_, except for paths that start with `std`.
    pub path: SmallVec<[Ident; 3]>,
    pub span: Span,
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let len = self.path.len();
        for (index, element) in self.path.iter().enumerate() {
            write!(f, "{}", element.0)?;
            if index < (len - 1) {
                write!(f, "::")?;
            }
        }
        Ok(())
    }
}

impl Module {
    pub fn is_stdlib(&self) -> bool {
        if !self.path.is_empty() {
            &self.path[0].0 == "std"
        } else {
            false
        }
    }

    ///Returns true, if this is a _local_ path, i.e. begins with _self_
    pub fn is_self(&self) -> bool {
        self.origin() == "self"
    }

    ///Returns the name of the path's origin, i.e. the first element
    pub fn origin(&self) -> &str {
        &self
            .path
            .get(0)
            .expect("Path was empty, that should not be possible!")
            .0
    }

    ///Substitudes the _origin_ element with `directory`, and builds the full path
    pub fn make_directory_local(&self, mut local_to: PathBuf) -> PathBuf {
        //NOTE: ignoring the origin
        for ident in &self.path[1..] {
            //NOTE: We switch out any part of the path
            // "super" as ".." which is exactly what super means in unix-path notation.
            let path_part = if ident.0 == "super" { ".." } else { &ident.0 };

            local_to.push(path_part);
        }

        local_to
    }

    ///Switches the _self_ stem of `self` with the path of `new_root_module`.
    ///
    /// # Example
    ///
    /// Lets say a file at my_project::a::b is has this line:
    /// `module self::c::d::e;`
    ///
    ///
    /// This will be called with `my_project::a::b` and turn `self`
    /// into `my_project::a::b::c::d::e`
    pub fn switch_root_module(&mut self, new_root_module: &Module) -> Self {
        assert_eq!(
            self.path[0].0, "self",
            "we can only switch roots on self-paths"
        );
        let mut stem = new_root_module.clone();
        //remove the _self_ part of the stem, i.e in the example above
        // the `e` part of `sel::c::d::e`.

        assert!(stem.path.len() > 1);

        stem.path.remove(stem.path.len() - 1);

        stem.span = self.span.clone();
        stem.path.extend(self.path[1..].iter().cloned());

        stem
    }
}
