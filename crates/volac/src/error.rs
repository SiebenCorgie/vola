use thiserror::Error;

///Top level Vola errors that can happen at compile-time.
#[derive(Debug, Error)]
pub enum VolaErr {
    #[error("Error while loading the vola language specification: {0}")]
    LangError(#[from] tree_sitter::LanguageError),
}
