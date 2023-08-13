//! # Vola Compiler
//!
//! The Vola MLIR compiler. Can use tree-sitter-vola to parse a vola file,
//! or just read some Vola-AST and compile that (to SPIR-V).

mod error;
pub use error::VolaErr;
mod parser;
pub use parser::parser;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
