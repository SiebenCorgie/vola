use std::path::Path;

use tree_sitter::{Parser, TreeCursor};
use vola_common::{CommonError, ErrorReporter, FileString, Span};

use crate::{common::CTArg, error::ParserError, AstEntry, TopLevelNode, VolaAst};

mod alge;
mod common;
mod csg;
mod toplevel;

pub trait FromTreeSitter {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized;
}

///Parses `file`. Returns the [VolaAst](crate::VolaAst) on success, or a partially parsed AST, and the reported errors, if any
/// parsing errors happened.
pub fn parse_file(
    file: impl AsRef<Path>,
) -> Result<VolaAst, (VolaAst, ErrorReporter<ParserError>)> {
    let dta = match std::fs::read(file.as_ref()) {
        Ok(dta) => dta,
        Err(e) => {
            let mut reporter = ErrorReporter::new();
            reporter.push_error(CommonError::new_spanless(ParserError::FSError(
                e.to_string(),
            )));
            return Err((
                VolaAst {
                    entries: Vec::with_capacity(0),
                },
                reporter,
            ));
        }
    };
    let file_src_str = file.as_ref().to_str().unwrap_or("NonUnicodeFilename");
    parse_data(&dta, Some(file_src_str))
}

///Parses `string`. Returns the [VolaAst](crate::VolaAst) on success, or a partially parsed AST, and the reported errors, if any
/// parsing errors happened.
pub fn parse_string(string: String) -> Result<VolaAst, (VolaAst, ErrorReporter<ParserError>)> {
    let as_bytes = string.as_bytes();
    parse_data(as_bytes, None)
}

pub fn parse_from_bytes(bytes: &[u8]) -> Result<VolaAst, (VolaAst, ErrorReporter<ParserError>)> {
    parse_data(bytes, None)
}

//load the vola-lang tree-sitter grammar / parser
fn parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_vola::language())
        .expect("Failed to load treesitter minisdf");
    parser
}

///Internal parser implementation
fn parse_data(
    data: &[u8],
    src_file: Option<&str>,
) -> Result<VolaAst, (VolaAst, ErrorReporter<ParserError>)> {
    let mut reporter = ErrorReporter::new();
    if let Some(src) = src_file {
        reporter.set_default_file(&src);
    }

    let mut ast = VolaAst {
        entries: Vec::new(),
    };

    //recursively parse all nodes
    let mut parser = parser();
    let syn_tree = match parser.parse(&data, None) {
        None => {
            reporter.push_error(CommonError::new_spanless(ParserError::TreeSitterFailed));
            return Err((ast, reporter));
        }
        Some(syntree) => syntree,
    };

    let mut cursor = syn_tree.root_node().walk();
    //Collects args until a non-comment, non-ct-arg node is returned. Attaches all preceeding ctargs
    // to that toplevel node.
    let mut ct_args = Vec::with_capacity(0);
    for node in syn_tree.root_node().children(&mut cursor) {
        match node.kind() {
            "comment" => continue,
            "ct_attrib" => {
                if let Ok(ctattrib) = CTArg::parse(&mut reporter, data, &node) {
                    ct_args.push(ctattrib);
                }
            }
            _ => {
                if let Ok(tlnode) = AstEntry::parse(&mut reporter, data, &node) {
                    let mut nodes_attribs = Vec::with_capacity(0);
                    //Swapout all collected nodes and start with an (usally) empty vec again.
                    std::mem::swap(&mut ct_args, &mut nodes_attribs);
                    let entry_node = TopLevelNode {
                        span: Span::from(&node),
                        ct_args: nodes_attribs,
                        entry: tlnode,
                    };
                    ast.entries.push(entry_node);
                }
            }
        }
    }

    if reporter.has_errors() {
        Err((ast, reporter))
    } else {
        Ok(ast)
    }
}
