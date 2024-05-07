/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::path::Path;

use error::ParserError;
use tree_sitter::{Node, Parser};
use vola_common::{error::error_reporter, report, FileString, Span};

use vola_ast::{common::CTArg, AstEntry, TopLevelNode, VolaAst};
pub mod alge;
pub mod common;
pub mod csg;
pub mod error;
pub mod toplevel;

///Context on the parser, like the current src file, and errors that occured, but are ignored.
pub struct ParserCtx {
    deep_errors: Vec<ParserError>,
    src_file: FileString,
}

impl ParserCtx {
    pub fn new(file: FileString) -> ParserCtx {
        ParserCtx {
            deep_errors: Vec::new(),
            src_file: file,
        }
    }
    pub fn new_fileless() -> ParserCtx {
        ParserCtx {
            deep_errors: Vec::new(),
            src_file: FileString::default(),
        }
    }
    ///Creates a new span for `node` on this context.
    pub fn span(&self, node: &Node) -> Span {
        Span::from(node).with_file(&self.src_file)
    }

    pub fn get_file(&self) -> Option<&str> {
        if self.src_file.is_empty() {
            None
        } else {
            Some(self.src_file.as_str())
        }
    }
}

pub trait FromTreeSitter {
    fn parse(
        ctx: &mut ParserCtx,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized;
}

///Parses `file`. Returns the [VolaAst](crate::VolaAst) on success, or a partially parsed AST, and the reported errors, if any
/// parsing errors happened.
pub fn parse_file(file: impl AsRef<Path>) -> Result<VolaAst, (VolaAst, Vec<ParserError>)> {
    let dta = match std::fs::read(file.as_ref()) {
        Ok(dta) => dta,
        Err(e) => {
            let err = ParserError::FSError(e.to_string());
            vola_common::report(error_reporter(err.clone(), Span::empty()).finish());

            return Err((
                VolaAst {
                    entries: Vec::with_capacity(0),
                },
                vec![err],
            ));
        }
    };
    let file_src_str = file.as_ref().to_str().unwrap_or("NonUnicodeFilename");
    parse_data(&dta, Some(file_src_str.into()))
}

///Parses `string`. Returns the [VolaAst](crate::VolaAst) on success, or a partially parsed AST, and the reported errors, if any
/// parsing errors happened.
pub fn parse_string(string: String) -> Result<VolaAst, (VolaAst, Vec<ParserError>)> {
    let as_bytes = string.as_bytes();
    parse_data(as_bytes, None)
}

pub fn parse_from_bytes(bytes: &[u8]) -> Result<VolaAst, (VolaAst, Vec<ParserError>)> {
    parse_data(bytes, None)
}

//load the vola-lang tree-sitter grammar / parser
fn parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_vola::language())
        .expect("Failed to load tree-sitter-vola");
    parser
}

pub struct VolaTreeSitterParser;

impl vola_ast::VolaParser for VolaTreeSitterParser {
    fn parse_from_byte(
        &self,
        src_file: Option<FileString>,
        byte: &[u8],
    ) -> Result<VolaAst, vola_ast::AstError> {
        parse_data(byte, src_file.clone()).map_err(|(_partial_ast, e)| {
            vola_ast::AstError::ParsingError {
                path: src_file
                    .map(|f| f.to_string())
                    .unwrap_or("NoFile".to_owned()),
                err: format!("Parser had {} errors", e.len()),
            }
        })
    }
}

///Internal parser implementation
fn parse_data(
    data: &[u8],
    src_file: Option<FileString>,
) -> Result<VolaAst, (VolaAst, Vec<ParserError>)> {
    let mut ctx = if let Some(src) = src_file {
        ParserCtx::new(src)
    } else {
        ParserCtx::new_fileless()
    };

    let mut ast = VolaAst {
        entries: Vec::new(),
    };

    //recursively parse all nodes
    let mut parser = parser();
    let syn_tree = match parser.parse(&data, None) {
        None => {
            let err = ParserError::TreeSitterFailed;
            report(error_reporter(err.clone(), Span::empty()).finish());
            return Err((ast, vec![err]));
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
                if let Ok(ctattrib) = CTArg::parse(&mut ctx, data, &node) {
                    ct_args.push(ctattrib);
                }
            }
            _ => {
                match AstEntry::parse(&mut ctx, data, &node) {
                    Ok(tlnode) => {
                        let mut nodes_attribs = Vec::with_capacity(0);
                        //Swapout all collected nodes and start with an (usally) empty vec again.
                        std::mem::swap(&mut ct_args, &mut nodes_attribs);
                        let entry_node = TopLevelNode {
                            span: ctx.span(&node),
                            ct_args: nodes_attribs,
                            entry: tlnode,
                        };
                        ast.entries.push(entry_node);
                    }
                    Err(e) => {
                        ctx.deep_errors.push(e);
                    }
                }
            }
        }
    }

    if ctx.deep_errors.len() > 0 {
        Err((ast, ctx.deep_errors))
    } else {
        Ok(ast)
    }
}
