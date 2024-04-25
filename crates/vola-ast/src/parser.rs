/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
use std::path::Path;

use tree_sitter::{Node, Parser};
use vola_common::{report, FileString, Span};

use crate::{common::CTArg, error::ParserError, AstEntry, TopLevelNode, VolaAst};

mod alge;
mod common;
mod csg;
mod toplevel;

///Context on the parser, like the current src file, and errors that occured, but are ignored.
pub struct ParserCtx {
    deep_errors: Vec<ParserError>,
    src_file: FileString,
}

impl ParserCtx {
    pub fn new(file: &str) -> ParserCtx {
        ParserCtx {
            deep_errors: Vec::new(),
            src_file: FileString::from(file),
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
            vola_common::report(err.clone(), file.as_ref().to_str());

            return Err((
                VolaAst {
                    entries: Vec::with_capacity(0),
                },
                vec![err],
            ));
        }
    };
    let file_src_str = file.as_ref().to_str().unwrap_or("NonUnicodeFilename");
    parse_data(&dta, Some(file_src_str))
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

///Internal parser implementation
fn parse_data(data: &[u8], src_file: Option<&str>) -> Result<VolaAst, (VolaAst, Vec<ParserError>)> {
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
            report(err.clone(), ctx.get_file());
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
