/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

#![doc(html_logo_url = "https://gitlab.com/tendsinmende/vola/-/raw/main/resources/vola_icon.svg")]

use error::ParserError;
use std::path::Path;
use tree_sitter::{Node, Parser};
pub use tree_sitter_vola;
use vola_ast::{
    alge::Expr,
    common::{CTArg, Comment, Stmt},
    AstEntry, TopLevelNode, VolaAst,
};
use vola_common::{FileString, Span, VolaError};
pub mod alge;
pub mod block;
pub mod common;
pub mod csg;
pub mod error;
pub mod toplevel;
///Context on the parser, like the current src file, and errors that occured, but are ignored.
pub struct ParserCtx {
    deep_errors: Vec<VolaError<ParserError>>,
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
    ) -> Result<Self, VolaError<ParserError>>
    where
        Self: Sized;
}

///Parses `file`. Returns the [VolaAst] on success, or a partially parsed AST, and the reported errors, if any
/// parsing errors happened.
pub fn parse_file(
    file: impl AsRef<Path>,
) -> Result<VolaAst, (VolaAst, Vec<VolaError<ParserError>>)> {
    let dta = match std::fs::read(file.as_ref()) {
        Ok(dta) => dta,
        Err(e) => {
            let err = VolaError::new(ParserError::FSError(e.to_string()));
            err.report();
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

///Parses `string`. Returns the [VolaAst] on success, or a partially parsed AST, and the reported errors, if any
/// parsing errors happened.
pub fn parse_string(string: String) -> Result<VolaAst, (VolaAst, Vec<VolaError<ParserError>>)> {
    let as_bytes = string.as_bytes();
    parse_data(as_bytes, None)
}

pub fn parse_from_bytes(bytes: &[u8]) -> Result<VolaAst, (VolaAst, Vec<VolaError<ParserError>>)> {
    parse_data(bytes, None)
}

///Tries to parse an element `E` via the tree-sitter-grammar from the given string. Note that `E` must be part of the _source_file_
///level of the grammar. If you are unsure, use a helper like [parse_expr] or [parse_stmt] instead.
pub fn parse_element<E: FromTreeSitter>(content: &str) -> Result<E, VolaError<ParserError>> {
    let mut parser = parser();
    let syn_tree = match parser.parse(content.as_bytes(), None) {
        None => {
            return Err(VolaError::new(ParserError::TreeSitterFailed));
        }
        Some(syntree) => syntree,
    };
    let mut ctx = ParserCtx::new_fileless();

    if syn_tree.root_node().child_count() != 1 {
        return Err(VolaError::new(ParserError::Other(format!(
            "Expected one child of root-node, had {}\n{}",
            syn_tree.root_node().child_count(),
            syn_tree.root_node().to_sexp()
        ))));
    }

    E::parse(
        &mut ctx,
        content.as_bytes(),
        &syn_tree.root_node().child(0).unwrap(),
    )
}

///Tries to parse `content` into a valid expression.
pub fn parse_expr(content: &str) -> Result<Expr, VolaError<ParserError>> {
    parse_element::<Expr>(content)
}

pub fn parse_stmt(content: &str) -> Result<Stmt, VolaError<ParserError>> {
    parse_element::<Stmt>(content)
}

//load the vola-lang tree-sitter grammar / parser
fn parser() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(&tree_sitter_vola::language())
        .expect("Failed to load tree-sitter-vola");
    parser
}

pub struct VolaTreeSitterParser;

impl vola_ast::VolaParser for VolaTreeSitterParser {
    type Error = ParserError;
    fn parse_from_byte(
        &self,
        src_file: Option<FileString>,
        byte: &[u8],
    ) -> Result<VolaAst, Vec<VolaError<Self::Error>>> {
        parse_data(byte, src_file.clone()).map_err(|(_rest_ast, e)| e)
    }
}

///Internal parser implementation
fn parse_data(
    data: &[u8],
    src_file: Option<FileString>,
) -> Result<VolaAst, (VolaAst, Vec<VolaError<ParserError>>)> {
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
            return Err((ast, vec![VolaError::new(ParserError::TreeSitterFailed)]));
        }
        Some(syntree) => syntree,
    };

    let mut cursor = syn_tree.root_node().walk();
    //Collects args until a non-comment, non-ct-arg node is returned. Attaches all preceeding ctargs
    // to that toplevel node.
    let mut ct_args = Vec::with_capacity(0);
    for node in syn_tree.root_node().children(&mut cursor) {
        match node.kind() {
            "comment" => match Comment::parse(&mut ctx, data, &node) {
                Ok(c) => {
                    let astnode = AstEntry::Comment(c);
                    let entry_node = TopLevelNode {
                        span: ctx.span(&node),
                        //NOTE: Comments can't be tagged with ct-args atm.
                        ct_args: Vec::with_capacity(0),
                        entry: astnode,
                    };
                    ast.entries.push(entry_node);
                }
                Err(e) => ctx.deep_errors.push(e),
            },
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

#[cfg(test)]
mod test {
    use vola_ast::{alge::Expr, common::Stmt};

    use crate::parse_element;

    #[test]
    fn parse_literal_int() {
        let s = "1";
        let e = parse_element::<Expr>(&s).unwrap();
    }
    #[test]
    fn parse_literal_real() {
        let s = "1.0";
        let e = parse_element::<Expr>(&s).unwrap();
    }

    #[test]
    fn parse_binary_expr() {
        let s = "1 + 1";
        let e = parse_element::<Expr>(&s).unwrap();
    }

    #[test]
    fn parse_stmt_expr() {
        let s = "let a = 1 + 1;";
        let e = parse_element::<Stmt>(&s).unwrap();
    }
}
