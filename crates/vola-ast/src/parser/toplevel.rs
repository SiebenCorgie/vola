use smallvec::SmallVec;
use vola_common::{CommonError, ErrorReporter, Span};

use crate::{
    alge::LetStmt,
    common::{Call, Ident, TypedIdent},
    csg::{AccessDesc, CSGBinding, CSGOp, CSGStmt, ExportFn, FieldDef},
    error::ParserError,
    AstEntry,
};

use super::FromTreeSitter;

impl FromTreeSitter for AstEntry {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        //Collects all compile-time attributes. They are appended to a toplevel construct,
        // whenever needed.
        //        let mut attrib_collector = Vec::new();

        println!("Teddy B");
        match node.kind() {
            //always ignore
            "comment" => Ok(AstEntry::Comment(Span::from(node))),
            "field_decl" => {
                let field_def = FieldDef::parse(reporter, dta, node)?;
                Ok(AstEntry::FieldDefine(field_def))
            }
            "field_export" => {
                let field_export = ExportFn::parse(reporter, dta, node)?;
                Ok(AstEntry::ExportFn(field_export))
            }
            "def_concept" => {
                println!(" Def -> ");
                todo!()
            }
            "def_operation" => {
                println!("def op ->");
                todo!()
            }
            "impl_block" => {
                println!("impl block ->");
                todo!()
            }
            "ct_attrib" => {
                println!("ct attrib ->");
                todo!()
            }
            _ => {
                println!("Bing bong");
                let err = ParserError::UnknownAstNode(node.kind().to_owned());
                reporter.push_error(CommonError::new(Span::from(node), err));
                Err(ParserError::UnknownAstNode(node.kind().to_owned()))
            }
        }
    }
}

impl FromTreeSitter for FieldDef {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(reporter, node, "field_decl")?;

        let mut cursor = node.walk();
        let mut children = node.children(&mut cursor);

        let field_ident = if let Some(child) = children.next() {
            Ident::parse(reporter, dta, &child)
        } else {
            Err(ParserError::NoChildAvailable)
        }?;

        ParserError::assert_node_no_error(reporter, node)?;
        Ok(FieldDef {
            span: Span::from(node),
            name: field_ident,
            inputs: SmallVec::new(),
            stmts: Vec::new(),
            ret: CSGOp {
                span: Span::empty(),
                op: Ident("Teddy".to_owned()),
                args: SmallVec::new(),
                sub_trees: Vec::new(),
            },
        })
    }
}

impl FromTreeSitter for ExportFn {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(reporter, node, "field_export")?;

        let mut cursor = node.walk();
        let mut children = node.children(&mut cursor);

        ParserError::consume_expected_node_kind(reporter, children.next(), "export")?;

        let field_ident = if let Some(child) = children.next() {
            Ident::parse(reporter, dta, &child)
        } else {
            Err(ParserError::NoChildAvailable)
        }?;

        ParserError::consume_expected_node_kind(reporter, children.next(), "(")?;

        let mut args = SmallVec::new();

        while let Some(next_node) = children.next() {
            match next_node.kind() {
                //found the end
                ")" => break,
                "typed_arg" => {
                    let arg = TypedIdent::parse(reporter, dta, &next_node)?;
                    args.push(arg);
                }
                //part of the list, just ignore
                "," => {}
                _ => {
                    let error = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "typed_arg".to_owned(),
                    };
                    reporter.push_error(CommonError::new_on_node(&next_node, error.clone()));
                    return Err(error);
                }
            }
        }

        //We should start with a block now
        ParserError::consume_expected_node_kind(reporter, children.next(), "{")?;

        let mut stmts = Vec::new();
        let mut access_descriptors = SmallVec::new();
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                "comment" => {}
                "let_stmt" => {
                    //take away the ;
                    stmts.push(CSGStmt::LetStmt(LetStmt::parse(reporter, dta, &next_node)?));
                    ParserError::consume_expected_node_kind(reporter, children.next(), ";")?;
                }
                "csg_binding" => {
                    stmts.push(CSGStmt::CSGBinding(CSGBinding::parse(
                        reporter, dta, &next_node,
                    )?));
                    //take away the ;
                    ParserError::consume_expected_node_kind(reporter, children.next(), ";")?;
                }

                //At this point, change into acces_decleration parsing, which must be the last part of the block
                "access_desc" => {
                    let mut access_decl_walker = next_node.walk();
                    for child in next_node.children(&mut access_decl_walker) {
                        match child.kind() {
                            //start and , can be ignored
                            "(" | "," => {}
                            "access_decl" => {
                                access_descriptors.push(AccessDesc::parse(reporter, dta, &child)?);
                            }
                            ")" => break,
                            _ => {
                                let error = ParserError::UnexpectedAstNode {
                                    kind: child.kind().to_owned(),
                                    expected: "access_decl | , | (".to_owned(),
                                };
                                reporter.push_error(CommonError::new_on_node(
                                    &next_node,
                                    error.clone(),
                                ));
                                return Err(error);
                            }
                        }
                    }

                    break;
                }
                _ => {
                    let error = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "comment | let_stmt | csg_binding | access_decl".to_owned(),
                    };
                    reporter.push_error(CommonError::new_on_node(&next_node, error.clone()));
                    return Err(error);
                }
            }
        }

        //check that we acutally are at the end
        ParserError::consume_expected_node_kind(reporter, children.next(), "}")?;

        if access_descriptors.len() == 0 {
            let err = ParserError::NoAccessDecs;
            reporter.push_error(CommonError::new_on_node(node, err.clone()));
            return Err(err);
        }

        ParserError::assert_node_no_error(reporter, node)?;
        Ok(ExportFn {
            span: Span::from(node),
            name: field_ident,
            inputs: args,
            stmts,
            access_descriptors,
        })
    }
}
