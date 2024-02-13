use smallvec::SmallVec;
use vola_common::{CommonError, ErrorReporter, Span};

use crate::{
    alge::AlgeExpr,
    common::{Call, Ident},
    csg::{AccessDesc, CSGBinding, CSGOp},
    error::ParserError,
};

use super::FromTreeSitter;

impl FromTreeSitter for AccessDesc {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(reporter, node, "access_decl")?;

        let src_tree = Ident::parse(reporter, dta, &node.child(0).unwrap())?;
        ParserError::consume_expected_node_kind(reporter, node.child(1), ".")?;
        let concept_call = Call::parse(reporter, dta, &node.child(2).unwrap())?;

        ParserError::assert_node_no_error(reporter, node)?;
        Ok(AccessDesc {
            span: Span::from(node),
            tree_ref: src_tree,
            call: concept_call,
        })
    }
}

impl FromTreeSitter for CSGBinding {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        ParserError::assert_node_kind(reporter, node, "csg_binding")?;

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        ParserError::consume_expected_node_kind(reporter, children.next(), "csg")?;

        let csg_ident = Ident::parse(reporter, dta, &children.next().unwrap())?;

        ParserError::consume_expected_node_kind(reporter, children.next(), "=")?;

        let tree = CSGOp::parse(reporter, dta, &children.next().unwrap())?;

        ParserError::assert_node_no_error(reporter, node)?;
        Ok(CSGBinding {
            span: Span::from(node),
            decl_name: csg_ident,
            tree,
        })
    }
}

impl FromTreeSitter for CSGOp {
    fn parse(
        reporter: &mut ErrorReporter<ParserError>,
        dta: &[u8],
        node: &tree_sitter::Node,
    ) -> Result<Self, ParserError>
    where
        Self: Sized,
    {
        if node.kind() != "csg_unary" && node.kind() != "csg_binary" && node.kind() != "fn_call" {
            let err = ParserError::UnexpectedAstNode {
                kind: node.kind().to_owned(),
                expected: "csg_unary | csg_binary | fn_call".to_owned(),
            };
            reporter.push_error(CommonError::new(Span::from(node), err.clone()));
            return Err(err);
        }

        //Right now there are three kinds of CSGOPs
        // 1. csg_unary / single sub tree,
        // 2. csg_binary / two sub trees,
        // 3. fn_call / primitive call,
        //
        // NOTE: This might change at some point, if we want to allow an arbitrary amount of sub-trees.
        //       But for now we mirror miniSDF's language in this point.
        //
        //
        // Parsing wise, we always first parse a fn_call structure, then, depending on the kind
        // either 0, 1 or 2 sub trees.

        let mut walker = node.walk();
        let mut children = node.children(&mut walker);
        let ident = Ident::parse(reporter, dta, &children.next().unwrap())?;

        ParserError::consume_expected_node_kind(reporter, children.next(), "(")?;

        let mut params = SmallVec::new();
        while let Some(next_node) = children.next() {
            match next_node.kind() {
                ")" => break,
                "alge_expr" => params.push(AlgeExpr::parse(reporter, dta, &next_node)?),
                _ => {
                    let err = ParserError::UnexpectedAstNode {
                        kind: next_node.kind().to_owned(),
                        expected: "alge_expr | )".to_owned(),
                    };
                    reporter.push_error(CommonError::new(Span::from(node), err.clone()));
                    return Err(err);
                }
            }
        }

        //now, depending on the actual node kind, parse one, two or zero sub trees
        let sub_trees = match node.kind() {
            "csg_unary" => {
                let mut subtrees = Vec::with_capacity(1);
                ParserError::consume_expected_node_kind(reporter, children.next(), "{")?;
                subtrees.push(CSGOp::parse(reporter, dta, &children.next().unwrap())?);
                ParserError::consume_expected_node_kind(reporter, children.next(), "}")?;
                subtrees
            }
            "csg_binary" => {
                let mut subtrees = Vec::with_capacity(2);
                //left tree
                ParserError::consume_expected_node_kind(reporter, children.next(), "{")?;
                subtrees.push(CSGOp::parse(reporter, dta, &children.next().unwrap())?);
                ParserError::consume_expected_node_kind(reporter, children.next(), "}")?;
                //right tree
                ParserError::consume_expected_node_kind(reporter, children.next(), "{")?;
                subtrees.push(CSGOp::parse(reporter, dta, &children.next().unwrap())?);
                ParserError::consume_expected_node_kind(reporter, children.next(), "}")?;

                subtrees
            }
            "fn_call" => Vec::with_capacity(0),
            _ => {
                //NOTE this should error at fn entry already.
                panic!("Failed at subtree parsing, should error earlier");
            }
        };

        ParserError::assert_ast_level_empty(reporter, children.next())?;

        ParserError::assert_node_no_error(reporter, node)?;
        Ok(CSGOp {
            span: Span::from(node),
            op: ident,
            args: params,
            sub_trees,
        })
    }
}
