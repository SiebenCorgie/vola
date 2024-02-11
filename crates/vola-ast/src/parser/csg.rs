use vola_common::ErrorReporter;

use crate::{
    common::{Call, Ident},
    csg::AccessDesc,
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

        Ok(AccessDesc {
            tree_ref: src_tree,
            call: concept_call,
        })
    }
}
