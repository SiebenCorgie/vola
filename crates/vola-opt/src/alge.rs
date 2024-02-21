//! # Alge dialect
//!

use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    NodeRef,
};
use vola_ast::{common::Ident, csg::CSGNodeTy};

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ConceptImplKey {
    pub concept_name: String,
    ///Name of either a entity or operation definition.
    pub onode_name: String,
}

///Contains the meta data of a concept implementation
pub struct ConceptImpl {
    pub concept: Ident,
    pub node_type: CSGNodeTy,

    ///Number of sub-trees that can be attached to this node. By definition those are
    /// the first n-context variables of the lambda node.
    pub num_sub_trees: usize,

    ///The lambda node representing this expression
    pub lambda: NodeRef,
}
