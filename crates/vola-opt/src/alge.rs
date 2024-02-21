//! # Alge dialect
//!

use crate::{error::OptError, Optimizer};
use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    NodeRef,
};
use vola_ast::{alge::ImplBlock, common::Ident, csg::CSGNodeTy};
use vola_common::{report, Span};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ConceptImplKey {
    pub concept_name: String,
    ///Name of either a entity or operation definition.
    pub node_name: String,
}

///Contains the meta data of a concept implementation
pub struct ConceptImpl {
    pub span: Span,
    pub concept: Ident,
    pub node_type: CSGNodeTy,

    ///Number of sub-trees that can be attached to this node. By definition those are
    /// the first n-context variables of the lambda node.
    pub num_sub_trees: usize,

    ///The lambda node representing this expression
    pub lambda: NodeRef,
}

impl Optimizer {
    ///Adds this implementation block to the optimizer. Takes care of all the initial legalizing as well.
    pub fn add_impl_block(&mut self, implblock: ImplBlock) -> Result<NodeRef, OptError> {
        //First, check if an implementation for that concept and node already exists
        let key = ConceptImplKey {
            concept_name: implblock.concept.0.clone(),
            node_name: implblock.dst.0.clone(),
        };

        //Check that the impl block doesn't yet exist
        if let Some(existing_key) = self.concept_impl.get(&key) {
            let err = OptError::AnySpannedWithSource {
                source_span: existing_key.span.clone().into(),
                source_text: format!(
                    "first impl of concept {} for node {}",
                    implblock.concept.0, implblock.dst.0
                ),
                text: format!(
                    "Tried to re-implement the concept {} for the entity or operation {}.",
                    implblock.concept.0, implblock.dst.0
                ),
                span: implblock.span.into(),
                span_text: format!("Second implementation here"),
            };

            report(err.clone(), existing_key.span.get_file());
            return Err(err);
        }

        //Now try to retrieve the actual definition of the concept, and check that we are abiding to its definition
        let src_concept = if let Some(src_concept) = self.concepts.get(&implblock.concept.0) {
            src_concept
        } else {
            //Could not find the source concept, bail!
            let err = OptError::AnySpanned {
                span: implblock.span.clone().into(),
                text: format!("Concept {} is undefined!", implblock.concept.0),
                span_text: format!("Consider defining the concept \"{}\"", implblock.concept.0),
            };

            report(err.clone(), implblock.span.get_file());
            return Err(err);
        };

        //Check that the amount of arguments mirror the concept's argument count.
        if src_concept.src_ty.len() != implblock.concept_arg_naming.len() {
            let err = OptError::AnySpannedWithSource {
                source_span: src_concept.span.clone().into(),
                source_text: format!("Src concept defines {} arguments", src_concept.src_ty.len()),
                text: format!(
                    "argument definition has {} arguments",
                    src_concept.src_ty.len(),
                ),
                span: implblock.span.into(),
                span_text: format!(
                    "this should take {} arguments, not {}",
                    src_concept.src_ty.len(),
                    implblock.concept_arg_naming.len()
                ),
            };
            report(err.clone(), src_concept.span.get_file());
            return Err(err);
        }

        //At this point we should have verified the overall signature. We therfore start building the actual λ-Node.
        // We do this by building the function-context helper that contains all concept-args as defined variables, as well as the
        // operation variables.

        todo!()
    }
}
