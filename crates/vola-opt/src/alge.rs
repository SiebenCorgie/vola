//! # Alge dialect
//!

use crate::{common::LmdContext, error::OptError, Optimizer};
use ahash::{AHashMap, AHashSet};
use rvsdg::{
    edge::{InportLocation, OutportLocation},
    nodes::LambdaNode,
    NodeRef,
};
use vola_ast::{
    alge::{AlgeStmt, ImplBlock},
    common::Ident,
    csg::CSGNodeTy,
};
use vola_common::{report, Span};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ConceptImplKey {
    pub concept_name: String,
    ///Name of either a entity or operation definition.
    pub node_name: String,
}

type CvDesc = AHashMap<String, AHashSet<String, (InportLocation, OutportLocation)>>;

///Contains the meta data of a concept implementation
pub struct ConceptImpl {
    pub span: Span,
    pub concept: Ident,
    pub node_type: CSGNodeTy,

    /// Operand variable description. This is the actual _generic_ interface of this λ-Node.
    ///
    /// In practice we might know that `OP<a,b>` has _two_ sub-trees. However, we don't know which concepts
    /// of a,b this λ is interested in. Therefore when building the block, everytime we eval a.X or b.Y, we check if concept
    /// a.X or b.Y are already described by the argument-interface. If not, we push it into the lookup table.
    ///
    /// When building a CSG tree we can then configure a.X and b.Y via the argument, which always need to be Lambda nodes.
    /// and call them as described by concept that is implemented.
    ///
    /// This map is keyed by operand-name first and operand's concept name second.
    //TODO The hash map might be overkill, but its working right now.
    // This is a 1..2 long hashmap usually. So this is really not worth it lol.
    pub cv_desc: CvDesc,

    ///The lambda node representing this expression
    pub lambda: NodeRef,
}

impl ConceptImpl {
    fn build_block(
        self,
        opt: &mut Optimizer,
        block: ImplBlock,
        lmd_context: LmdContext,
    ) -> Result<Self, OptError> {
        println!("Todo impl {}", block.dst.0);
        Ok(self)
    }
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

        let src_csg_def = if let Some(src_csg_def) = self.csg_node_defs.get(&implblock.dst.0) {
            src_csg_def
        } else {
            let err = OptError::AnySpanned {
                span: implblock.span.clone().into(),
                text: format!(
                    "csg-node-type (entity or operation) {} is undefined!",
                    implblock.concept.0
                ),
                span_text: format!(
                    "Consider defining an entity or operation named \"{}\"",
                    implblock.concept.0
                ),
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

        //Make sure that an entity has no sub operands
        if src_csg_def.ty == CSGNodeTy::Entity && implblock.operands.len() > 0 {
            let err = OptError::AnySpanned {
                span: implblock.span.clone().into(),
                text: format!(
                    "entity {} cannot have CSG operands. Only operations can have CSG operands!",
                    implblock.dst.0
                ),
                span_text: format!(
                    "Consider removing the operands, or implementing an \"operation\" instead"
                ),
            };
            report(err.clone(), implblock.span.get_file());
            return Err(err);
        }

        //prebuild the concept_key so we can drop our src_concept references early.
        let concept_key = ConceptImplKey {
            concept_name: src_concept.name.0.clone(),
            node_name: src_csg_def.name.0.clone(),
        };

        //At this point we should have verified the overall signature. We therefore start building the actual λ-Node.
        // We do this by building the function-context helper that contains all concept-args as defined variables, as well as the
        // operation variables.

        //Create the lambda node. NOTE that we currently alway export. For debug purposes.
        let (lmd, _) = self
            .graph
            .on_omega_node(|omg| omg.new_function(true, |_| {}));

        let lmd_context = LmdContext::new_for_impl_block(
            &mut self.graph,
            &mut self.typemap,
            lmd.clone(),
            &implblock,
            &src_csg_def,
            &src_concept,
        );

        println!("LMD context: {:#?}", lmd_context);

        //Now reverse the game by building the initial ConceptImpl and then letting it handle itself.
        let concept_impl = ConceptImpl {
            span: implblock.span.clone(),
            concept: implblock.concept.clone(),
            node_type: src_csg_def.ty.clone(),
            cv_desc: CvDesc::default(),
            lambda: lmd,
        };

        let build_concept_impl = concept_impl.build_block(self, implblock, lmd_context)?;

        //After finishing (successfully) add the impl description to the Optimizer so we can
        // find that later on if needed and return
        //NOTE: _should be the same, but could also be changed at some point I guess_
        let lmd = build_concept_impl.lambda.clone();

        let old = self.concept_impl.insert(concept_key, build_concept_impl);
        assert!(old.is_none(), "Had an old concept + node combination already, should have been caught before adding it!");

        Ok(lmd)
    }
}
