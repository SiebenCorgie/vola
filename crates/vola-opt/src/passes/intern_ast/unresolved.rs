/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Helper dialect that can place unresolved nodes while building an AST.
//!
//! Supplies a resolve pass, that tries to resolve everything, once all information is expected to exist.

use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::smallvec,
    SmallColl,
};
use rvsdg_viewer::View;

use crate::{DialectNode, OptNode};

#[derive(LangNode)]
pub struct UnresolvedCall {
    ///The function that is called
    pub function_ident: String,
    #[inputs]
    pub inputs: SmallColl<Input>,
    #[output]
    pub output: Output,
}

impl UnresolvedCall {
    pub fn new_with_args(ident: String, arg_count: usize) -> Self {
        Self {
            function_ident: ident,
            inputs: smallvec![Input::default(); arg_count],
            output: Output::default(),
        }
    }
}

impl View for UnresolvedCall {
    fn name(&self) -> String {
        format!("UCall<{}>", self.function_ident)
    }
    fn color(&self) -> rvsdg_viewer::Color {
        rvsdg_viewer::Color::from_rgba(255, 200, 150, 255)
    }
    fn stroke(&self) -> rvsdg_viewer::Stroke {
        rvsdg_viewer::Stroke::Line
    }
}

impl DialectNode for UnresolvedCall {
    fn dialect(&self) -> &'static str {
        "unresolved"
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode::new(
            Self::new_with_args(self.function_ident.clone(), self.inputs.len()),
            span,
        )
    }
    fn try_derive_type(
        &self,
        _typemap: &rvsdg::attrib::FlagStore<crate::common::Ty>,
        _graph: &crate::OptGraph,
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CSGConcept>,
        _csg_defs: &ahash::AHashMap<String, vola_ast::csg::CsgDef>,
    ) -> Result<Option<crate::common::Ty>, crate::OptError> {
        Err(crate::OptError::Any {
            text: format!("Cannot type unresolved-call to \"{}\"", self.function_ident),
        })
    }

    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        None
    }

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if let Some(other) = other.try_downcast_ref::<Self>() {
            other.function_ident == self.function_ident
        } else {
            false
        }
    }
}

#[derive(LangNode)]
pub struct UnresolvedEval {
    pub concept: String,
    #[inputs]
    pub inputs: SmallColl<Input>,
    #[output]
    pub output: Output,
}

impl UnresolvedEval {
    pub fn new_with_args(concept: String, arg_count: usize) -> Self {
        Self {
            concept,
            inputs: smallvec![Input::default(); arg_count],
            output: Output::default(),
        }
    }
}

impl View for UnresolvedEval {
    fn name(&self) -> String {
        format!("UEval<{}>", self.concept)
    }
    fn color(&self) -> rvsdg_viewer::Color {
        rvsdg_viewer::Color::from_rgba(255, 200, 150, 255)
    }
    fn stroke(&self) -> rvsdg_viewer::Stroke {
        rvsdg_viewer::Stroke::Line
    }
}

impl DialectNode for UnresolvedEval {
    fn dialect(&self) -> &'static str {
        "unresolved"
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode::new(
            Self::new_with_args(self.concept.clone(), self.inputs.len()),
            span,
        )
    }
    fn try_derive_type(
        &self,
        _typemap: &rvsdg::attrib::FlagStore<crate::common::Ty>,
        _graph: &crate::OptGraph,
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CSGConcept>,
        _csg_defs: &ahash::AHashMap<String, vola_ast::csg::CsgDef>,
    ) -> Result<Option<crate::common::Ty>, crate::OptError> {
        Err(crate::OptError::Any {
            text: format!("Cannot type unresolved-eval to \"{}\"", self.concept),
        })
    }

    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        None
    }

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if let Some(other) = other.try_downcast_ref::<Self>() {
            other.concept == self.concept
        } else {
            false
        }
    }
}
