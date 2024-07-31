/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Trigonometric operations

use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    SmallColl,
};
use rvsdg_viewer::{Color, View};
use vola_common::Span;

use crate::{
    common::Ty,
    imm::{ImmScalar, ImmVector},
    DialectNode, OptError, OptNode,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrigOp {
    Sin,
    Cos,
    Tan,
    ASin,
    ACos,
    ATan,
}

#[derive(LangNode)]
pub struct Trig {
    pub op: TrigOp,
    #[input]
    pub inputs: Input,
    #[output]
    pub output: Output,
}

impl Trig {
    pub fn new(op: TrigOp) -> Self {
        Trig {
            op,
            inputs: Input::default(),
            output: Output::default(),
        }
    }
}

impl View for Trig {
    fn name(&self) -> String {
        format!("{:?}", self.op)
    }

    fn stroke(&self) -> rvsdg_viewer::Stroke {
        rvsdg_viewer::Stroke::Line
    }
    fn color(&self) -> Color {
        super::ALGE_VIEW_COLOR
    }
}

impl DialectNode for Trig {
    fn dialect(&self) -> &'static str {
        "alge"
    }
    fn try_derive_type(
        &self,
        _typemap: &rvsdg::attrib::FlagStore<Ty>,
        graph: &crate::OptGraph,
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CSGConcept>,
        _csg_defs: &ahash::AHashMap<String, vola_ast::csg::CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        let input_ty = if let Some(edg) = &self.inputs.edge {
            //resolve if there is a type set
            if let Some(t) = graph.edge(*edg).ty.get_type() {
                t.clone()
            } else {
                return Ok(None);
            }
        } else {
            //input not set atm. so return None as well
            return Ok(None);
        };

        match input_ty {
            Ty::Scalar => {}
            Ty::Vector { .. } => {}
            _ => {
                return Err(OptError::Any {
                    text: format!(
                        "{:?} expects operands of type scalar or vector, got {:?}",
                        self.op, input_ty
                    ),
                })
            }
        }

        //seems to be alright, return scalar
        Ok(Some(input_ty))
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(Trig {
                inputs: Input::default(),
                output: Output::default(),
                op: self.op.clone(),
            }),
        }
    }

    fn is_operation_equal(&self, other: &crate::OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<Trig>() {
            other_cop.op == self.op
        } else {
            false
        }
    }

    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        //defined on singular scalar or vector
        if src_nodes.len() == 0 {
            return None;
        }

        if src_nodes[0].is_none() {
            return None;
        }

        if !src_nodes[0].as_ref().unwrap().node_type.is_simple() {
            return None;
        }

        let op = match self.op {
            TrigOp::Sin => f64::sin,
            TrigOp::Cos => f64::cos,
            TrigOp::Tan => f64::tan,
            TrigOp::ASin => f64::asin,
            TrigOp::ACos => f64::acos,
            TrigOp::ATan => f64::atan,
        };

        if let Some(vec) = src_nodes[0]
            .as_ref()
            .unwrap()
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<ImmVector>()
        {
            let new: SmallColl<f64> = vec.lit.iter().map(|element| op(*element)).collect();
            return Some(OptNode::new(ImmVector::new(new.as_slice()), Span::empty()));
        }
        if let Some(scalar) = src_nodes[0]
            .as_ref()
            .unwrap()
            .node_type
            .unwrap_simple_ref()
            .try_downcast_ref::<ImmScalar>()
        {
            return Some(OptNode::new(ImmScalar::new(op(scalar.lit)), Span::empty()));
        }

        None
    }
}
