/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! relational operations
use ahash::AHashMap;
use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
};
use rvsdg_viewer::View;
use vola_ast::csg::{CsgConcept, CsgDef};
use vola_common::Span;

use crate::{
    common::{DataType, Ty},
    imm::{ImmBool, ImmNat, ImmScalar},
    DialectNode, OptError, OptNode,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryRelOp {
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    NotEq,
}

#[derive(LangNode)]
pub struct BinaryRel {
    pub op: BinaryRelOp,
    #[inputs]
    pub inputs: [Input; 2],
    #[output]
    pub output: Output,
}

impl BinaryRel {
    pub fn new(op: BinaryRelOp) -> Self {
        BinaryRel {
            op,
            inputs: [Input::default(), Input::default()],
            output: Output::default(),
        }
    }
}

impl View for BinaryRel {
    fn name(&self) -> String {
        format!("{:?}", self.op)
    }

    fn stroke(&self) -> rvsdg_viewer::Stroke {
        rvsdg_viewer::Stroke::Line
    }
    fn color(&self) -> rvsdg_viewer::Color {
        super::ALGE_VIEW_COLOR
    }
}

impl DialectNode for BinaryRel {
    fn dialect(&self) -> &'static str {
        "alge"
    }
    fn try_derive_type(
        &self,
        input_types: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
        _csg_defs: &AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        assert_eq!(input_types.len(), 2);
        let t0 = input_types[0].clone();
        let t1 = input_types[1].clone();

        //NOTE same test regardless if its & or |
        if t0 != t1 {
            return Err(OptError::Any {
                text: format!(
                    "{:?} expectes the same type for both operands, got {} & {}",
                    self.op, t0, t1
                ),
            });
        }

        match &t0{
            &Ty::SCALAR_INT | &Ty::SCALAR_REAL => Ok(Ty::scalar_type(DataType::Bool)),
            any => {
                Err(OptError::Any { text: format!("Cannot use comperator {:?} on {}. Consider breaking it down to either a simple scalar or natural value", self.op, any) })
            }
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(BinaryRel {
                inputs: [Input::default(), Input::default()],
                output: Output::default(),
                op: self.op.clone(),
            }),
        }
    }

    fn is_operation_equal(&self, other: &crate::OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<BinaryRel>() {
            other_cop.op == self.op
        } else {
            false
        }
    }

    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        //implemented only for pairs of scalar on nat
        if src_nodes.len() != 2 {
            return None;
        }
        if src_nodes[0].is_none() || src_nodes[1].is_none() {
            return None;
        }

        if !src_nodes[0].as_ref().unwrap().node_type.is_simple()
            || !src_nodes[1].as_ref().unwrap().node_type.is_simple()
        {
            return None;
        }

        let a = src_nodes[0].as_ref().unwrap().node_type.unwrap_simple_ref();
        let b = src_nodes[1].as_ref().unwrap().node_type.unwrap_simple_ref();

        if let (Some(a), Some(b)) = (
            a.try_downcast_ref::<ImmScalar>(),
            b.try_downcast_ref::<ImmScalar>(),
        ) {
            let op = match self.op {
                BinaryRelOp::Lt => f64::lt,
                BinaryRelOp::Gt => f64::gt,
                BinaryRelOp::Lte => f64::le,
                BinaryRelOp::Gte => f64::ge,
                BinaryRelOp::Eq => f64::eq,
                BinaryRelOp::NotEq => |a: &f64, b: &f64| a != b,
            };

            return Some(OptNode::new(
                ImmBool::new(op(&a.lit, &b.lit)),
                Span::empty(),
            ));
        }

        if let (Some(a), Some(b)) = (
            a.try_downcast_ref::<ImmNat>(),
            b.try_downcast_ref::<ImmNat>(),
        ) {
            let op = match self.op {
                BinaryRelOp::Lt => u64::lt,
                BinaryRelOp::Gt => u64::gt,
                BinaryRelOp::Lte => u64::le,
                BinaryRelOp::Gte => u64::ge,
                BinaryRelOp::Eq => u64::eq,
                BinaryRelOp::NotEq => |a: &u64, b: &u64| a != b,
            };

            return Some(OptNode::new(
                ImmBool::new(op(&a.lit, &b.lit)),
                Span::empty(),
            ));
        }

        None
    }
}
