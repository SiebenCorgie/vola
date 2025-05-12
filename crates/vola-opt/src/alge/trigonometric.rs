/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Trigonometric operations

use ahash::AHashMap;
use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
    SmallColl,
};
use rvsdg_viewer::{Color, View};
use vola_ast::csg::{CSGConcept, CsgDef};
use vola_common::Span;

use crate::{
    common::{DataType, Shape, Ty},
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
    //Atan(y_over_x)
    ATan,

    Sinh,
    Cosh,
    Tanh,

    ASinh,
    ACosh,
    ATanh,

    //Atan2(y, x) see Atan2 https://registry.khronos.org/SPIR-V/specs/unified1/GLSL.std.450.pdf
    ATan2,
}

#[derive(LangNode)]
pub struct Trig {
    pub op: TrigOp,
    #[inputs]
    pub inputs: SmallVec<[Input; 2]>,
    #[output]
    pub output: Output,
}

impl Trig {
    pub fn new(op: TrigOp) -> Self {
        let inputs = if let TrigOp::ATan2 = op {
            smallvec![Input::default(); 2]
        } else {
            smallvec![Input::default(); 1]
        };

        Trig {
            op,
            inputs,
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
        input_types: &[Ty],
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &ahash::AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //check input count
        match self.op {
            TrigOp::ATan2 => {
                assert!(input_types.len() == 2);
                assert!(input_types[0] == input_types[1]);
            }
            _ => {
                assert!(input_types.len() == 1);
            }
        }

        let input_ty = input_types[0].clone();

        match input_ty {
            Ty::SCALAR_REAL => {}
            Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Vec { .. },
            } => {}
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
        Ok(input_ty)
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(Trig {
                inputs: smallvec![Input::default(); self.inputs.len()],
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
        src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
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
            TrigOp::Sinh => f64::sinh,
            TrigOp::Cosh => f64::cosh,
            TrigOp::Tanh => f64::tanh,
            TrigOp::ASinh => f64::asinh,
            TrigOp::ACosh => f64::acosh,
            TrigOp::ATanh => f64::atanh,
            TrigOp::ATan2 => {
                //TODO: allow folding atan2
                return None;
            }
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
