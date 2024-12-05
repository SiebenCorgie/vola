/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Matrix operations

use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
};
use rvsdg_viewer::View;

use crate::{
    common::{DataType, Shape, Ty},
    DialectNode, OptError, OptNode,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryMatrixOp {
    Invert,
}

#[derive(LangNode)]
pub struct UnaryMatrix {
    pub op: UnaryMatrixOp,
    #[input]
    pub inputs: Input,
    #[output]
    pub output: Output,
}

impl UnaryMatrix {
    pub fn new(op: UnaryMatrixOp) -> Self {
        UnaryMatrix {
            op,
            inputs: Input::default(),
            output: Output::default(),
        }
    }
}

impl View for UnaryMatrix {
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

impl DialectNode for UnaryMatrix {
    fn dialect(&self) -> &'static str {
        "alge"
    }
    fn try_derive_type(
        &self,
        _typemap: &rvsdg::attrib::FlagStore<Ty>,
        graph: &crate::OptGraph,
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CSGConcept>,
        _csg_defs: &ahash::AHashMap<String, vola_ast::csg::CsgDef>,
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

        match &self.op {
            UnaryMatrixOp::Invert => {
                match &input_ty {
                    Ty::Shaped {
                        ty: DataType::Real,
                        shape: Shape::Matrix { width, height },
                    } => {
                        if width != height {
                            return Err(OptError::Any { text: format!("Inverse operation expects quadratic matrix, got one with width={width} & height={height}") });
                        }
                    }
                    _ => {
                        return Err(OptError::Any {
                            text: format!(
                                "{:?} expects operands of type matrix, got {:?}",
                                self.op, input_ty
                            ),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(input_ty))
            }
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(UnaryMatrix {
                inputs: Input::default(),
                output: Output::default(),
                op: self.op.clone(),
            }),
        }
    }

    fn is_operation_equal(&self, other: &crate::OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<UnaryMatrix>() {
            other_cop.op == self.op
        } else {
            false
        }
    }
}
