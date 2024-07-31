/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! Logical operations
use rvsdg::{
    nodes::NodeType,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
};
use rvsdg_viewer::View;
use vola_common::Span;

use crate::{common::Ty, imm::ImmBool, DialectNode, OptError, OptNode};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryBoolOp {
    Not,
}

#[derive(LangNode)]
pub struct UnaryBool {
    pub op: UnaryBoolOp,
    #[input]
    pub inputs: Input,
    #[output]
    pub output: Output,
}

impl UnaryBool {
    pub fn new(op: UnaryBoolOp) -> Self {
        UnaryBool {
            op,
            inputs: Input::default(),
            output: Output::default(),
        }
    }
}

impl View for UnaryBool {
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

impl DialectNode for UnaryBool {
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

        match &self.op {
            UnaryBoolOp::Not => {
                if !input_ty.is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operand (scalar, vector, matrix, tensor) got {:?}",
                            self.op, input_ty
                        ),
                    });
                }
                //seem allright, for neg, we return the _same_ datatype as we get
                Ok(Some(input_ty))
            }
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(UnaryBool {
                inputs: Input::default(),
                output: Output::default(),
                op: self.op.clone(),
            }),
        }
    }

    fn is_operation_equal(&self, other: &crate::OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<UnaryBool>() {
            other_cop.op == self.op
        } else {
            false
        }
    }

    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        if src_nodes.len() == 0 {
            return None;
        }

        if src_nodes[0].is_none() {
            return None;
        }

        if let NodeType::Simple(s) = &src_nodes[0].as_ref().unwrap().node_type {
            if let Some(bool) = s.try_downcast_ref::<ImmBool>() {
                Some(OptNode::new(ImmBool::new(!bool.lit), s.span.clone()))
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryBoolOp {
    And,
    Or,
}

#[derive(LangNode)]
pub struct BinaryBool {
    pub op: BinaryBoolOp,
    #[inputs]
    pub inputs: [Input; 2],
    #[output]
    pub output: Output,
}

impl BinaryBool {
    pub fn new(op: BinaryBoolOp) -> Self {
        BinaryBool {
            op,
            inputs: [Input::default(), Input::default()],
            output: Output::default(),
        }
    }
}

impl View for BinaryBool {
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

impl DialectNode for BinaryBool {
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
        let t0 = if let Some(edg) = &self.inputs[0].edge {
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
        let t1 = if let Some(edg) = &self.inputs[1].edge {
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

        //NOTE same test regardless if its & or |
        if t0 != t1 {
            return Err(OptError::Any {
                text: format!(
                    "{:?} expectes the same type for both operands, got {} & {}",
                    self.op, t0, t1
                ),
            });
        }

        match t0{
            Ty::Bool  =>  Ok(Some(Ty::Bool)),
            any => {
                Err(OptError::Any { text: format!("Cannot use comperator {:?} on {}. Consider breaking it down into a single bool value", self.op, any) })
            }
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(BinaryBool {
                inputs: [Input::default(), Input::default()],
                output: Output::default(),
                op: self.op.clone(),
            }),
        }
    }

    fn is_operation_equal(&self, other: &crate::OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<BinaryBool>() {
            other_cop.op == self.op
        } else {
            false
        }
    }

    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        if src_nodes.len() != 2 {
            return None;
        }

        if src_nodes[0].is_none() || src_nodes[1].is_none() {
            return None;
        }

        if let (NodeType::Simple(a), NodeType::Simple(b)) = (
            &src_nodes[0].as_ref().unwrap().node_type,
            &src_nodes[1].as_ref().unwrap().node_type,
        ) {
            if let (Some(bool_a), Some(bool_b)) = (
                a.try_downcast_ref::<ImmBool>(),
                b.try_downcast_ref::<ImmBool>(),
            ) {
                let op = match self.op {
                    BinaryBoolOp::Or => |a: bool, b: bool| a || b,
                    BinaryBoolOp::And => |a: bool, b: bool| a && b,
                };
                Some(OptNode::new(
                    ImmBool::new(op(bool_a.lit, bool_b.lit)),
                    Span::empty(),
                ))
            } else {
                None
            }
        } else {
            None
        }
    }
}
