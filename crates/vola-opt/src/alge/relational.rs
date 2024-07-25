/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! relational operations
use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
};
use rvsdg_viewer::View;

use crate::{common::Ty, DialectNode, OptError, OptNode};

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

        match &t0{
            Ty::Nat | Ty::Scalar  =>  Ok(Some(Ty::Bool)),
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
}
