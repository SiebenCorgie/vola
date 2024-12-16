/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # CSG Dialect

use crate::{
    common::{DataType, Ty},
    error::OptError,
    DialectNode, OptNode,
};
use ahash::AHashMap;
use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
};
use rvsdg_viewer::Color;
use vola_ast::{
    common::Ident,
    csg::{CSGConcept, CsgDef},
};

//Macro that implements the "View" trait for an AlgeDialect op
macro_rules! implViewCsgOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> Color {
                Color::from_rgba(170, 200, 170, 255)
            }

            fn name(&self) -> String {
                format!($str, $(self.$arg)*,)
            }

            fn stroke(&self) -> rvsdg_viewer::Stroke {
                rvsdg_viewer::Stroke::Line
            }
        }
    };
    ($opname:ident, $str:expr) =>{
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> Color {
                Color::from_rgba(170, 200, 170, 255)
            }

            fn name(&self) -> String {
                $str.to_owned()
            }

            fn stroke(&self) -> rvsdg_viewer::Stroke {
                rvsdg_viewer::Stroke::Line
            }
        }
    }
}

///Highlevel CSG Op where the concept identifier is verified, but not yet specialized.
/// Used to build resolved.
#[derive(LangNode, Debug)]
pub struct CsgOp {
    ///The operation or entity that is being called.
    pub op: String,

    pub subtree_count: usize,
    #[inputs]
    pub inputs: SmallVec<[Input; 2]>,
    #[output]
    pub output: Output,
}

impl CsgOp {
    pub fn new(op: Ident, subtree_count: usize, parameter_count: usize) -> Self {
        CsgOp {
            op: op.0,
            subtree_count,
            inputs: smallvec![Input::default(); parameter_count],
            output: Output::default(),
        }
    }
}

implViewCsgOp!(CsgOp, "{:?}", op);
impl DialectNode for CsgOp {
    fn dialect(&self) -> &'static str {
        "csg"
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(CsgOp {
                op: self.op.clone(),
                subtree_count: self.subtree_count,
                inputs: smallvec![Input::default(); self.inputs.len()],
                output: Output::default(),
            }),
        }
    }

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        //NOTE: Two construct nodes are always equal
        if let Some(other_cop) = other.try_downcast_ref::<CsgOp>() {
            other_cop.op == self.op && other_cop.subtree_count == self.subtree_count
        } else {
            false
        }
    }

    fn try_derive_type(
        &self,
        inputs: &[Ty],
        _concepts: &AHashMap<String, CSGConcept>,
        csg_defs: &AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //We resole the CSG op by checking, that all inputs adher to the op's specification.
        // Which means the arguments that are connected are equal to the one specified by the
        // implemented operation or entity
        let expected_signature = csg_defs
            .get(&self.op)
            .unwrap()
            .args
            .iter()
            .map(|arg| {
                arg.ty
                    .clone()
                    .try_into()
                    .expect("Could not convert ty opt-type")
            })
            .collect::<SmallVec<[Ty; 3]>>();

        if inputs.len() != expected_signature.len() + self.subtree_count {
            panic!(
                "Unexpected argcount: {} != {}",
                expected_signature.len() + self.subtree_count,
                inputs.len()
            );
        }

        //we always output a _CSGTree_ component.
        let output: Ty = Ty::scalar_type(DataType::Csg);

        //In practice we now iterate all connected inputs. The first 0..n migth be CSGTrees
        // already, which are our sub_trees. We verify those against the `subtree_count`.
        // All following connected nodes must be part of the `expected_signature`.
        for i in 0..self.subtree_count {
            //should be CSG typed
            if inputs[i] != Ty::CSG {
                return Err(OptError::Any {
                    text: format!("Subtree {i} was not of type CSGTree for CSGOp {}", self.op),
                });
            }
        }

        for (idx, alge_arg_ty) in inputs[self.subtree_count..].iter().enumerate() {
            if alge_arg_ty != &expected_signature[idx] {
                return Err(OptError::Any {
                    text: format!(
                        "expected {idx}-th argument to be {:?} not {:?} for CSGOp {}",
                        expected_signature[idx], alge_arg_ty, self.op
                    ),
                });
            }
        }

        Ok(output)
    }
}
