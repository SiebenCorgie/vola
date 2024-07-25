/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! base arithmetic operations

use crate::{common::Ty, DialectNode, OptError, OptNode};
use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::SmallVec,
};
use rvsdg_viewer::{Color, View};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(LangNode)]
pub struct BinaryArith {
    pub op: BinaryArithOp,
    #[inputs]
    pub inputs: [Input; 2],
    #[output]
    pub output: Output,
}

impl View for BinaryArith {
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

impl BinaryArith {
    pub fn new(op: BinaryArithOp) -> Self {
        BinaryArith {
            op,
            inputs: [Input::default(), Input::default()],
            output: Output::default(),
        }
    }

    pub fn derive_type(&self, sig: [Ty; 2]) -> Result<Option<Ty>, OptError> {
        match &self.op {
            BinaryArithOp::Add | BinaryArithOp::Sub | BinaryArithOp::Div | BinaryArithOp::Mod => {
                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects the two operands to be of the same type. but got {:?} & {:?}",
                            self.op, sig[0], sig[1]
                        ),
                    });
                }

                if !sig[0].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {:?}",
                            self.op, sig[0]
                        ),
                    });
                }

                //Is okay, for these we return the _same_ type that we got
                Ok(Some(sig[0].clone()))
            }
            BinaryArithOp::Mul => {
                //Multiplication works on:
                // a x b, where a,b are of the same type,
                //        where a = Vec and b = Scalar
                //        where a = Mat and b = Scalar
                //        where a = Mat and b = Vec
                //        where a = Vec and b = Mat
                //        where a = Mat and b = Mat,

                //Still always two inputs one output
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {:?}", self.op, sig.len()),
                    });
                }

                if !sig[0].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {:?}",
                            self.op, sig[0]
                        ),
                    });
                }

                if !sig[1].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {:?}",
                            self.op, sig[1]
                        ),
                    });
                }

                match (&sig[0], &sig[1]) {
                    //In that case, use the vector as return type
                    (Ty::Vector { width: _ }, Ty::Scalar) => Ok(Some(sig[0].clone())),
                    //In that case, we return the same-sized
                    //matrix
                    (
                        Ty::Matrix {
                            width: _,
                            height: _,
                        },
                        Ty::Scalar,
                    ) => Ok(Some(sig[0].clone())),
                    (
                        Ty::Vector { width: vecwidth },
                        Ty::Matrix {
                            width: num_columns,
                            height: _,
                        },
                    ) => {
                        //NOTE: we can only do that, if the Vector::width
                        //      is the same has Matrix width.
                        //TODO: Decide if we really want to make that bound check already in the optimizer.
                        //      generally speaking this is more of a SPIR-V issue. Other backends could implement
                        //      other Matrix/Vector multiplication
                        if vecwidth != num_columns {
                            Err(OptError::Any { text: format!("Vector-Matrix multiplication expects the Matrix's \"width\" to be equal to Vector's \"width\". Matrix width = {num_columns} & vector_width = {vecwidth}") })
                        } else {
                            //Use the vector's type.
                            Ok(Some(sig[0].clone()))
                        }
                    }
                    (
                        Ty::Matrix {
                            width: num_columns,
                            height: _,
                        },
                        Ty::Vector { width: vecwidth },
                    ) => {
                        //Similar to above.
                        if num_columns != vecwidth {
                            Err(OptError::Any { text: format!("Vector-Matrix multiplication expects the Matrix's \"width\" to be equal to Vector's \"width\". Matrix width = {num_columns} & vector_width = {vecwidth}") })
                        } else {
                            //Use the vector's type.
                            Ok(Some(sig[1].clone()))
                        }
                    }
                    //if none of those was used, check that
                    // both are at least the same. Otherwise
                    //we actually need to return Err
                    (a, b) => {
                        if a != b {
                            Err(OptError::Any {
                                text: format!("{:?} expects the two operands, to be of the same type. but got {:?} & {:?}", self.op, sig[0], sig[1]),
                            })
                        } else {
                            //thats okay
                            Ok(Some(sig[0].clone()))
                        }
                    }
                }
            }
        }
    }
}

impl DialectNode for BinaryArith {
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
        //For all WKOps we first collect all inputs, then let the op check itself.
        // For now we already bail if any type is unset, since we currently don't have any ops that
        // _don't_ care about any input.
        let mut signature: SmallVec<[Ty; 2]> = SmallVec::new();
        for (idx, inp) in self.inputs.iter().enumerate() {
            if let Some(edg) = inp.edge {
                //resolve if there is a type set
                if let Some(t) = graph.edge(edg).ty.get_type() {
                    signature.insert(idx, t.clone());
                } else {
                    return Ok(None);
                }
            } else {
                //input not set atm. so return None as well
                return Ok(None);
            }
        }

        if signature.len() != 2 {
            return Err(OptError::TypeDeriveError {
                text: format!("Binary operation expects 2 inputs, had {}", signature.len()),
            });
        }

        let signature = [signature[0].clone(), signature[1].clone()];
        self.derive_type(signature)
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(BinaryArith {
                inputs: [Input::default(), Input::default()],
                output: Output::default(),
                op: self.op.clone(),
            }),
        }
    }

    fn is_operation_equal(&self, other: &crate::OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<BinaryArith>() {
            other_cop.op == self.op
        } else {
            false
        }
    }
}
