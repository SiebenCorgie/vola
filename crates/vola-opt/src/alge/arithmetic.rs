/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! base arithmetic operations

use std::ops::Neg;

use crate::{
    common::{DataType, Shape, Ty},
    imm::{ImmMatrix, ImmScalar, ImmVector},
    DialectNode, OptError, OptNode,
};
use ahash::AHashMap;
use rvsdg::{
    nodes::NodeType,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    SmallColl,
};
use rvsdg_viewer::{Color, View};
use vola_ast::csg::{CSGConcept, CsgDef};
use vola_common::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinaryArithOp {
    pub fn apply_on_scalar(&self, a: f64, b: f64) -> f64 {
        match self {
            Self::Add => a + b,
            Self::Sub => a - b,
            Self::Mul => a * b,
            Self::Div => a / b,
            Self::Mod => a % b,
        }
    }
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

    pub fn derive_type(&self, sig: [Ty; 2]) -> Result<Ty, OptError> {
        match &self.op {
            BinaryArithOp::Add | BinaryArithOp::Sub | BinaryArithOp::Div | BinaryArithOp::Mod => {
                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects the two operands to be of the same type. but got {} & {}",
                            self.op, sig[0], sig[1]
                        ),
                    });
                }

                if !sig[0].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {}",
                            self.op, sig[0]
                        ),
                    });
                }

                //Is okay, for these we return the _same_ type that we got
                Ok(sig[0].clone())
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
                        text: format!("{:?} expects two operands, got {}", self.op, sig.len()),
                    });
                }

                if !sig[0].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {}",
                            self.op, sig[0]
                        ),
                    });
                }

                if !sig[1].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {}",
                            self.op, sig[1]
                        ),
                    });
                }

                match (&sig[0], &sig[1]) {
                    //In that case, use the vector as return type
                    (
                        Ty::Shaped {
                            ty: DataType::Real,
                            shape: Shape::Vec { width: _ },
                        },
                        &Ty::SCALAR_REAL,
                    ) => Ok(sig[0].clone()),
                    //In that case, we return the same-sized
                    //matrix
                    (
                        Ty::Shaped {
                            ty: DataType::Real,
                            shape: Shape::Matrix { .. },
                        },
                        &Ty::SCALAR_REAL,
                    ) => Ok(sig[0].clone()),
                    (
                        Ty::Shaped {
                            ty: DataType::Real,
                            shape: Shape::Vec { width: vecwidth },
                        },
                        Ty::Shaped {
                            ty: DataType::Real,
                            shape:
                                Shape::Matrix {
                                    width: num_columns, ..
                                },
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
                            Ok(sig[0].clone())
                        }
                    }
                    (
                        Ty::Shaped {
                            ty: DataType::Real,
                            shape:
                                Shape::Matrix {
                                    width: num_columns, ..
                                },
                        },
                        Ty::Shaped {
                            ty: DataType::Real,
                            shape: Shape::Vec { width: vecwidth },
                        },
                    ) => {
                        //Similar to above.
                        if num_columns != vecwidth {
                            Err(OptError::Any { text: format!("Matrix-Vector multiplication expects the Matrix's \"width\" to be equal to Vector's \"width\". Matrix width = {num_columns} & vector_width = {vecwidth}") })
                        } else {
                            //Use the vector's type.
                            Ok(sig[1].clone())
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
                            Ok(sig[0].clone())
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
        input_types: &[Ty],
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //For all WKOps we first collect all inputs, then let the op check itself.
        // For now we already bail if any type is unset, since we currently don't have any ops that
        // _don't_ care about any input.

        if input_types.len() != 2 {
            return Err(OptError::TypeDeriveError {
                text: format!(
                    "Binary operation expects 2 inputs, had {}",
                    input_types.len()
                ),
            });
        }

        let signature = [input_types[0].clone(), input_types[1].clone()];
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

    fn try_constant_fold(
        &self,
        src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        if src_nodes.len() != 2 {
            return None;
        }
        if src_nodes[0].is_none() || src_nodes[1].is_none() {
            return None;
        }

        //similar to the unary case, but for two inputs. We try to cast the first one to ImmScalar/Vector/Matrix,
        //and then try to do it with the second one as well.
        //
        //an exception is `Mul`, which can use all kinds of operand combinations as is no necessarly
        match (
            src_nodes[0].map(|n| &n.node_type),
            src_nodes[1].map(|n| &n.node_type),
        ) {
            (Some(NodeType::Simple(a)), Some(NodeType::Simple(b))) => match self.op {
                BinaryArithOp::Mul => crate::alge::constant_fold::handle_constant_mul(a, b),
                _ => crate::alge::constant_fold::handle_piece_wise(&self.op, a, b),
            },
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryArithOp {
    Neg,
    Abs,
    Fract,
    Ceil,
    Floor,
    Round,
}

impl UnaryArithOp {
    pub fn apply_on_scalar(&self, s: f64) -> f64 {
        match self {
            Self::Abs => s.abs(),
            Self::Ceil => s.ceil(),
            Self::Floor => s.floor(),
            Self::Fract => s.fract(),
            Self::Round => s.round(),
            Self::Neg => s.neg(),
        }
    }
}

#[derive(LangNode)]
pub struct UnaryArith {
    pub op: UnaryArithOp,
    #[input]
    pub inputs: Input,
    #[output]
    pub output: Output,
}

impl UnaryArith {
    pub fn new(op: UnaryArithOp) -> Self {
        UnaryArith {
            op,
            inputs: Input::default(),
            output: Output::default(),
        }
    }
}

impl View for UnaryArith {
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

impl DialectNode for UnaryArith {
    fn dialect(&self) -> &'static str {
        "alge"
    }

    fn try_derive_type(
        &self,
        input_types: &[Ty],
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        assert_eq!(input_types.len(), 1);
        let input_ty = input_types[0].clone();

        match &self.op {
            UnaryArithOp::Neg => {
                if !input_ty.is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operand (scalar, vector, matrix, tensor) got {:?}",
                            self.op,input_ty
                        ),
                    });
                }
                //seem allright, for neg, we return the _same_ datatype as we get
                Ok(input_ty)
            }
            UnaryArithOp::Abs
            | UnaryArithOp::Fract
            | UnaryArithOp::Round
            | UnaryArithOp::Ceil
            | UnaryArithOp::Floor => {
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
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(UnaryArith {
                inputs: Input::default(),
                output: Output::default(),
                op: self.op.clone(),
            }),
        }
    }

    fn is_operation_equal(&self, other: &crate::OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<UnaryArith>() {
            other_cop.op == self.op
        } else {
            false
        }
    }

    fn try_constant_fold(
        &self,
        src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        if src_nodes.len() != 1 {
            return None;
        }
        if src_nodes[0].is_none() {
            return None;
        }

        //Try to cast the first src node into any of the known imm values, if successfull, try to apply the operation
        //to the value, and build a new, constant immediate node
        if let Some(NodeType::Simple(s)) = &src_nodes[0].map(|n| &n.node_type) {
            if let Some(scalar) = s.try_downcast_ref::<ImmScalar>() {
                let new = self.op.apply_on_scalar(scalar.lit);
                return Some(OptNode::new(ImmScalar::new(new), Span::empty()));
            }

            if let Some(vector) = s.try_downcast_ref::<ImmVector>() {
                let mut new_vec = ImmVector::new(&[]);
                for element in &vector.lit {
                    new_vec.lit.push(self.op.apply_on_scalar(*element));
                }

                return Some(OptNode::new(new_vec, Span::empty()));
            }

            if let Some(matrix) = s.try_downcast_ref::<ImmMatrix>() {
                let mut columns = SmallColl::new();
                for col in &matrix.lit {
                    let mut newcol = SmallColl::new();
                    for element in col {
                        newcol.push(self.op.apply_on_scalar(*element));
                    }
                    columns.push(newcol);
                }

                return Some(OptNode::new(ImmMatrix::new(columns), Span::empty()));
            }

            None
        } else {
            None
        }
    }
}
