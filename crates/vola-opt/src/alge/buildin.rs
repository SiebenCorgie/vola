/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! (Glsl) buildin operations

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

///Buildin optimizer ops that are _not-standard_.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuildinOp {
    //Call like ops we _know_
    Dot,
    Cross,
    Length,
    SquareRoot,
    Exp,
    Pow,
    Min,
    Max,
    Mix,
    Clamp,
}

impl BuildinOp {
    ///Returns the input-count for the op
    pub fn in_count(&self) -> usize {
        match self {
            Self::Dot => 2,
            Self::Cross => 2,
            Self::Length => 1,
            Self::SquareRoot => 1,
            Self::Exp => 1,
            Self::Pow => 2,
            Self::Min => 2,
            Self::Max => 2,
            Self::Mix => 3,
            Self::Clamp => 3,
        }
    }

    fn constant_handle_unary(&self, input: &OptNode) -> Option<OptNode> {
        match self {
            Self::Length => {
                //must be on vector
                if let Some(vec) = input.try_downcast_ref::<ImmVector>() {
                    let mut acc = 0.0;
                    //Not length is defined as all elements squared, then
                    // sqrt of acc. Aka. "euclidean distance".
                    for ele in &vec.lit {
                        acc += ele * ele;
                    }

                    Some(OptNode::new(ImmScalar::new(acc.sqrt()), Span::empty()))
                } else {
                    None
                }
            }
            Self::SquareRoot => {
                //only defined on Scalar
                if let Some(scalar) = input.try_downcast_ref::<ImmScalar>() {
                    Some(OptNode::new(
                        ImmScalar::new(scalar.lit.sqrt()),
                        Span::empty(),
                    ))
                } else {
                    None
                }
            }

            Self::Exp => {
                //only defined on Scalar
                if let Some(scalar) = input.try_downcast_ref::<ImmScalar>() {
                    Some(OptNode::new(
                        ImmScalar::new(scalar.lit.exp()),
                        Span::empty(),
                    ))
                } else {
                    None
                }
            }

            //All others are undefined / unfoldable
            _ => None,
        }
    }

    fn constant_handle_binary(&self, a: &OptNode, b: &OptNode) -> Option<OptNode> {
        match self {
            Self::Dot => {
                //only defined on vectors
                if let (Some(a), Some(b)) = (
                    a.try_downcast_ref::<ImmVector>(),
                    b.try_downcast_ref::<ImmVector>(),
                ) {
                    if a.lit.len() != b.lit.len() {
                        return None;
                    }
                    let mut acc = 0.0;

                    for i in 0..a.lit.len() {
                        acc += a.lit[i] * b.lit[i];
                    }

                    Some(OptNode::new(ImmScalar::new(acc), Span::empty()))
                } else {
                    None
                }
            }
            Self::Cross => {
                //Only defined for vectors
                if let (Some(a), Some(b)) = (
                    a.try_downcast_ref::<ImmVector>(),
                    b.try_downcast_ref::<ImmVector>(),
                ) {
                    if a.lit.len() != b.lit.len() {
                        return None;
                    }

                    //NOTE: Well, we need _generic_ determinant product and stuff
                    //      like that here. Sooo... do we want a generic lin-alg crate
                    //      at this point, or not?
                    #[cfg(feature = "log")]
                    log::error!("constant folding cross-product not implemented!");

                    None
                } else {
                    None
                }
            }
            Self::Min | Self::Max | Self::Pow => {
                //Fetch the correct f64 op, then apply it either pair wise,
                //or for the two scalars
                let op = match self {
                    Self::Min => f64::min,
                    Self::Max => f64::max,
                    Self::Pow => f64::powf,
                    _ => return None,
                };

                if let (Some(a), Some(b)) = (
                    a.try_downcast_ref::<ImmVector>(),
                    b.try_downcast_ref::<ImmVector>(),
                ) {
                    if a.lit.len() != b.lit.len() {
                        return None;
                    }
                    let mut new = SmallColl::new();

                    for i in 0..a.lit.len() {
                        new.push(op(a.lit[i], b.lit[i]));
                    }

                    return Some(OptNode::new(ImmVector::new(&new), Span::empty()));
                }

                if let (Some(a), Some(b)) = (
                    a.try_downcast_ref::<ImmScalar>(),
                    b.try_downcast_ref::<ImmScalar>(),
                ) {
                    return Some(OptNode::new(
                        ImmScalar::new(op(a.lit, b.lit)),
                        Span::empty(),
                    ));
                }

                None
            }
            _ => None,
        }
    }

    fn constant_handle_3ops(&self, a: &OptNode, b: &OptNode, c: &OptNode) -> Option<OptNode> {
        match self {
            Self::Mix | Self::Clamp => {
                let op = match self {
                    Self::Mix => |a: f64, b: f64, c: f64| {
                        let c = c.clamp(0.0, 1.0);
                        a * (1.0 - c) + b * c
                    },
                    Self::Clamp => f64::clamp,
                    _ => return None,
                };

                if let (Some(a), Some(b), Some(c)) = (
                    a.try_downcast_ref::<ImmVector>(),
                    b.try_downcast_ref::<ImmVector>(),
                    c.try_downcast_ref::<ImmVector>(),
                ) {
                    if a.lit.len() != b.lit.len() {
                        return None;
                    }
                    let mut new = SmallColl::new();

                    for i in 0..a.lit.len() {
                        new.push(op(a.lit[i], b.lit[i], c.lit[i]));
                    }

                    return Some(OptNode::new(ImmVector::new(&new), Span::empty()));
                }

                if let (Some(a), Some(b), Some(c)) = (
                    a.try_downcast_ref::<ImmScalar>(),
                    b.try_downcast_ref::<ImmScalar>(),
                    c.try_downcast_ref::<ImmScalar>(),
                ) {
                    return Some(OptNode::new(
                        ImmScalar::new(op(a.lit, b.lit, c.lit)),
                        Span::empty(),
                    ));
                }

                None
            }
            _ => None,
        }
    }

    fn try_derive_type(&self, sig: &[Ty]) -> Result<Ty, OptError> {
        //NOTE: sig.len() is somewhat redundant, since the caller wouldn't try to derive if any input is
        // empty. However, its a good place to unify this check even if the type resolution changes at some point.

        match self {
            Self::Min | Self::Max => {
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {:?}", self, sig.len()),
                    });
                }

                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!("{:?} expects the two operands to be of the same type. but got {:?} & {:?}", self, sig[0], sig[1]),
                    });
                }

                if !sig[0].is_algebraic() || sig[0].is_matrix() || sig[1].is_tensor() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {:?}",
                            self, sig[0]
                        ),
                    });
                }

                //Is okay, for these we return the _same_ type that we got
                Ok(sig[0].clone())
            }
            Self::Dot => {
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {:?}", self, sig.len()),
                    });
                }

                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!("{:?} expects the two operands to be of the same type. but got {:?} & {:?}", self, sig[0], sig[1]),
                    });
                }

                if !sig[0].is_vector() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects operands to be vectors, got {:?}",
                            self, sig[0]
                        ),
                    });
                }

                Ok(Ty::SCALAR_REAL)
            }
            Self::Cross => {
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {:?}", self, sig.len()),
                    });
                }

                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!("{:?} expects the two operands to be of the same type. but got {:?} & {:?}", self, sig[0], sig[1]),
                    });
                }

                if !sig[0].is_vector() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects operands to be vectors, got {:?}",
                            self, sig[0]
                        ),
                    });
                }

                //Cross returns the same vector type as supplied
                Ok(sig[0].clone())
            }
            Self::Length => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("Length expects one operand, got {:?}", sig.len()),
                    });
                }
                match &sig[0] {
                    Ty::Shaped {
                        ty: DataType::Real,
                        shape: Shape::Vec { width: _ },
                    } => {}
                    _ => {
                        return Err(OptError::Any {
                            text: format!(
                                "Length expects operand of type vector, got {:?}",
                                sig[0]
                            ),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Ty::SCALAR_REAL)
            }

            Self::SquareRoot => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("SquareRoot expects one operand, got {:?}", sig.len()),
                    });
                }
                match &sig[0] {
                    &Ty::SCALAR_REAL => Ok(Ty::SCALAR_REAL),
                    Ty::Shaped {
                        ty: DataType::Real,
                        shape: Shape::Vec { width },
                    } => Ok(Ty::vector_type(DataType::Real, *width)),
                    _ => {
                        return Err(OptError::Any {
                            text: format!(
                                "SquareRoot expects operand of type scalar, or vector got {:?}",
                                sig[0]
                            ),
                        })
                    }
                }
            }
            Self::Exp => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("Exp expects one operand, got {:?}", sig.len()),
                    });
                }
                match &sig[0] {
                    &Ty::SCALAR_REAL => Ok(Ty::SCALAR_REAL),
                    Ty::Shaped {
                        ty: DataType::Real,
                        shape: Shape::Vec { width },
                    } => Ok(Ty::vector_type(DataType::Real, *width)),
                    _ => {
                        return Err(OptError::Any {
                            text: format!("Exp expects operands of type scalar, got {:?}", sig[0]),
                        })
                    }
                }
            }
            Self::Pow => {
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {:?}", self, sig.len()),
                    });
                }

                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!("{:?} expects the two operands to be of the same type. but got {:?} & {:?}", self, sig[0], sig[1]),
                    });
                }

                if !sig[0].is_scalar() && !sig[0].is_vector() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects operands to be vectors or scalars, got {:?}",
                            self, sig[0]
                        ),
                    });
                }

                //Cross returns the same vector type as supplied
                Ok(sig[0].clone())
            }
            Self::Mix | Self::Clamp => {
                if sig.len() != 3 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects three operand, got {:?}", self, sig.len()),
                    });
                }
                match (&sig[0], &sig[1], &sig[2]) {
                    (&Ty::SCALAR_REAL, &Ty::SCALAR_REAL, &Ty::SCALAR_REAL) => {},
                    (
                        Ty::Shaped{ty: DataType::Real, shape: Shape::Vec{width: w0}},
                        Ty::Shaped{ty: DataType::Real, shape: Shape::Vec{width: w1}},
                        Ty::Shaped{ty: DataType::Real, shape: Shape::Vec{width: w2}}
                    ) => {
                        if w0 != w1 || w0 != w2{
                            return Err(OptError::Any {
                                text: format!("{:?} expects operands of type scalar or vector (of equal width), got {:?}", self, sig),
                            });
                        }
                    },
                    _ => {
                        return Err(OptError::Any {
                            text: format!("{:?} expects operands of type scalar or vector (of equal width), got {:?}", self, sig),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(sig[0].clone())
            }
        }
    }
}

///In the algebraic dialect all operations are unified into a Call-Like op.
///
/// Think of it like prefix notation. So the expression `a + b` becomes `add(a, b)`, `-a` becomes `neg(a)` etc.
///
/// this'll make optimizing easier later on.
///
/// Note that always only one result is returned. So only site-effect free ops can be modeled by this node.
#[derive(LangNode)]
pub struct Buildin {
    #[inputs]
    pub inputs: SmallVec<[Input; 2]>,
    #[output]
    pub output: Output,

    pub op: BuildinOp,
}

impl Buildin {
    pub fn new(op: BuildinOp) -> Self {
        let mut op = Buildin {
            inputs: SmallVec::new(),
            output: Output::default(),
            op,
        };
        //configure input count
        for _ in 0..op.op.in_count() {
            op.inputs.push(Input::default())
        }
        op
    }
}

impl View for Buildin {
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

impl DialectNode for Buildin {
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
        self.op.try_derive_type(input_types)
    }

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<Buildin>() {
            other_cop.op == self.op
        } else {
            false
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(Buildin {
                inputs: smallvec![Input::default(); self.inputs.len()],
                output: Output::default(),
                op: self.op.clone(),
            }),
        }
    }

    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        match self.op {
            BuildinOp::Length | BuildinOp::SquareRoot | BuildinOp::Exp => {
                //unary case
                if src_nodes.len() == 0 {
                    return None;
                }
                if src_nodes[0].is_none() {
                    return None;
                }
                if !src_nodes[0].as_ref().unwrap().node_type.is_simple() {
                    return None;
                }

                self.op.constant_handle_unary(
                    src_nodes[0].as_ref().unwrap().node_type.unwrap_simple_ref(),
                )
            }
            BuildinOp::Dot
            | BuildinOp::Cross
            | BuildinOp::Pow
            | BuildinOp::Min
            | BuildinOp::Max => {
                //Binary case
                if src_nodes.len() < 2 {
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

                self.op.constant_handle_binary(
                    src_nodes[0].as_ref().unwrap().node_type.unwrap_simple_ref(),
                    src_nodes[1].as_ref().unwrap().node_type.unwrap_simple_ref(),
                )
            }
            BuildinOp::Mix | BuildinOp::Clamp => {
                //3-component case
                if src_nodes.len() < 3 {
                    return None;
                }
                if src_nodes[0].is_none() || src_nodes[1].is_none() || src_nodes[2].is_none() {
                    return None;
                }
                if !src_nodes[0].as_ref().unwrap().node_type.is_simple()
                    || !src_nodes[1].as_ref().unwrap().node_type.is_simple()
                    || !src_nodes[2].as_ref().unwrap().node_type.is_simple()
                {
                    return None;
                }

                self.op.constant_handle_3ops(
                    src_nodes[0].as_ref().unwrap().node_type.unwrap_simple_ref(),
                    src_nodes[1].as_ref().unwrap().node_type.unwrap_simple_ref(),
                    src_nodes[2].as_ref().unwrap().node_type.unwrap_simple_ref(),
                )
            }
        }
    }
}
