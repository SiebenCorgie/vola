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
    attrib::FlagStore,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
};
use rvsdg_viewer::{Color, View};
use vola_ast::csg::{CSGConcept, CSGNodeDef};

use crate::{common::Ty, DialectNode, OptError, OptGraph, OptNode};

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

    fn try_derive_type(&self, mut sig: SmallVec<[Ty; 2]>) -> Result<Option<Ty>, OptError> {
        //NOTE: sig.len() is somewhat redundant, since the caller wouldn't try to derive if any input is
        // empty. However, its a good place to unify this check even if the type resolution changes at some point.

        match self {
            Self::Min | Self::Max => {
                //For those we need _the same algebraic_ for both inputs.
                //NOTE: Mul is a little _special_ since
                //      there is matrix multiplication with
                //      vectors and stuff
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

                if !sig[0].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {:?}",
                            self, sig[0]
                        ),
                    });
                }

                //Is okay, for these we return the _same_ type that we got
                Ok(Some(sig.remove(0)))
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

                Ok(Some(Ty::Scalar))
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
                Ok(Some(sig[0].clone()))
            }
            Self::Length => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("Length expects one operand, got {:?}", sig.len()),
                    });
                }
                match &sig[0] {
                    Ty::Vector { .. } => {}
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
                Ok(Some(Ty::Scalar))
            }

            Self::SquareRoot => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("SquareRoot expects one operand, got {:?}", sig.len()),
                    });
                }
                match &sig[0] {
                    Ty::Scalar => {}
                    _ => {
                        return Err(OptError::Any {
                            text: format!(
                                "SquareRoot expects operand of type scalar, got {:?}",
                                sig[0]
                            ),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(Ty::Scalar))
            }
            Self::Exp => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("Exp expects one operand, got {:?}", sig.len()),
                    });
                }
                match &sig[0] {
                    Ty::Scalar => {}
                    _ => {
                        return Err(OptError::Any {
                            text: format!("Exp expects operands of type scalar, got {:?}", sig[0]),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(Ty::Scalar))
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
                Ok(Some(sig[0].clone()))
            }
            Self::Mix | Self::Clamp => {
                if sig.len() != 3 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects three operand, got {:?}", self, sig.len()),
                    });
                }
                match (&sig[0], &sig[1], &sig[2]) {
                    (Ty::Scalar, Ty::Scalar, Ty::Scalar) => {},
                    (Ty::Vector{width: w0}, Ty::Vector{width: w1}, Ty::Vector{width: w2}) => {
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
                Ok(Some(sig[0].clone()))
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
        _typemap: &FlagStore<Ty>,
        graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
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

        self.op.try_derive_type(signature)
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
}
