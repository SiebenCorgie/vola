/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # Alge dialect
//!

use ahash::AHashMap;
use rvsdg::{
    attrib::FlagStore,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
    SmallColl,
};
use rvsdg_viewer::Color;
use vola_ast::csg::{CSGConcept, CSGNodeDef};

use crate::{common::Ty, error::OptError, DialectNode, OptEdge, OptGraph, OptNode, TypeState};

pub(crate) mod algefn;
pub(crate) mod implblock;

///Well known ops for the optimizer. Includes all _BinaryOp_ of the Ast, as well as
/// some well known _function_like_ ops in the SPIRV spec. For instance
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WkOp {
    //WK unary ops
    //TODO: do we really want to include NOT? I mean flipping floats is fun I guess,
    // but also error prone.
    Not,
    Neg,

    //WK _standard_ binary ops
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    //relation
    Lt,
    Gt,
    Lte,
    Gte,
    Eq,
    NotEq,

    //logical
    And,
    Or,

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

    Abs,
    Fract,
    Round,
    Ceil,
    Floor,
    Sin,
    Cos,
    Tan,
    ASin,
    ACos,
    ATan,
    //TODO: basically implement the rest of the glsl-ext-inst, cause people might be used to those.
    Inverse,
}

impl WkOp {
    ///Returns the input-count for the op
    fn in_count(&self) -> usize {
        match self {
            WkOp::Not => 1,
            WkOp::Neg => 1,

            WkOp::Add => 2,
            WkOp::Sub => 2,
            WkOp::Mul => 2,
            WkOp::Div => 2,
            WkOp::Mod => 2,

            WkOp::Lt => 2,
            WkOp::Gt => 2,
            WkOp::Lte => 2,
            WkOp::Gte => 2,
            WkOp::Eq => 2,
            WkOp::NotEq => 2,

            WkOp::And => 2,
            WkOp::Or => 2,

            WkOp::Dot => 2,
            WkOp::Cross => 2,
            WkOp::Length => 1,
            WkOp::SquareRoot => 1,
            WkOp::Exp => 1,
            WkOp::Pow => 2,
            WkOp::Min => 2,
            WkOp::Max => 2,
            WkOp::Mix => 3,
            WkOp::Clamp => 3,
            WkOp::Round => 1,

            WkOp::Abs => 1,
            WkOp::Fract => 1,
            WkOp::Ceil => 1,
            WkOp::Floor => 1,

            WkOp::Sin => 1,
            WkOp::Cos => 1,
            WkOp::Tan => 1,
            WkOp::ASin => 1,
            WkOp::ACos => 1,
            WkOp::ATan => 1,

            WkOp::Inverse => 1,
        }
    }

    pub fn try_parse(s: &str) -> Option<Self> {
        match s {
            "dot" => Some(Self::Dot),
            "cross" => Some(Self::Cross),
            "length" => Some(Self::Length),
            "sqrt" => Some(Self::SquareRoot),
            "exp" => Some(Self::Exp),
            "pow" => Some(Self::Pow),
            "min" => Some(Self::Min),
            "max" => Some(Self::Max),
            "mix" | "lerp" => Some(Self::Mix),
            "mod" => Some(Self::Mod),
            "clamp" => Some(Self::Clamp),
            "fract" => Some(Self::Fract),
            "abs" => Some(Self::Abs),
            "round" => Some(Self::Round),
            "ceil" => Some(Self::Ceil),
            "floor" => Some(Self::Floor),
            "sin" => Some(Self::Sin),
            "cos" => Some(Self::Cos),
            "tan" => Some(Self::Tan),
            "asin" => Some(Self::ASin),
            "acos" => Some(Self::ACos),
            "atan" => Some(Self::ATan),
            "inverse" | "invert" => Some(Self::Inverse),
            _ => None,
        }
    }

    fn try_derive_type(&self, mut sig: SmallVec<[Ty; 2]>) -> Result<Option<Ty>, OptError> {
        //NOTE: sig.len() is somewhat redundant, since the caller wouldn't try to derive if any input is
        // empty. However, its a good place to unify this check even if the type resolution changes at some point.

        match self {
            WkOp::Not | WkOp::Neg => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects one operand, got {:?}", self, sig.len()),
                    });
                }
                if !sig[0].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operand (scalar, vector, matrix, tensor) got {:?}",
                            self, sig[0]
                        ),
                    });
                }
                //seem allright, for neg, we return the _same_ datatype as we get
                Ok(Some(sig.remove(0)))
            }

            WkOp::Add | WkOp::Sub | WkOp::Div | WkOp::Min | WkOp::Max | WkOp::Mod => {
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
            WkOp::Mul => {
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
                        text: format!("{:?} expects two operands, got {:?}", self, sig.len()),
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

                if !sig[1].is_algebraic() {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expects algebraic operands (scalar, vector, matrix, tensor) got {:?}",
                            self, sig[1]
                        ),
                    });
                }

                match (&sig[0], &sig[1]) {
                    //In that case, use the vector as return type
                    (Ty::Vector { width: _ }, Ty::Scalar) => Ok(Some(sig.remove(0))),
                    //In that case, we return the same-sized
                    //matrix
                    (
                        Ty::Matrix {
                            width: _,
                            height: _,
                        },
                        Ty::Scalar,
                    ) => Ok(Some(sig.remove(0))),
                    (Ty::Vector { width }, Ty::Matrix { width: _t, height }) => {
                        //NOTE: we can only do that, if the Vector::width
                        //      is the same has Matrix height.
                        //TODO: Decide if we really want to make that bound check already in the optimizer.
                        //      generally speaking this is more of a SPIR-V issue. Other backends could implement
                        //      other Matrix/Vector multiplication
                        if width != height {
                            Err(OptError::Any { text: format!("Matrix-Vector multiplication expects the Matrix's \"height\" to be the same as the Vector's \"width\". Got h={height} & w={width} instead.") })
                        } else {
                            //Use the vector's type.
                            Ok(Some(sig.remove(0)))
                        }
                    }
                    (Ty::Matrix { width: _t, height }, Ty::Vector { width }) => {
                        //Similar to above.
                        if width != height {
                            Err(OptError::Any { text: format!("Vector- Matrix multiplication expects the Matrix's \"height\" to be the same as the Vector's \"width\". Got h={height} & w={width} instead.") })
                        } else {
                            //Use the vector's type.
                            Ok(Some(sig.remove(1)))
                        }
                    }
                    //if none of those was used, check that
                    // both are at least the same. Otherwise
                    //we actually need to return Err
                    (a, b) => {
                        if a != b {
                            Err(OptError::Any {
                                text: format!("{:?} expects the two operands, to be of the same type. but got {:?} & {:?}", self, sig[0], sig[1]),
                            })
                        } else {
                            //thats okay
                            Ok(Some(sig.remove(0)))
                        }
                    }
                }
            }
            WkOp::Dot => {
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
            WkOp::Cross => {
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
            WkOp::Length => {
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

            WkOp::SquareRoot => {
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
            WkOp::Exp => {
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
            WkOp::Pow => {
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
            WkOp::Mix | WkOp::Clamp => {
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

            WkOp::Abs
            | WkOp::Fract
            | WkOp::Round
            | WkOp::Ceil
            | WkOp::Floor
            | WkOp::Sin
            | WkOp::Cos
            | WkOp::Tan
            | WkOp::ASin
            | WkOp::ACos
            | WkOp::ATan => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects one operand, got {:?}", self, sig.len()),
                    });
                }
                match &sig[0] {
                    Ty::Scalar => {}
                    Ty::Vector { .. } => {}
                    _ => {
                        return Err(OptError::Any {
                            text: format!(
                                "{:?} expects operands of type scalar or vector, got {:?}",
                                self, sig
                            ),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(sig[0].clone()))
            }

            WkOp::Inverse => {
                if sig.len() != 1 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects one operand, got {:?}", self, sig.len()),
                    });
                }
                match &sig[0] {
                    Ty::Matrix { width, height } => {
                        if width != height {
                            return Err(OptError::Any { text: format!("Inverse operation expects quadratic matrix, got one with width={width} & height={height}") });
                        }
                    }
                    _ => {
                        return Err(OptError::Any {
                            text: format!(
                                "{:?} expects operands of type matrix, got {:?}",
                                self, sig
                            ),
                        })
                    }
                }

                //seems to be alright, return scalar
                Ok(Some(sig[0].clone()))
            }
            WkOp::Lt | WkOp::Gt | WkOp::Lte | WkOp::Gte | WkOp::Eq | WkOp::NotEq => {
                //Right now we only allow those on scalars / nats, othewise we'd need a way to represent vecs
                //of bools etc. Which we don't (yet, ever?).
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {}", self, sig.len()),
                    });
                }

                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expectes the same type for both operands, got {} & {}",
                            self, sig[0], sig[1]
                        ),
                    });
                }

                match &sig[0]{
                    Ty::Nat | Ty::Scalar  =>  Ok(Some(Ty::Bool)),
                    any => {
                        Err(OptError::Any { text: format!("Cannot use comperator {:?} on {}. Consider breaking it down to either a simple scalar or natural value", self, any) })
                    }
                }
            }
            WkOp::And | WkOp::Or => {
                //Right now we only allow those on single bools, othewise we'd need a way to represent vecs
                //of bools etc. Which we don't (yet, ever?).
                if sig.len() != 2 {
                    return Err(OptError::Any {
                        text: format!("{:?} expects two operands, got {}", self, sig.len()),
                    });
                }

                if sig[0] != sig[1] {
                    return Err(OptError::Any {
                        text: format!(
                            "{:?} expectes the same type for both operands, got {} & {}",
                            self, sig[0], sig[1]
                        ),
                    });
                }

                match &sig[0]{
                    Ty::Bool  =>  Ok(Some(Ty::Bool)),
                    any => {
                        Err(OptError::Any { text: format!("Cannot use comperator {:?} on {}. Consider breaking it down into a single bool value", self, any) })
                    }
                }
            } // wk => Err(OptError::Any {
              //     text: format!("derive not implemented for {:?}", wk),
              // }),
        }
    }
}

impl From<vola_ast::alge::UnaryOp> for WkOp {
    fn from(value: vola_ast::alge::UnaryOp) -> Self {
        match value {
            vola_ast::alge::UnaryOp::Neg => Self::Neg,
            vola_ast::alge::UnaryOp::Not => Self::Not,
        }
    }
}

impl From<vola_ast::alge::BinaryOp> for WkOp {
    fn from(value: vola_ast::alge::BinaryOp) -> Self {
        match value {
            vola_ast::alge::BinaryOp::Add => Self::Add,
            vola_ast::alge::BinaryOp::Sub => Self::Sub,
            vola_ast::alge::BinaryOp::Mul => Self::Mul,
            vola_ast::alge::BinaryOp::Div => Self::Div,
            vola_ast::alge::BinaryOp::Mod => Self::Mod,

            vola_ast::alge::BinaryOp::Lt => Self::Lt,
            vola_ast::alge::BinaryOp::Gt => Self::Gt,
            vola_ast::alge::BinaryOp::Gte => Self::Gte,
            vola_ast::alge::BinaryOp::Lte => Self::Lte,
            vola_ast::alge::BinaryOp::Eq => Self::Eq,
            vola_ast::alge::BinaryOp::NotEq => Self::NotEq,

            vola_ast::alge::BinaryOp::Or => Self::Or,
            vola_ast::alge::BinaryOp::And => Self::And,
        }
    }
}

//Macro that implements the "View" trait for an AlgeDialect op
macro_rules! implViewAlgeOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> Color {
                Color::from_rgba(200, 170, 170, 255)
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
                Color::from_rgba(200, 170, 170, 255)
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

///In the algebraic dialect all operations are unified into a Call-Like op.
///
/// Think of it like prefix notation. So the expression `a + b` becomes `add(a, b)`, `-a` becomes `neg(a)` etc.
///
/// this'll make optimizing easier later on.
///
/// Note that always only one result is returned. So only site-effect free ops can be modeled by this node.
#[derive(LangNode)]
pub struct CallOp {
    #[inputs]
    pub inputs: SmallVec<[Input; 2]>,
    #[output]
    pub output: Output,

    pub op: WkOp,
}

impl CallOp {
    pub fn new(op: WkOp) -> Self {
        let mut op = CallOp {
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

implViewAlgeOp!(CallOp, "{:?}", op);
impl DialectNode for CallOp {
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

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode {
            span,
            node: Box::new(CallOp {
                inputs: smallvec![Input::default(); self.inputs.len()],
                output: Output::default(),
                op: self.op.clone(),
            }),
        }
    }
}

///The Eval node in itself is a call-site to some connected λ-node, when specialised.
///
/// By definition the first argument is the callable that will be called, and all following ports are arguments
/// to that call.
#[derive(LangNode, Debug)]
pub struct EvalNode {
    ///The concept that is being called.
    pub called_concept: String,
    #[inputs]
    pub inputs: SmallVec<[Input; 3]>,
    ///The eval node itsel has only one output, the state that is produced by the called concept.
    #[output]
    pub out: Output,
}

impl EvalNode {
    ///Returns the concept name that is called by this node.
    pub fn concept(&self) -> &String {
        &self.called_concept
    }
    pub fn new(argount: usize, concept: String) -> Self {
        EvalNode {
            called_concept: concept,
            inputs: smallvec![Input::default(); argount + 1],
            out: Output::default(),
        }
    }
}

implViewAlgeOp!(EvalNode, "Eval");
impl DialectNode for EvalNode {
    fn dialect(&self) -> &'static str {
        "alge"
    }
    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(EvalNode {
                called_concept: self.called_concept.clone(),
                inputs: smallvec![Input::default(); self.inputs.len()],
                out: Output::default(),
            }),
        }
    }

    fn try_derive_type(
        &self,
        _typemap: &FlagStore<Ty>,
        graph: &OptGraph,
        concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //For eval nodes, the first type must be a callable, and all following must addher to the called concept's definition
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

        if signature.len() == 0 {
            return Err(OptError::Any {
                text: format!("Eval must have at least a callable input, had none at all."),
            });
        }

        //Make sure that the concept that is being called exists at all
        if !concepts.get(self.concept()).is_some() {
            return Err(OptError::Any {
                text: format!(
                    "the concept {} which is called here is not defined!",
                    self.concept()
                ),
            });
        }

        //Build the expected signature type
        let (expected_signature, output_ty, concept_name): (SmallVec<[Ty; 3]>, Ty, String) =
            if let Ty::CSGTree = &signature[0] {
                let concept = concepts.get(self.concept()).unwrap();

                (
                    concept
                        .src_ty
                        .iter()
                        .map(|astty| astty.clone().into())
                        .collect(),
                    concept.dst_ty.clone().into(),
                    concept.name.0.clone(),
                )
            } else {
                return Err(OptError::Any {
                    text: format!("Expected a CSGTree as first input, got {:?}", signature[0]),
                });
            };

        //Now check that the signature (except for the callabel, is the one we expect. If so, return the output_ty)
        let argcount = signature.len() - 1;
        if argcount != expected_signature.len() {
            return Err(OptError::Any {
                text: format!(
                    "Concept {} expected {} arguments, but got {}",
                    concept_name,
                    expected_signature.len(),
                    argcount
                ),
            });
        }

        for i in 0..argcount {
            //NOTE shift, since the first one is the concept we are calling (by definition).
            if expected_signature[i] != signature[i + 1] {
                return Err(OptError::Any {
                    text: format!(
                        "Concept {} expected {}-th argument to be {:?}, but was {:?}",
                        concept_name,
                        i,
                        expected_signature[i],
                        signature[i + 1]
                    ),
                });
            }
        }

        //If we made it till here, we actually passed, therfore return the right type
        Ok(Some(output_ty))
    }
}

///An immediate scalar number.
#[derive(LangNode, Debug)]
pub struct ImmScalar {
    ///The immediate value
    pub lit: f64,
    ///the output port the `lit` value is passed down to.
    #[output]
    pub out: Output,
}

impl ImmScalar {
    pub fn new(lit: f64) -> Self {
        ImmScalar {
            lit,
            out: Output::default(),
        }
    }
}

implViewAlgeOp!(ImmScalar, "{}f", lit);
impl DialectNode for ImmScalar {
    fn dialect(&self) -> &'static str {
        "alge"
    }

    fn try_derive_type(
        &self,
        _typemap: &FlagStore<Ty>,
        _graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //NOTE: all literals are translated to a _scalar_
        Ok(Some(Ty::Scalar))
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(ImmScalar {
                lit: self.lit.clone(),
                out: Output::default(),
            }),
        }
    }
}

///An immediate natural number.
#[derive(LangNode, Debug)]
pub struct ImmNat {
    //The natural value
    pub lit: u64,
    #[output]
    pub out: Output,
}

impl ImmNat {
    pub fn new(lit: usize) -> Self {
        ImmNat {
            lit: lit as u64,
            out: Output::default(),
        }
    }
}

implViewAlgeOp!(ImmNat, "{}i", lit);
impl DialectNode for ImmNat {
    fn dialect(&self) -> &'static str {
        "alge"
    }

    fn try_derive_type(
        &self,
        _typemap: &FlagStore<Ty>,
        _graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //NOTE: all literals are translated to a _scalar_
        Ok(Some(Ty::Nat))
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(ImmNat {
                lit: self.lit.clone(),
                out: Output::default(),
            }),
        }
    }
}

///Constructs some _aggregated_ type, i.e. A vector of _numbers_, a matrix of vectors or a tensor of matrices.
#[derive(LangNode, Debug)]
pub struct Construct {
    #[inputs]
    pub inputs: SmallColl<Input>,
    #[output]
    pub output: Output,
}

impl Construct {
    pub fn new() -> Self {
        Construct {
            inputs: SmallVec::new(),
            output: Output::default(),
        }
    }
}

implViewAlgeOp!(Construct, "Construct");
impl DialectNode for Construct {
    fn dialect(&self) -> &'static str {
        "alge"
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(Construct {
                inputs: smallvec![Input::default(); self.inputs.len()],
                output: Output::default(),
            }),
        }
    }

    fn try_derive_type(
        &self,
        _typemap: &FlagStore<Ty>,
        graph: &OptGraph,
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //For the list constructor, we check that all inputs are of the same (algebraic) type, and then derive the
        // algebraic super(?)-type. So a list of scalars becomes a vector, a list of vectors a matrix, and a list of
        // matrices a tensor. Tensors then just grow by pushing the next dimension size.
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

        if signature.len() == 0 {
            return Err(OptError::Any {
                text: format!("Cannot create an empty list (there is no void type in Vola)."),
            });
        }

        if !signature[0].is_algebraic() {
            return Err(OptError::Any { text: format!("List can only be created from algebraic types (scalar, vector, matrix, tensor), but first element was of type {:?}", signature[0]) });
        }

        //Check that all have the same type
        for s in 1..signature.len() {
            if signature[s] != signature[0] {
                return Err(OptError::Any { text: format!("List must be created from equal types. But first element is of type {:?} and {}-th element is of type {:?}", signature[0], s, signature[s]) });
            }
        }

        //Passed the equality check, therefore derive next list->Algebraic type
        let listlen = signature.len();
        match &signature[0] {
            Ty::Scalar => Ok(Some(Ty::Vector { width: listlen })),
            //NOTE this makes us effectively create line first matrices... doc that somewhere...
            Ty::Vector { width } => Ok(Some(Ty::Matrix {
                width: *width,
                height: listlen,
            })),
            Ty::Matrix { width, height } => {
                let mut tensor_sizes = SmallVec::new();
                tensor_sizes.push(*width);
                tensor_sizes.push(*height);
                tensor_sizes.push(listlen);
                Ok(Some(Ty::Tensor { dim: tensor_sizes }))
            }
            Ty::Tensor { dim } => {
                let mut dim = dim.clone();
                dim.push(listlen);
                Ok(Some(Ty::Tensor { dim }))
            }
            _ => panic!("enountered non-algebraic type"),
        }
    }
}

///Selects a subset of an _aggregated_ value. For instance an element of a matrix, a row in a matrix or a sub-matrix of an tensor.
#[derive(LangNode, Debug)]
pub struct ConstantIndex {
    ///The _value_ that is being indexed
    #[input]
    pub input: Input,
    #[output]
    pub output: Output,

    ///Constant value of what element being indexed.
    pub access: usize,
}

impl ConstantIndex {
    pub fn new(access_index: usize) -> Self {
        ConstantIndex {
            input: Input::default(),
            access: access_index,
            output: Output::default(),
        }
    }
}

implViewAlgeOp!(ConstantIndex, "CIndex: {}", access);
impl DialectNode for ConstantIndex {
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
        if let Some(edg) = self.input.edge {
            //List access can only be done on algebraic types. The type can decide itself if it fits the access pattern.
            if let OptEdge::Value {
                ty: TypeState::Derived(t) | TypeState::Set(t),
            } = &graph.edge(edg).ty
            {
                t.try_derive_access_index(self.access).map(|r| Some(r))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(ConstantIndex {
                access: self.access,
                input: Input::default(),
                output: Output::default(),
            }),
        }
    }
}
