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
use arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp};
use rvsdg::{
    attrib::FlagStore,
    nodes::NodeType,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
    SmallColl,
};
use rvsdg_viewer::Color;
use trigonometric::{Trig, TrigOp};
use vola_ast::csg::{CSGConcept, CSGNodeDef};
use vola_common::Span;

use crate::{
    common::Ty,
    error::OptError,
    imm::{ImmMatrix, ImmScalar, ImmVector},
    DialectNode, OptEdge, OptGraph, OptNode, TypeState,
};

pub mod algefn;
pub mod arithmetic;
pub mod boolean;
pub mod buildin;
pub mod constant_fold;
pub mod implblock;
pub mod logical;
pub mod matrix;
pub mod trigonometric;

pub const ALGE_VIEW_COLOR: Color = Color {
    r: 200.0 / 255.0,
    g: 170.0 / 255.0,
    b: 170.0 / 255.0,
    a: 1.0,
};

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

///Well known ops for the optimizer. Includes all _BinaryOp_ of the Ast, as well as
/// some well known _function_like_ ops in the SPIRV spec. For instance
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WkOp {
    //WK unary ops
    //TODO: do we really want to include NOT? I mean flipping floats is fun I guess,
    // but also error prone.
    Not,

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

    //TODO: basically implement the rest of the glsl-ext-inst, cause people might be used to those.
    Inverse,
}

impl WkOp {
    ///Returns the input-count for the op
    pub fn in_count(&self) -> usize {
        match self {
            WkOp::Not => 1,

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

            WkOp::Inverse => 1,
        }
    }

    fn try_derive_type(&self, mut sig: SmallVec<[Ty; 2]>) -> Result<Option<Ty>, OptError> {
        //NOTE: sig.len() is somewhat redundant, since the caller wouldn't try to derive if any input is
        // empty. However, its a good place to unify this check even if the type resolution changes at some point.

        match self {
            WkOp::Not => {
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

            WkOp::Min | WkOp::Max => {
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

impl OptNode {
    pub fn try_parse(s: &str) -> Option<Self> {
        match s {
            "dot" => Some(OptNode::new(CallOp::new(WkOp::Dot), Span::empty())),
            "cross" => Some(OptNode::new(CallOp::new(WkOp::Cross), Span::empty())),
            "length" => Some(OptNode::new(CallOp::new(WkOp::Length), Span::empty())),
            "sqrt" => Some(OptNode::new(CallOp::new(WkOp::SquareRoot), Span::empty())),
            "exp" => Some(OptNode::new(CallOp::new(WkOp::Exp), Span::empty())),
            "pow" => Some(OptNode::new(CallOp::new(WkOp::Pow), Span::empty())),
            "min" => Some(OptNode::new(CallOp::new(WkOp::Min), Span::empty())),
            "max" => Some(OptNode::new(CallOp::new(WkOp::Max), Span::empty())),
            "mix" | "lerp" => Some(OptNode::new(CallOp::new(WkOp::Mix), Span::empty())),
            "mod" => Some(OptNode::new(
                BinaryArith::new(BinaryArithOp::Mod),
                Span::empty(),
            )),
            "clamp" => Some(OptNode::new(CallOp::new(WkOp::Clamp), Span::empty())),
            "fract" => Some(OptNode::new(
                UnaryArith::new(UnaryArithOp::Fract),
                Span::empty(),
            )),
            "abs" => Some(OptNode::new(
                UnaryArith::new(UnaryArithOp::Abs),
                Span::empty(),
            )),
            "round" => Some(OptNode::new(
                UnaryArith::new(UnaryArithOp::Round),
                Span::empty(),
            )),
            "ceil" => Some(OptNode::new(
                UnaryArith::new(UnaryArithOp::Ceil),
                Span::empty(),
            )),
            "floor" => Some(OptNode::new(
                UnaryArith::new(UnaryArithOp::Floor),
                Span::empty(),
            )),
            "sin" => Some(OptNode::new(Trig::new(TrigOp::Sin), Span::empty())),
            "cos" => Some(OptNode::new(Trig::new(TrigOp::Cos), Span::empty())),
            "tan" => Some(OptNode::new(Trig::new(TrigOp::Tan), Span::empty())),
            "asin" => Some(OptNode::new(Trig::new(TrigOp::ASin), Span::empty())),
            "acos" => Some(OptNode::new(Trig::new(TrigOp::ACos), Span::empty())),
            "atan" => Some(OptNode::new(Trig::new(TrigOp::ATan), Span::empty())),
            "inverse" | "invert" => Some(OptNode::new(CallOp::new(WkOp::Inverse), Span::empty())),
            _ => None,
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

impl From<vola_ast::alge::UnaryOp> for OptNode {
    fn from(value: vola_ast::alge::UnaryOp) -> Self {
        match value {
            vola_ast::alge::UnaryOp::Neg => {
                OptNode::new(UnaryArith::new(UnaryArithOp::Neg), Span::empty())
            }
            vola_ast::alge::UnaryOp::Not => OptNode::new(CallOp::new(WkOp::Not), Span::empty()),
        }
    }
}

impl From<vola_ast::alge::BinaryOp> for OptNode {
    fn from(value: vola_ast::alge::BinaryOp) -> Self {
        match value {
            vola_ast::alge::BinaryOp::Add => {
                OptNode::new(BinaryArith::new(BinaryArithOp::Add), Span::empty())
            }
            vola_ast::alge::BinaryOp::Sub => {
                OptNode::new(BinaryArith::new(BinaryArithOp::Sub), Span::empty())
            }
            vola_ast::alge::BinaryOp::Mul => {
                OptNode::new(BinaryArith::new(BinaryArithOp::Mul), Span::empty())
            }
            vola_ast::alge::BinaryOp::Div => {
                OptNode::new(BinaryArith::new(BinaryArithOp::Div), Span::empty())
            }
            vola_ast::alge::BinaryOp::Mod => {
                OptNode::new(BinaryArith::new(BinaryArithOp::Mod), Span::empty())
            }

            vola_ast::alge::BinaryOp::Lt => OptNode::new(CallOp::new(WkOp::Lt), Span::empty()),
            vola_ast::alge::BinaryOp::Gt => OptNode::new(CallOp::new(WkOp::Gt), Span::empty()),
            vola_ast::alge::BinaryOp::Gte => OptNode::new(CallOp::new(WkOp::Gte), Span::empty()),
            vola_ast::alge::BinaryOp::Lte => OptNode::new(CallOp::new(WkOp::Lte), Span::empty()),
            vola_ast::alge::BinaryOp::Eq => OptNode::new(CallOp::new(WkOp::Eq), Span::empty()),
            vola_ast::alge::BinaryOp::NotEq => {
                OptNode::new(CallOp::new(WkOp::NotEq), Span::empty())
            }

            vola_ast::alge::BinaryOp::Or => OptNode::new(CallOp::new(WkOp::Or), Span::empty()),
            vola_ast::alge::BinaryOp::And => OptNode::new(CallOp::new(WkOp::And), Span::empty()),
        }
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

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<CallOp>() {
            other_cop.op == self.op
        } else {
            false
        }
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

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        //NOTE: this is _correct_, becauset the _eval_ itself is only keyed by the concept it calls.
        //      So if two evals reference the same sub-tree, and call the same concept, they can indeed
        //      be unified.
        if let Some(other_cop) = other.try_downcast_ref::<EvalNode>() {
            other_cop.called_concept == self.called_concept
        } else {
            false
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
    pub fn with_inputs(mut self, count: usize) -> Self {
        self.inputs = smallvec![Input::default(); count];
        self
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

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        //NOTE: Two construct nodes are always equal
        if let Some(_other_cop) = other.try_downcast_ref::<Construct>() {
            true
        } else {
            false
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
            //NOTE:
            // This creates a column-major matrix. So each vector is a _column_ of this matrix. This is somewhat unintuitive
            // since maths do it the other way around, but glsl / spirv do it like that. So we keep that convention
            Ty::Vector { width } => Ok(Some(Ty::Matrix {
                width: listlen,
                height: *width,
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
            _ => Err(OptError::TypeDeriveError {
                text: format!("Cannot construct \"List\" from \"{}\"", signature[0]),
            }),
        }
    }

    fn try_constant_fold(
        &self,
        src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        if src_nodes.len() == 0 {
            return None;
        }
        if src_nodes[0].is_none() {
            return None;
        }

        //We can constant fold a set of scalars into a vector, and a set of
        //equal width vectors into a matrix.
        //
        //Everything else can be ignored.
        //
        // Since both depend on _all nodes being the same_ we can just try_cast the first node, and depending on if its a scalar
        // or vector, try to cast the rest. If it ain't any of these, just bail

        if let NodeType::Simple(s) = &src_nodes[0].unwrap().node_type {
            if s.try_downcast_ref::<ImmScalar>().is_some() {
                //try to build vector of scalars
                let mut scalars = SmallColl::new();
                for srcn in src_nodes {
                    if let Some(NodeType::Simple(s)) = srcn.map(|n| &n.node_type) {
                        if let Some(scalar) = s.try_downcast_ref::<ImmScalar>() {
                            scalars.push(scalar.lit);
                        } else {
                            //Was no scalar actualy, bail
                            return None;
                        }
                    } else {
                        //If couldn't be cast, bail
                        return None;
                    }
                }

                //If we reached this, we could fold all constants into one vec
                return Some(OptNode::new(ImmVector::new(&scalars), Span::empty()));
            }

            //try folding into matix with equal-width vectors
            if let Some(first_vec) = s.try_downcast_ref::<ImmVector>() {
                let expected_column_height = first_vec.lit.len();
                let mut columns = SmallColl::new();

                for srcn in src_nodes {
                    if let Some(NodeType::Simple(s)) = srcn.map(|n| &n.node_type) {
                        if let Some(vec) = s.try_downcast_ref::<ImmVector>() {
                            //Bail if not same size
                            if vec.lit.len() != expected_column_height {
                                return None;
                            }
                            columns.push(SmallColl::from_slice(&vec.lit));
                        } else {
                            //Was no scalar actualy, bail
                            return None;
                        }
                    } else {
                        //If couldn't be cast, bail
                        return None;
                    }
                }

                //If we reached this, we could fold all constants into one matrix
                return Some(OptNode::new(ImmMatrix::new(columns), Span::empty()));
            }
        }

        None
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
    fn is_operation_equal(&self, other: &OptNode) -> bool {
        //NOTE: Two construct nodes are always equal
        if let Some(other_cop) = other.try_downcast_ref::<ConstantIndex>() {
            other_cop.access == self.access
        } else {
            false
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

    fn try_constant_fold(
        &self,
        src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        //we can fold any ImmVector and ImmMatrix into a the next lower representation.
        //The access _should_ be valid / in-bounds, otherwise the type-derive pass before _should_ have paniced.

        if src_nodes.len() > 1 {
            #[cfg(feature = "log")]
            log::error!("ConstantIndex had {} inputs, expected 1", src_nodes.len());
        }

        if src_nodes.len() != 1 {
            return None;
        }

        if let Some(NodeType::Simple(s)) = src_nodes[0].map(|n| &n.node_type) {
            if let Some(vec) = s.try_downcast_ref::<ImmVector>() {
                assert!(vec.lit.len() > self.access);
                return Some(OptNode::new(
                    ImmScalar::new(vec.lit[self.access]),
                    Span::empty(),
                ));
            }

            if let Some(mat) = s.try_downcast_ref::<ImmMatrix>() {
                assert!(mat.lit.len() > self.access);
                return Some(OptNode::new(
                    ImmVector::new(mat.lit[self.access].as_slice()),
                    Span::empty(),
                ));
            }
        }

        None
    }
}
