/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */
//! # Alge dialect
//!
//! Implements anything related to algebraic operations

use ahash::AHashMap;
use arithmetic::{BinaryArith, BinaryArithOp, UnaryArith, UnaryArithOp};
use buildin::{Buildin, BuildinOp};
use logical::{BinaryBool, BinaryBoolOp, UnaryBool, UnaryBoolOp};
use matrix::{UnaryMatrix, UnaryMatrixOp};
use relational::{BinaryRel, BinaryRelOp};
use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
};
use rvsdg_viewer::Color;
use trigonometric::{Trig, TrigOp};
use vola_ast::csg::{CSGConcept, CsgDef};
use vola_common::Span;

use crate::{autodiff::AutoDiff, common::Ty, error::OptError, DialectNode, OptNode};

pub mod arithmetic;
pub mod buildin;
pub mod constant_fold;
pub mod logical;
pub mod matrix;
pub mod relational;
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

impl OptNode {
    pub fn try_parse(s: &str) -> Option<Self> {
        match s {
            "dot" => Some(OptNode::new(Buildin::new(BuildinOp::Dot), Span::empty())),
            "cross" => Some(OptNode::new(Buildin::new(BuildinOp::Cross), Span::empty())),
            "length" => Some(OptNode::new(Buildin::new(BuildinOp::Length), Span::empty())),
            "sqrt" => Some(OptNode::new(
                Buildin::new(BuildinOp::SquareRoot),
                Span::empty(),
            )),
            "exp" => Some(OptNode::new(Buildin::new(BuildinOp::Exp), Span::empty())),
            "pow" => Some(OptNode::new(Buildin::new(BuildinOp::Pow), Span::empty())),
            "min" => Some(OptNode::new(Buildin::new(BuildinOp::Min), Span::empty())),
            "max" => Some(OptNode::new(Buildin::new(BuildinOp::Max), Span::empty())),
            "mix" | "lerp" => Some(OptNode::new(Buildin::new(BuildinOp::Mix), Span::empty())),
            "mod" => Some(OptNode::new(
                BinaryArith::new(BinaryArithOp::Mod),
                Span::empty(),
            )),
            "clamp" => Some(OptNode::new(Buildin::new(BuildinOp::Clamp), Span::empty())),
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
            "inverse" | "invert" => Some(OptNode::new(
                UnaryMatrix::new(UnaryMatrixOp::Invert),
                Span::empty(),
            )),
            "diff" => Some(OptNode::new(AutoDiff::default(), Span::empty())),
            _ => None,
        }
    }
}

impl From<vola_ast::alge::UnaryOp> for OptNode {
    fn from(value: vola_ast::alge::UnaryOp) -> Self {
        match value {
            vola_ast::alge::UnaryOp::Neg => {
                OptNode::new(UnaryArith::new(UnaryArithOp::Neg), Span::empty())
            }
            vola_ast::alge::UnaryOp::Not => {
                OptNode::new(UnaryBool::new(UnaryBoolOp::Not), Span::empty())
            }
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

            vola_ast::alge::BinaryOp::Lt => {
                OptNode::new(BinaryRel::new(BinaryRelOp::Lt), Span::empty())
            }
            vola_ast::alge::BinaryOp::Gt => {
                OptNode::new(BinaryRel::new(BinaryRelOp::Gt), Span::empty())
            }
            vola_ast::alge::BinaryOp::Gte => {
                OptNode::new(BinaryRel::new(BinaryRelOp::Gte), Span::empty())
            }
            vola_ast::alge::BinaryOp::Lte => {
                OptNode::new(BinaryRel::new(BinaryRelOp::Lte), Span::empty())
            }
            vola_ast::alge::BinaryOp::Eq => {
                OptNode::new(BinaryRel::new(BinaryRelOp::Eq), Span::empty())
            }
            vola_ast::alge::BinaryOp::NotEq => {
                OptNode::new(BinaryRel::new(BinaryRelOp::NotEq), Span::empty())
            }

            vola_ast::alge::BinaryOp::Or => {
                OptNode::new(BinaryBool::new(BinaryBoolOp::Or), Span::empty())
            }
            vola_ast::alge::BinaryOp::And => {
                OptNode::new(BinaryBool::new(BinaryBoolOp::And), Span::empty())
            }
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
            inputs: smallvec![Input::default(); argount],
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
        input_types: &[Ty],
        concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //For eval nodes, the first type must be a callable, and all following must addher to the called concept's definition

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
        let (expected_signature, output_ty, concept_name): (Ty, Ty, String) =
            if Ty::CSG == input_types[0] {
                let concept = concepts.get(self.concept()).unwrap();

                (
                    concept.src_ty.clone().into(),
                    concept.dst_ty.clone().into(),
                    concept.name.0.clone(),
                )
            } else {
                return Err(OptError::Any {
                    text: format!("Expected a CSG as first input, got {:?}", input_types[0]),
                });
            };

        //Now check that the signature (except for the callabel, is the one we expect. If so, return the output_ty)
        let argcount = input_types.len() - 1;
        if argcount != 1 {
            return Err(OptError::Any {
                text: format!(
                    "Concept {} expected 1 argument, but got {}",
                    concept_name, argcount
                ),
            });
        }

        //NOTE shift, since the first one is the concept we are calling (by definition).
        if expected_signature != input_types[1] {
            return Err(OptError::Any {
                text: format!(
                    "Concept {} expected argument to be of type {:?}, but was {:?}",
                    concept_name, expected_signature, input_types[1]
                ),
            });
        }

        //If we made it till here, we actually passed, therfore return the right type
        Ok(output_ty)
    }
}
