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
    nodes::NodeType,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
    SmallColl,
};
use rvsdg_viewer::Color;
use trigonometric::{Trig, TrigOp};
use vola_ast::csg::{CSGConcept, CsgDef};
use vola_common::Span;

use crate::{
    //autodiff::AutoDiff,
    common::{DataType, Shape, Ty},
    error::OptError,
    imm::{ImmMatrix, ImmScalar, ImmVector},
    DialectNode,
    OptNode,
};

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
            //"diff" => Some(OptNode::new(AutoDiff::default(), Span::empty())),
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
        input_types: &[Ty],
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //For the list constructor, we check that all inputs are of the same (algebraic) type, and then derive the
        // algebraic super(?)-type. So a list of scalars becomes a vector, a list of vectors a matrix, and a list of
        // matrices a tensor. Tensors then just grow by pushing the next dimension size.

        if input_types.len() == 0 {
            return Err(OptError::Any {
                text: format!("Cannot create an empty list (there is no void type in Vola)."),
            });
        }

        if !input_types[0].is_algebraic() {
            return Err(OptError::Any { text: format!("List can only be created from algebraic types (scalar, vector, matrix, tensor), but first element was of type {:?}", input_types[0]) });
        }

        //Check that all have the same type
        for s in 1..input_types.len() {
            if input_types[s] != input_types[0] {
                return Err(OptError::Any { text: format!("List must be created from equal types. But first element is of type {:?} and {}-th element is of type {:?}", input_types[0], s, input_types[s]) });
            }
        }

        //Passed the equality check, therefore derive next list->Algebraic type
        let listlen = input_types.len();
        match &input_types[0] {
            Ty::Shaped {
                ty: DataType::Real,
                shape,
            } => match &shape {
                Shape::Scalar => Ok(Ty::vector_type(DataType::Real, listlen)),
                //NOTE:
                // This creates a column-major matrix. So each vector is a _column_ of this matrix. This is somewhat unintuitive
                // since maths do it the other way around, but glsl / spirv do it like that. So we keep that convention
                Shape::Vec { width } => Ok(Ty::shaped(
                    DataType::Real,
                    Shape::Matrix {
                        width: listlen,
                        height: *width,
                    },
                )),
                Shape::Matrix { width, height } => {
                    let mut tensor_sizes = SmallVec::new();
                    tensor_sizes.push(*width);
                    tensor_sizes.push(*height);
                    tensor_sizes.push(listlen);
                    Ok(Ty::shaped(
                        DataType::Real,
                        Shape::Tensor {
                            sizes: tensor_sizes,
                        },
                    ))
                }
                Shape::Tensor { sizes } => {
                    let mut dim = sizes.clone();
                    dim.push(listlen);
                    Ok(Ty::shaped(DataType::Real, Shape::Tensor { sizes: dim }))
                }
                _ => Err(OptError::TypeDeriveError {
                    text: format!("Cannot construct list from {} elements", input_types[0]),
                }),
            },
            _ => Err(OptError::TypeDeriveError {
                text: format!("Cannot construct \"List\" from \"{}\"", input_types[0]),
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
        input_types: &[Ty],
        _concepts: &AHashMap<String, CSGConcept>,
        _csg_defs: &AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        assert_eq!(input_types.len(), 1);
        input_types[0].try_derive_access_index(self.access)
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
