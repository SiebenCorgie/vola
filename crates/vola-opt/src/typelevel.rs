/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use ahash::AHashMap;
use rvsdg::{
    nodes::NodeType,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
    SmallColl,
};
use rvsdg_viewer::Color;
use vola_ast::csg::{CsgConcept, CsgDef};
use vola_common::Span;

use crate::{
    common::{DataType, Shape, Ty},
    error::OptError,
    imm::{ImmBool, ImmMatrix, ImmNat, ImmScalar, ImmVector},
    DialectNode, OptNode,
};

//Macro that implements the "View" trait for an TypeLevel op
macro_rules! implViewTyOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> Color {
                Color::from_rgba(180, 150, 180, 255)
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
                Color::from_rgba(180, 150, 180, 255)
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

///Constructs some _aggregated_ uniform type, i.e. A vector of _numbers_, a matrix of vectors or a tensor of matrices.
///The point being, that each input is of the same type `T`, and the output is a shaped `T`.
#[derive(LangNode, Debug)]
pub struct UniformConstruct {
    #[inputs]
    pub inputs: SmallColl<Input>,
    #[output]
    pub output: Output,
}

impl UniformConstruct {
    pub fn new() -> Self {
        UniformConstruct {
            inputs: SmallVec::new(),
            output: Output::default(),
        }
    }
    pub fn with_inputs(mut self, count: usize) -> Self {
        self.inputs = smallvec![Input::default(); count];
        self
    }
}

implViewTyOp!(UniformConstruct, "UniformConstruct");
impl DialectNode for UniformConstruct {
    fn dialect(&self) -> &'static str {
        "typelevel"
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(UniformConstruct {
                inputs: smallvec![Input::default(); self.inputs.len()],
                output: Output::default(),
            }),
        }
    }

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        //NOTE: Two construct nodes are always equal
        if let Some(_other_cop) = other.try_downcast_ref::<UniformConstruct>() {
            true
        } else {
            false
        }
    }
    fn try_derive_type(
        &self,
        input_types: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
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

///Creates a non-uniform aggregate from several input types. Contrary to [UniformConstruct] this won't produce a shaped
/// type, but a Aggregate.
#[derive(LangNode, Debug)]
pub struct NonUniformConstruct {
    #[inputs]
    pub inputs: SmallColl<Input>,
    #[output]
    pub output: Output,
}

impl NonUniformConstruct {
    pub fn new(argcount: usize) -> Self {
        Self {
            inputs: smallvec![Input::default(); argcount],
            output: Output::default(),
        }
    }
}

implViewTyOp!(NonUniformConstruct, "NonUniformConstruct");
impl DialectNode for NonUniformConstruct {
    fn dialect(&self) -> &'static str {
        "typelevel"
    }

    fn structural_copy(&self, span: Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(NonUniformConstruct {
                inputs: smallvec![Input::default(); self.inputs.len()],
                output: Output::default(),
            }),
        }
    }
    fn try_derive_type(
        &self,
        input_types: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
        _csg_defs: &ahash::AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //Tuple construct does not reall care _at-all_ what is _inside_.
        //However, we make sure that no callables and voids are supplied, which we do
        //not have the capabilities to use atm.

        for (idx, t) in input_types.iter().enumerate() {
            if t == &Ty::VOID || t == &Ty::Callable {
                return Err(OptError::TypeDeriveError {
                    text: format!(
                        "Tuple constructor cannot take {t} as {}-th argument",
                        idx + 1
                    ),
                });
            }
        }

        let ty = Ty::Tuple(input_types.to_vec());
        Ok(ty)
    }
    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        //Tuple constructors cannot meaningfully be folded.
        None
    }
    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if other.try_downcast_ref::<Self>().is_some() {
            true
        } else {
            false
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

implViewTyOp!(ConstantIndex, "CIndex: {}", access);
impl DialectNode for ConstantIndex {
    fn dialect(&self) -> &'static str {
        "typelevel"
    }

    fn try_derive_type(
        &self,
        input_types: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
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

///Type cast: Allows casting the base-type of any value.
/// The casting rules follow SPIR-V, see for instance [ConvertFToS](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#OpConvertFToS)
/// for an example.
#[derive(LangNode, Debug)]
pub struct TypeCast {
    ///The _value_ that is being indexed
    #[input]
    pub input: Input,
    #[output]
    pub output: Output,
    ///the type this node casts to
    dst_ty: Ty,
}

impl TypeCast {
    pub fn new(dst_ty: Ty) -> Self {
        TypeCast {
            input: Input::default(),
            output: Output::default(),
            dst_ty,
        }
    }
}

implViewTyOp!(TypeCast, "TypeCast: {}", dst_ty);
impl DialectNode for TypeCast {
    fn dialect(&self) -> &'static str {
        "typelevel"
    }

    fn try_derive_type(
        &self,
        input_types: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
        _csg_defs: &AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        assert_eq!(input_types.len(), 1);
        match (input_types[0].clone(), self.dst_ty.clone()) {
            (Ty::Tuple(_), _) => {
                return Err(OptError::TypeDeriveError {
                    text: "Can not cast tuple type".to_owned(),
                })
            }
            (Ty::Callable, _) => {
                return Err(OptError::TypeDeriveError {
                    text: "can not cast callables".to_owned(),
                })
            }
            (
                Ty::Shaped {
                    ty: tyl,
                    shape: shapel,
                },
                Ty::Shaped {
                    ty: tyr,
                    shape: shaper,
                },
            ) => {
                //Make sure the data types are compatible
                match (tyl, tyr) {
                    (DataType::Bool, DataType::Integer)
                    | (DataType::Bool, DataType::Real)
                    | (DataType::Integer, DataType::Real)
                    | (DataType::Real, DataType::Integer)
                    | (DataType::Integer, DataType::Bool) => {}
                    (l, r) => {
                        return Err(OptError::TypeDeriveError {
                            text: format!("can not convert data-type {l} to {r}",),
                        })
                    }
                };
                //now check that the types are compatible
                match (shapel, shaper) {
                    //For vectors make sure they are of the same element count
                    (Shape::Vec { width: wr }, Shape::Vec { width: wl }) => {
                        if wr != wl {
                            return Err(OptError::TypeDeriveError {
                                text: format!(
                                    "Can not convert vector with {wr}-elements to {wl}-elements"
                                ),
                            });
                        }
                    }
                    //Scalar types of compatible data-type always work
                    (Shape::Scalar, Shape::Scalar) => {}
                    //Otherwise, if the types are not the same, we can't cast.
                    (l, r) => {
                        if l != r {
                            return Err(OptError::TypeDeriveError {
                                text: format!("Can not cast {l} into {r}"),
                            });
                        }
                    }
                }
            }
            (l, r) => {
                return Err(OptError::TypeDeriveError {
                    text: format!("Can not cast {} to {}", l, r),
                })
            }
        }
        //Passed all checks, so matches
        Ok(self.dst_ty.clone())
    }

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        //NOTE: Two construct nodes are always equal
        if let Some(other_cop) = other.try_downcast_ref::<TypeCast>() {
            other_cop.dst_ty == self.dst_ty
        } else {
            false
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(TypeCast {
                dst_ty: self.dst_ty.clone(),
                input: Input::default(),
                output: Output::default(),
            }),
        }
    }

    fn try_constant_fold(
        &self,
        src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        //We can fold immediate value with matching types.

        if src_nodes.len() > 1 {
            #[cfg(feature = "log")]
            log::error!("TypeCast had {} inputs, expected 1", src_nodes.len());
        }

        if src_nodes.len() != 1 {
            return None;
        }

        if let Some(NodeType::Simple(s)) = src_nodes[0].map(|n| &n.node_type) {
            if let Some(immscalar) = s.try_downcast_ref::<ImmScalar>() {
                match self.dst_ty {
                    Ty::Shaped {
                        ty: DataType::Bool,
                        shape: Shape::Scalar,
                    } => {
                        return Some(OptNode::new(
                            //Emulates rounddown + unsigned cast SPIR-V does
                            ImmBool::new(immscalar.lit >= 1.0),
                            Span::empty(),
                        ));
                    }
                    Ty::Shaped {
                        ty: DataType::Integer,
                        shape: Shape::Scalar,
                    } => {
                        return Some(OptNode::new(
                            //Emulates rounddown + unsigned cast SPIR-V does
                            ImmNat::new(immscalar.lit as usize),
                            Span::empty(),
                        ));
                    }
                    //All others can't be folded for now
                    _ => return None,
                }
            }
            if let Some(immnat) = s.try_downcast_ref::<ImmNat>() {
                match self.dst_ty {
                    Ty::Shaped {
                        ty: DataType::Bool,
                        shape: Shape::Scalar,
                    } => {
                        return Some(OptNode::new(
                            //Emulates rounddown + unsigned cast SPIR-V does
                            ImmBool::new(immnat.lit >= 1),
                            Span::empty(),
                        ));
                    }
                    Ty::Shaped {
                        ty: DataType::Real,
                        shape: Shape::Scalar,
                    } => {
                        return Some(OptNode::new(
                            ImmScalar::new(immnat.lit as f64),
                            Span::empty(),
                        ));
                    }
                    //All others can't be folded for now
                    _ => return None,
                }
            }
        }

        None
    }
}
