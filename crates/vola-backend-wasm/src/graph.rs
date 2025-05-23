/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use std::fmt::Display;

use rvsdg::{
    edge::LangEdge,
    nodes::LangNode,
    region::{Inport, Input, Outport, Output},
    smallvec::smallvec,
    SmallColl,
};
use rvsdg_viewer::View;
use vola_opt::{
    alge::{
        arithmetic::{BinaryArith, UnaryArith},
        buildin::Buildin,
        logical::{BinaryBool, UnaryBool},
        matrix::UnaryMatrix,
        relational::BinaryRel,
        trigonometric::Trig,
    },
    common::{DataType, Shape},
    imm::{ImmBool, ImmMatrix, ImmNat, ImmScalar, ImmVector},
    typelevel::ConstantIndex,
    OptNode,
};
use walrus::{
    ir::{LoadKind, StoreKind},
    ValType,
};

use crate::{
    error::WasmError,
    wasm::{
        self, ExternOp, Index, NonUniformConstruct, UniformConstruct, WasmBinaryOp, WasmRuntimeOp,
        WasmUnaryOp, WasmValue,
    },
};

mod utils;

pub enum WasmNode {
    Unary(WasmUnaryOp),
    Binary(WasmBinaryOp),
    Runtime(WasmRuntimeOp),
    Value(WasmValue),
    Index(Index),
    UniformConstruct(UniformConstruct),
    NonUniformConstruct(NonUniformConstruct),
    Error {
        inputs: SmallColl<Input>,
        outputs: SmallColl<Output>,
    },
}

impl WasmNode {
    pub fn error_for_opt(opt: &OptNode) -> Self {
        Self::error_for_sig(opt.inputs().len(), opt.outputs().len())
    }

    pub fn error_for_sig(inputs: usize, outputs: usize) -> Self {
        Self::Error {
            inputs: smallvec![Inport::default(); inputs],
            outputs: smallvec![Outport::default(); outputs],
        }
    }

    pub fn try_from_opt(
        value: &OptNode,
        input_sig: &[Option<vola_opt::common::Ty>],
        output_sig: &[Option<vola_opt::common::Ty>],
    ) -> Result<Self, WasmError> {
        //Our wasm backend supports the whole alge dialect, and all immediate values.
        if let Some(imm) = value.try_downcast_ref::<ImmNat>() {
            assert!(input_sig.len() == 0);
            assert!(output_sig.len() == 1);
            assert!(output_sig[0] == Some(vola_opt::common::Ty::SCALAR_INT));
            return Ok(WasmNode::from(imm));
        }
        if let Some(imm) = value.try_downcast_ref::<ImmScalar>() {
            assert!(input_sig.len() == 0);
            assert!(output_sig.len() == 1);
            assert!(output_sig[0] == Some(vola_opt::common::Ty::SCALAR_REAL));
            return Ok(WasmNode::from(imm));
        }
        if let Some(imm) = value.try_downcast_ref::<ImmBool>() {
            assert!(output_sig[0] == Some(vola_opt::common::Ty::SCALAR_BOOL));
            return Ok(WasmNode::from(imm));
        }

        if let Some(index_node) = value.try_downcast_ref::<ConstantIndex>() {
            return Ok(WasmNode::Index(Index {
                input: Inport::default(),
                index: index_node.access,
                output: Outport::default(),
            }));
        }

        if let Some(construct_node) =
            value.try_downcast_ref::<vola_opt::typelevel::UniformConstruct>()
        {
            return Ok(WasmNode::UniformConstruct(wasm::UniformConstruct::from(
                construct_node,
            )));
        }
        if let Some(construct_node) =
            value.try_downcast_ref::<vola_opt::typelevel::NonUniformConstruct>()
        {
            return Ok(WasmNode::NonUniformConstruct(
                wasm::NonUniformConstruct::from(construct_node),
            ));
        }

        if let Some(_tc) = value.try_downcast_ref::<vola_opt::typelevel::TypeCast>() {
            assert!(input_sig.len() == 1);
            assert!(input_sig[0].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());

            let dst_ty = WasmTy::try_from(output_sig[0].clone().unwrap())?;

            return Ok(WasmNode::Runtime(WasmRuntimeOp::new_with_signature(
                1,
                ExternOp::Cast(dst_ty),
            )));
        }

        if let Some(binop) = value.try_downcast_ref::<BinaryArith>() {
            assert!(input_sig.len() == 2);
            assert!(input_sig[0].is_some() && input_sig[1].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());
            return WasmNode::try_from_opt_binary(
                binop,
                [
                    input_sig[0].as_ref().unwrap().clone(),
                    input_sig[1].as_ref().unwrap().clone(),
                ],
                output_sig[0].as_ref().unwrap().clone(),
            );
        }
        if let Some(binop) = value.try_downcast_ref::<BinaryRel>() {
            assert!(input_sig.len() == 2);
            assert!(input_sig[0].is_some() && input_sig[1].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());
            return WasmNode::try_from_op_binaryrel(
                binop,
                [
                    input_sig[0].as_ref().unwrap().clone(),
                    input_sig[1].as_ref().unwrap().clone(),
                ],
                output_sig[0].as_ref().unwrap().clone(),
            );
        }
        if let Some(binop) = value.try_downcast_ref::<BinaryBool>() {
            assert!(input_sig.len() == 2);
            assert!(input_sig[0].is_some() && input_sig[1].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());
            return WasmNode::try_from_opt_binary_bool(
                binop,
                [
                    input_sig[0].as_ref().unwrap().clone(),
                    input_sig[1].as_ref().unwrap().clone(),
                ],
                output_sig[0].as_ref().unwrap().clone(),
            );
        }

        if let Some(unop) = value.try_downcast_ref::<UnaryArith>() {
            assert!(input_sig.len() == 1);
            assert!(input_sig[0].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());
            return WasmNode::try_from_unary_arith(
                unop,
                input_sig[0].as_ref().unwrap().clone(),
                output_sig[0].as_ref().unwrap().clone(),
            );
        }
        if let Some(unop) = value.try_downcast_ref::<UnaryBool>() {
            assert!(input_sig.len() == 1);
            assert!(input_sig[0].is_some());
            assert!(output_sig.len() == 1);
            assert!(output_sig[0].is_some());
            return WasmNode::try_from_opt_unary_bool(
                unop,
                input_sig[0].as_ref().unwrap().clone(),
                output_sig[0].as_ref().unwrap().clone(),
            );
        }

        if let Some(bi) = value.try_downcast_ref::<Buildin>() {
            let inputs = input_sig
                .into_iter()
                .map(|i| i.clone().unwrap_or(vola_opt::common::Ty::VOID))
                .collect::<SmallColl<_>>();
            let outputs = output_sig
                .into_iter()
                .map(|o| o.clone().unwrap_or(vola_opt::common::Ty::VOID))
                .collect::<SmallColl<_>>();
            return WasmNode::try_from_opt_buildin(bi, &inputs, &outputs);
        }
        if let Some(unop) = value.try_downcast_ref::<Trig>() {
            return Ok(WasmNode::from(unop));
        }
        if let Some(unop) = value.try_downcast_ref::<UnaryMatrix>() {
            return Ok(WasmNode::from(unop));
        }

        //This would hint that the expected ImmScalarizer was not executed.
        if value.try_downcast_ref::<ImmVector>().is_some()
            || value.try_downcast_ref::<ImmMatrix>().is_some()
        {
            return Err(WasmError::UnexpectedComposite);
        }

        Err(WasmError::UnsupportedNode(value.name().to_string()))
    }
}

impl LangNode for WasmNode {
    fn inputs(&self) -> &[rvsdg::region::Input] {
        match self {
            Self::Unary(u) => u.inputs(),
            Self::Binary(b) => b.inputs(),
            Self::Runtime(r) => r.inputs(),
            Self::Value(v) => v.inputs(),
            Self::Index(i) => i.inputs(),
            Self::UniformConstruct(c) => c.inputs(),
            Self::NonUniformConstruct(c) => c.inputs(),
            Self::Error { inputs, .. } => inputs,
        }
    }

    fn inputs_mut(&mut self) -> &mut [rvsdg::region::Input] {
        match self {
            Self::Unary(u) => u.inputs_mut(),
            Self::Binary(b) => b.inputs_mut(),
            Self::Runtime(r) => r.inputs_mut(),
            Self::Value(v) => v.inputs_mut(),
            Self::Index(i) => i.inputs_mut(),
            Self::UniformConstruct(c) => c.inputs_mut(),
            Self::NonUniformConstruct(c) => c.inputs_mut(),
            Self::Error { inputs, .. } => inputs,
        }
    }

    fn outputs(&self) -> &[rvsdg::region::Output] {
        match self {
            Self::Unary(u) => u.outputs(),
            Self::Binary(b) => b.outputs(),
            Self::Runtime(r) => r.outputs(),
            Self::Value(v) => v.outputs(),
            Self::Index(i) => i.outputs(),
            Self::UniformConstruct(c) => c.outputs(),
            Self::NonUniformConstruct(c) => c.outputs(),
            Self::Error { outputs, .. } => outputs,
        }
    }

    fn outputs_mut(&mut self) -> &mut [rvsdg::region::Output] {
        match self {
            Self::Unary(u) => u.outputs_mut(),
            Self::Binary(b) => b.outputs_mut(),
            Self::Runtime(r) => r.outputs_mut(),
            Self::Value(v) => v.outputs_mut(),
            Self::Index(i) => i.outputs_mut(),
            Self::UniformConstruct(c) => c.outputs_mut(),
            Self::NonUniformConstruct(c) => c.outputs_mut(),
            Self::Error { outputs, .. } => outputs,
        }
    }
}

impl View for WasmNode {
    fn name(&self) -> String {
        match self {
            Self::Unary(u) => format!("{:?}", u.op),
            Self::Binary(b) => format!("{:?}", b.op),
            Self::Runtime(r) => format!("{:?}", r.op),
            Self::Value(v) => format!("{:?}", v.op),
            Self::Index(i) => format!("Index<{}>", i.index),
            Self::UniformConstruct(_) => format!("UniformConstruct"),
            Self::NonUniformConstruct(_) => format!("NonUniformConstruct"),
            Self::Error { .. } => format!("Error"),
        }
    }

    fn color(&self) -> rvsdg_viewer::Color {
        match self {
            Self::Unary(_u) => rvsdg_viewer::Color::from_rgba(255, 230, 220, 255),
            Self::Binary(_b) => rvsdg_viewer::Color::from_rgba(220, 255, 230, 255),
            Self::Runtime(_r) => rvsdg_viewer::Color::from_rgba(230, 220, 255, 255),
            Self::Value(_r) => rvsdg_viewer::Color::from_rgba(150, 220, 150, 255),
            Self::Index(_r) => rvsdg_viewer::Color::from_rgba(150, 150, 150, 255),
            Self::UniformConstruct(_r) => rvsdg_viewer::Color::from_rgba(150, 150, 150, 255),
            Self::NonUniformConstruct(_r) => rvsdg_viewer::Color::from_rgba(150, 150, 150, 255),
            Self::Error { .. } => rvsdg_viewer::Color::from_rgba(255, 150, 100, 255),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum WasmTy {
    Defined { shape: TyShape, ty: walrus::ValType },
    Tuple(Vec<Self>),
    Callabale,
    Undefined,
}

impl TryFrom<vola_opt::common::Ty> for WasmTy {
    type Error = WasmError;
    fn try_from(value: vola_opt::common::Ty) -> Result<Self, Self::Error> {
        let (shape, ty) = match value {
            vola_opt::common::Ty::SCALAR_BOOL => (TyShape::Scalar, walrus::ValType::I32),
            vola_opt::common::Ty::SCALAR_INT => (TyShape::Scalar, walrus::ValType::I32),
            vola_opt::common::Ty::SCALAR_REAL => (TyShape::Scalar, walrus::ValType::F32),
            vola_opt::common::Ty::Shaped {
                ty: DataType::Integer,
                shape: Shape::Vec { width },
            } => (TyShape::Vector { width }, walrus::ValType::I32),
            vola_opt::common::Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Vec { width },
            } => (TyShape::Vector { width }, walrus::ValType::F32),
            vola_opt::common::Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Matrix { width, height },
            } => (TyShape::Matrix { width, height }, walrus::ValType::F32),
            vola_opt::common::Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Tensor { sizes },
            } => (
                TyShape::Tensor {
                    dim: sizes.into_iter().collect(),
                },
                walrus::ValType::F32,
            ),
            vola_opt::common::Ty::Tuple(_t) => {
                return Err(WasmError::Any(
                    format!("Tuple are not supported in WASM (yet).").into(),
                ));

                //let subtypes: Result<Vec<Self>, _> =
                //    t.into_iter().map(|t| Self::try_from(t)).collect();
                //return Ok(Self::Tuple(subtypes?));
            }
            vola_opt::common::Ty::Callable => return Ok(WasmTy::Callabale),
            other => return Err(WasmError::UnexpectedType(other)),
        };

        Ok(WasmTy::Defined { shape, ty })
    }
}

impl WasmTy {
    ///Turns a _index_ operation into a set of indices into this type.
    ///
    ///For example, indexing a vec3 with 1 returns 4, which is the offset
    ///of the second float.
    ///
    ///Doing the same for a mat3 returns three offsets for the
    ///4-th, 5-th & 6-th float, which is the second column of the vector
    pub fn index_to_offset_elements(&self, index: usize) -> SmallColl<u32> {
        match self {
            WasmTy::Defined { shape, .. } => {
                let per_element_offset = self.base_type_size().unwrap();
                match shape {
                    TyShape::Scalar => panic!("cannot index scalar"),
                    TyShape::Vector { width } => {
                        assert!(*width > index, "Out of bound vector index");
                        smallvec![(index * per_element_offset).try_into().unwrap()]
                    }
                    TyShape::Matrix { width, height } => {
                        assert!(*width > index);
                        //load all indices of the n-th column.
                        //the base offset is index-times the element count of each
                        //column times the element size
                        let base_offset = per_element_offset * height * index;
                        let mut indices = SmallColl::new();
                        for idx in 0..*height {
                            indices
                                .push((base_offset + per_element_offset * idx).try_into().unwrap());
                        }

                        indices
                    }
                    TyShape::Tensor { .. } => panic!("Tensor not yet supported"),
                }
            }
            WasmTy::Tuple(ts) => ts[index].element_offsets(),
            WasmTy::Callabale => SmallColl::new(),
            WasmTy::Undefined => SmallColl::new(),
        }
    }

    ///Returns the offset for each _atomic_ element of this type
    pub fn element_offsets(&self) -> SmallColl<u32> {
        match self {
            Self::Defined { shape, ty: _ } => {
                let element_size = self.base_type_size().unwrap() as u32;
                let mut offsets = SmallColl::default();
                for c in 0..shape.element_count() {
                    offsets.push(c as u32 * element_size);
                }
                offsets
            }
            Self::Tuple(t) => {
                let mut local = 0u32;
                let mut offsets = SmallColl::default();
                for ele in t {
                    let local_offset = local;
                    let sub_elements = ele.element_offsets();
                    for sub in sub_elements {
                        //move the element according to `local`
                        let element_offset = local_offset + sub;
                        offsets.push(element_offset);
                    }
                    //now move the local pointer the size of ele
                    local += ele.wasm_size() as u32;
                }
                offsets
            }
            Self::Callabale | Self::Undefined => SmallColl::new(),
        }
    }

    pub fn is_vector(&self) -> bool {
        if let Self::Defined {
            shape: TyShape::Vector { .. },
            ..
        } = self
        {
            true
        } else {
            false
        }
    }

    pub fn is_scalar(&self) -> bool {
        if let Self::Defined { shape, .. } = self {
            if let TyShape::Scalar = shape {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn store_kind(&self, element_index: usize) -> StoreKind {
        match self.unwarp_walrus_ty(element_index) {
            ValType::F32 => StoreKind::F32,
            ValType::I32 => StoreKind::I32 { atomic: false },
            ValType::F64 => StoreKind::F64,
            ValType::I64 => StoreKind::I64 { atomic: false },
            //TODO: is this right, or should we panic?
            ValType::Ref(_) => StoreKind::I32 { atomic: false },
            ValType::V128 => StoreKind::V128,
        }
    }

    pub fn load_kind(&self, element_index: usize) -> LoadKind {
        match self.unwarp_walrus_ty(element_index) {
            ValType::F32 => LoadKind::F32,
            ValType::I32 => LoadKind::I32 { atomic: false },
            ValType::F64 => LoadKind::F64,
            ValType::I64 => LoadKind::I64 { atomic: false },
            //TODO: is this right, or should we panic?
            ValType::Ref(_) => LoadKind::I32 { atomic: false },
            ValType::V128 => LoadKind::V128,
        }
    }

    pub fn base_type_size(&self) -> Option<usize> {
        match self {
            Self::Defined { shape: _, ty } => match ty {
                ValType::F32 | ValType::I32 | ValType::Ref(_) => Some(4),
                ValType::F64 | ValType::I64 => Some(8),
                ValType::V128 => Some(16),
            },
            Self::Tuple(_t) => None,
            Self::Callabale => None,
            Self::Undefined => None,
        }
    }

    ///Returns the size (in bytes) of this type for wasm.
    pub fn wasm_size(&self) -> usize {
        match self {
            WasmTy::Defined { shape, ty: _ } => {
                //NOTE: unwrap can't panic, cause basetype will always be defined
                let base_type_size = self.base_type_size().unwrap();
                let element_count = shape.element_count();
                base_type_size * element_count
            }
            WasmTy::Tuple(t) => {
                let size = t.iter().fold(0, |last, t| last + t.wasm_size());
                size
            }
            WasmTy::Callabale => 0,
            WasmTy::Undefined => 0,
        }
    }

    pub fn new_with_shape(ty: walrus::ValType, shape: TyShape) -> Self {
        Self::Defined { shape, ty }
    }

    pub fn element_count(&self) -> usize {
        match self {
            Self::Defined { shape, ty: _ } => shape.element_count(),
            Self::Tuple(t) => t.iter().fold(0, |a, t| a + t.element_count()),
            Self::Callabale => 0,
            Self::Undefined => 0,
        }
    }

    pub fn unwarp_walrus_ty(&self, index: usize) -> walrus::ValType {
        match self {
            Self::Defined { shape, ty } => {
                assert!(shape.element_count() > index);
                ty.clone()
            }
            Self::Tuple(t) => {
                let overall_count = self.element_count();
                let mut element_counter = 0;
                let mut tuple_index = 0;
                loop {
                    let next_element_count = t[tuple_index].element_count();
                    //println!("{:?} has {} elements", t[tuple_index], next_element_count);
                    if (element_counter + next_element_count) > index {
                        //must be in here, find local offset and unwrap the type
                        let local_index = index - element_counter;
                        //not try unwrap on that
                        return t[tuple_index].unwarp_walrus_ty(local_index);
                    } else {
                        //go to next tuple element
                        element_counter += next_element_count;
                        tuple_index += 1;
                    }

                    if element_counter >= overall_count {
                        break;
                    }
                }
                panic!(
                    "Could not find element {index} in tuple {:?} (overall: {overall_count})",
                    self
                );
            }
            _ => panic!("cannot unwrap {:?} into WASM type", self),
        }
    }

    pub fn append_elements_to_signature(&self, signature: &mut SmallColl<walrus::ValType>) {
        match self {
            Self::Callabale => {}
            Self::Undefined => {}
            Self::Tuple(t) => {
                for st in t {
                    st.append_elements_to_signature(signature);
                }
            }
            Self::Defined { shape, ty } => {
                for _ in 0..shape.element_count() {
                    signature.push(ty.clone());
                }
            }
        }
    }

    pub fn as_runtime_type_string(&self) -> Result<&str, WasmError> {
        if let WasmTy::Defined { shape, ty } = self {
            match ty {
                ValType::F32 => {
                    match shape {
                        TyShape::Scalar => Ok("scalar"),
                        TyShape::Vector { width } => match width {
                            2 => Ok("vec2"),
                            3 => Ok("vec3"),
                            4 => Ok("vec4"),
                            _ => Err(WasmError::RuntimeUndefinedType(self.clone())),
                        },
                        TyShape::Matrix { width, height } => match (width, height) {
                            (2, 2) => Ok("mat2"),
                            (3, 3) => Ok("mat3"),
                            (4, 4) => Ok("mat4"),
                            _ => Err(WasmError::RuntimeUndefinedType(self.clone())),
                        },
                        //NOTE: we currently have no vector typed extern functions
                        _ => Err(WasmError::RuntimeUndefinedType(self.clone())),
                    }
                }
                ValType::I32 => {
                    match shape {
                        TyShape::Scalar => Ok("int"),
                        TyShape::Vector { width } => match width {
                            2 => Ok("ivec2"),
                            3 => Ok("ivec3"),
                            4 => Ok("ivec4"),
                            _ => Err(WasmError::RuntimeUndefinedType(self.clone())),
                        },
                        TyShape::Matrix { width, height } => match (width, height) {
                            (2, 2) => Ok("imat2"),
                            (3, 3) => Ok("imat3"),
                            (4, 4) => Ok("imat4"),
                            _ => Err(WasmError::RuntimeUndefinedType(self.clone())),
                        },
                        //NOTE: we currently have no vector typed extern functions
                        _ => Err(WasmError::RuntimeUndefinedType(self.clone())),
                    }
                }
                _ => Err(WasmError::RuntimeUndefinedType(self.clone())),
            }
        } else {
            Err(WasmError::RuntimeUndefinedType(self.clone()))
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyShape {
    Scalar,
    Vector { width: usize },
    Matrix { width: usize, height: usize },
    Tensor { dim: SmallColl<usize> },
}

impl TyShape {
    pub fn element_count(&self) -> usize {
        match self {
            Self::Scalar => 1,
            Self::Vector { width } => *width,
            Self::Matrix { width, height } => width * height,
            Self::Tensor { dim } => dim.iter().fold(1, |x, y| x * y),
        }
    }
}

impl Display for TyShape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar => write!(f, "Scalar"),
            Self::Vector { width } => write!(f, "Vector{width}"),
            Self::Matrix { width, height } => write!(f, "Mat{width}x{height}"),
            Self::Tensor { dim } => write!(f, "Tensor[{dim:?}]"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum WasmEdge {
    State,
    Value(WasmTy),
}

impl WasmEdge {
    pub fn type_or_undefined(&self) -> WasmTy {
        match self {
            Self::State => WasmTy::Undefined,
            Self::Value(t) => t.clone(),
        }
    }
}

impl LangEdge for WasmEdge {
    fn state_edge() -> Self {
        Self::State
    }

    fn value_edge() -> Self {
        Self::Value(WasmTy::Undefined)
    }

    fn is_state_edge(&self) -> bool {
        if let Self::State = self {
            true
        } else {
            false
        }
    }

    fn is_value_edge(&self) -> bool {
        if let Self::Value { .. } = self {
            true
        } else {
            false
        }
    }
}

#[cfg(feature = "viewer")]
impl View for WasmEdge {
    fn name(&self) -> String {
        match self {
            Self::State => format!("State"),
            Self::Value(WasmTy::Defined { shape, ty }) => format!("{shape}<{ty:?}>"),
            Self::Value(WasmTy::Undefined) => format!("Undefined"),
            Self::Value(WasmTy::Callabale) => format!("Callable"),
            Self::Value(WasmTy::Tuple(t)) => format!("Tuple({t:?}"),
        }
    }

    fn color(&self) -> rvsdg_viewer::Color {
        match self {
            Self::State => rvsdg_viewer::Color::RED,
            Self::Value { .. } => rvsdg_viewer::Color::BLACK,
        }
    }
}
