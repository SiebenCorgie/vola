/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

use ahash::AHashMap;
use rvsdg::{
    attrib::FlagStore,
    edge::{OutportLocation, OutputType},
    smallvec::SmallVec,
    NodeRef, SmallColl,
};
use std::fmt::Display;
use vola_ast::csg::{CSGConcept, CsgDef, ImplBlock};

use crate::{error::OptError, OptGraph};

///All data-types we might encounter in the optimizer
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataType {
    Void,
    Csg,
    Real,
    Integer,
    Bool,
    Complex,
    Quaternion,
}

impl From<vola_ast::common::DataTy> for DataType {
    fn from(value: vola_ast::common::DataTy) -> Self {
        match value {
            vola_ast::common::DataTy::Void => Self::Void,
            vola_ast::common::DataTy::Bool => Self::Bool,
            vola_ast::common::DataTy::Csg => Self::Csg,
            vola_ast::common::DataTy::Real => Self::Real,
            vola_ast::common::DataTy::Integer => Self::Integer,
            vola_ast::common::DataTy::Complex => Self::Complex,
            vola_ast::common::DataTy::Quaternion => Self::Quaternion,
        }
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Void => write!(f, "Void"),
            Self::Bool => write!(f, "Bool"),
            Self::Csg => write!(f, "Csg"),
            Self::Real => write!(f, "Real"),
            Self::Integer => write!(f, "Integer"),
            Self::Complex => write!(f, "Complex"),
            Self::Quaternion => write!(f, "Quaternion"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Shape {
    Scalar,
    Interval,
    Vec { width: usize },
    Matrix { width: usize, height: usize },
    Tensor { sizes: SmallVec<[usize; 8]> },
}

impl Shape {
    ///Tries to build the new shape, based on the given index
    pub(crate) fn try_derive_access_index(&self, index: usize) -> Result<Shape, OptError> {
        match self {
            Self::Interval => {
                if index <= 1 {
                    Ok(Self::Scalar)
                } else {
                    Err(OptError::Any {
                    text: format!("Interval cannot be indexed with {}, only 0 for the lower bound, and 1 for the upper bound are valid", index),
                    })
                }
            }
            Self::Vec { width } => {
                if index >= *width {
                    Err(OptError::Any {
                        text: format!("Vector of width {width} cannot be indexed with {index}"),
                    })
                } else {
                    //Otherwise always resolves to an scalar
                    Ok(Self::Scalar)
                }
            }
            Self::Matrix { width, height } => {
                if index >= *width {
                    Err(OptError::Any {
                        text: format!("Matrix {width}x{height} cannot be indexed with {index}"),
                    })
                } else {
                    Ok(Self::Vec { width: *height })
                }
            }
            Self::Tensor { sizes } => match sizes.len() {
                0 => Err(OptError::Any {
                    text: "Encountered zero dimensional tensor!".to_owned(),
                }),
                1 => Self::Vec { width: sizes[0] }.try_derive_access_index(index),
                2 => Self::Matrix {
                    width: sizes[1],
                    height: sizes[0],
                }
                .try_derive_access_index(index),
                _any => {
                    if index >= sizes[0] {
                        Err(OptError::Any {
                            text: format!(
                                "Cannot index tensor dimension of width={} with {index}",
                                sizes[0]
                            ),
                        })
                    } else {
                        let new_dim = sizes[1..].iter().map(|d| *d).collect();
                        Ok(Self::Tensor { sizes: new_dim })
                    }
                }
            },
            other_ty => Err(OptError::Any {
                text: format!("Cannot index into {:?}", other_ty),
            }),
        }
    }
}

impl From<vola_ast::common::Shape> for Shape {
    fn from(value: vola_ast::common::Shape) -> Self {
        match value {
            vola_ast::common::Shape::Interval => Self::Interval,
            vola_ast::common::Shape::Vec { width } => Self::Vec { width },
            vola_ast::common::Shape::Matrix { width, height } => Self::Matrix { width, height },
            vola_ast::common::Shape::Tensor { sizes } => Self::Tensor { sizes },
        }
    }
}

impl Display for Shape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Scalar => write!(f, "scalar"),
            Self::Interval => write!(f, "interval"),
            Self::Vec { width } => write!(f, "vec{width}"),
            Self::Matrix { width, height } => {
                if width == height {
                    write!(f, "mat{width}")
                } else {
                    write!(f, "mat{width}x{height}")
                }
            }
            Self::Tensor { sizes } => {
                write!(f, "tensor<")?;
                let mut is_first = true;
                for t in sizes {
                    if is_first {
                        is_first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{t}")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Ty {
    Callable,
    Shaped {
        ty: DataType,
        shape: Shape,
    },
    ///Aggregate type in the form of a tuple
    Tuple(Vec<Self>),
}

impl From<vola_ast::common::Ty> for Ty {
    fn from(value: vola_ast::common::Ty) -> Self {
        match value {
            vola_ast::common::Ty::Shaped { ty, shape } => Self::Shaped {
                ty: ty.into(),
                shape: shape.into(),
            },
            vola_ast::common::Ty::Simple(ty) => Self::Shaped {
                ty: ty.into(),
                shape: Shape::Scalar,
            },
            vola_ast::common::Ty::Tuple(tys) => {
                Self::Tuple(tys.into_iter().map(|t| t.into()).collect())
            }
        }
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Callable => write!(f, "Callable"),
            Self::Shaped { ty, shape } => {
                write!(f, "{shape}<{ty}>")
            }
            Self::Tuple(ts) => {
                write!(f, "(")?;
                let mut is_first = true;
                for s in ts {
                    if is_first {
                        is_first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{s}")?;
                }
                Ok(())
            }
        }
    }
}

impl Ty {
    pub const SCALAR_BOOL: Self = Self::scalar_type(DataType::Bool);
    pub const SCALAR_REAL: Self = Self::Shaped {
        ty: DataType::Real,
        shape: Shape::Scalar,
    };
    pub const SCALAR_INT: Self = Self::Shaped {
        ty: DataType::Integer,
        shape: Shape::Scalar,
    };
    pub const CSG: Self = Self::Shaped {
        ty: DataType::Csg,
        shape: Shape::Scalar,
    };

    pub const fn shaped(data_ty: DataType, shape: Shape) -> Self {
        Self::Shaped { ty: data_ty, shape }
    }

    pub const fn scalar_type(data_ty: DataType) -> Self {
        Self::Shaped {
            ty: data_ty,
            shape: Shape::Scalar,
        }
    }

    pub const fn vector_type(data_ty: DataType, width: usize) -> Self {
        Self::Shaped {
            ty: data_ty,
            shape: Shape::Vec { width },
        }
    }

    pub const fn matrix_type(data_ty: DataType, width: usize, height: usize) -> Self {
        Self::Shaped {
            ty: data_ty,
            shape: Shape::Matrix { width, height },
        }
    }

    ///Returns the singular data-type, if this is either `Simple` or `Shaped`.
    pub fn data_type(&self) -> Option<DataType> {
        match self {
            Self::Shaped { ty, shape: _ } => Some(*ty),
            _ => None,
        }
    }

    ///Returns the shape of this type, if there is any. None for CSG, Simple and tuple
    pub fn shape(&self) -> Option<Shape> {
        match self {
            Self::Shaped { ty: _, shape } => Some(shape.clone()),
            _ => None,
        }
    }

    pub fn is_bool(&self) -> bool {
        self.data_type() == Some(DataType::Bool)
    }
    ///Returns true for real, integer, quaternion and complex data-types
    pub fn is_algebraic(&self) -> bool {
        if let Some(ty) = self.data_type() {
            match ty {
                DataType::Real | DataType::Integer | DataType::Quaternion | DataType::Complex => {
                    true
                }
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn is_integer(&self) -> bool {
        self.data_type() == Some(DataType::Integer)
    }

    ///Returns true, if self is of "scalar" shape
    pub fn is_scalar(&self) -> bool {
        if let Some(Shape::Scalar) = self.shape() {
            true
        } else {
            false
        }
    }

    pub fn is_vector(&self) -> bool {
        if let Some(Shape::Vec { .. }) = self.shape() {
            true
        } else {
            false
        }
    }

    pub fn is_matrix(&self) -> bool {
        if let Some(Shape::Matrix { .. }) = self.shape() {
            true
        } else {
            false
        }
    }

    pub fn is_tensor(&self) -> bool {
        if let Some(Shape::Tensor { .. }) = self.shape() {
            true
        } else {
            false
        }
    }

    ///If the type has a width, returns it. This is either the vector's or matrix's width,
    ///or the tensors first dimension.
    pub fn width(&self) -> Option<usize> {
        if let Self::Shaped { shape, .. } = self {
            match shape {
                Shape::Vec { width } => Some(*width),
                Shape::Matrix { width, .. } => Some(*width),
                Shape::Tensor { sizes } => sizes.get(0).cloned(),
                _ => None,
            }
        } else {
            None
        }
    }

    ///If the type has a height, returns it. This is either the matrix's height,
    ///or the tensors second dimension.
    pub fn height(&self) -> Option<usize> {
        if let Self::Shaped { shape, .. } = self {
            match shape {
                Shape::Matrix { height, .. } => Some(*height),
                Shape::Tensor { sizes } => sizes.get(1).cloned(),
                _ => None,
            }
        } else {
            None
        }
    }

    ///Tries to derive a type that would be produced by indexing with `index` into the `Ty`.
    pub(crate) fn try_derive_access_index(&self, index: usize) -> Result<Ty, OptError> {
        match self {
            //Basically a scalar at this point, so we can't index
            Self::Shaped { ty, shape } => {
                let sub_shape = shape.try_derive_access_index(index)?;
                Ok(Self::Shaped {
                    ty: ty.clone(),
                    shape: sub_shape,
                })
            }
            Self::Tuple(t) => {
                if let Some(inner) = t.get(index) {
                    Ok(inner.clone())
                } else {
                    Err(OptError::Any {
                        text: format!("Tuple of size {} has no element at index {index}", t.len()),
                    })
                }
            }
            other_ty => Err(OptError::Any {
                text: format!("Cannot index into {:?}", other_ty),
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub port: OutportLocation,
    pub span: vola_common::Span,
}

#[derive(Debug, Clone)]
pub struct FnImport {
    pub port: OutportLocation,
    pub span: vola_common::Span,
    pub args: SmallColl<Ty>,
    pub ret: Ty,
}

///Helper utility that keeps track of defined variables.
#[derive(Debug, Clone)]
pub struct LmdContext {
    ///Maps a variable name to an Value outport.
    pub defined_vars: AHashMap<String, VarDef>,
    pub imported_functions: AHashMap<String, FnImport>,
}

impl LmdContext {
    pub(crate) fn merge_inner_ctx(&mut self, ctx: Self) {
        //merge imported functions
        for (key, fn_import) in ctx.imported_functions {
            if !self.imported_functions.contains_key(&key) {
                self.imported_functions.insert(key, fn_import);
            }
        }
    }

    pub fn empty() -> Self {
        LmdContext {
            defined_vars: AHashMap::new(),
            imported_functions: AHashMap::new(),
        }
    }

    pub fn new_for_impl_block(
        graph: &mut OptGraph,
        type_map: &mut FlagStore<Ty>,
        lmd: NodeRef,
        block: &ImplBlock,
        entity_or_op: &CsgDef,
        concept_def: &CSGConcept,
    ) -> Self {
        let mut defmap = AHashMap::default();

        //first append all _hidden_ operation variables, then all renamed
        // concept defs, (always tagging the types in the optimizer as well)

        for arg in entity_or_op.args.iter() {
            let arg_idx = graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_argument();
            let argport = OutportLocation {
                node: lmd,
                output: rvsdg::edge::OutputType::Argument(arg_idx),
            };
            defmap.insert(
                arg.ident.0.clone(),
                VarDef {
                    port: argport.clone(),
                    span: arg.span.clone(),
                },
            );
            //tag the type as well
            type_map.set(argport.into(), arg.ty.clone().into());
        }

        //lookup type in the concept definition
        let ty = concept_def.src_ty.clone();
        //now add to the node as argument as well
        let arg_idx = graph
            .node_mut(lmd)
            .node_type
            .unwrap_lambda_mut()
            .add_argument();
        let argport = OutportLocation {
            node: lmd,
            output: rvsdg::edge::OutputType::Argument(arg_idx),
        };

        //TODO use the actual correct span.
        defmap.insert(
            block.concept_arg_name.0.clone(),
            VarDef {
                port: argport.clone(),
                span: block.span.clone(),
            },
        );
        //tag the type as well
        type_map.set(argport.into(), ty.into());

        LmdContext {
            defined_vars: defmap,
            imported_functions: AHashMap::with_capacity(0),
        }
    }

    pub fn new_for_fn(
        graph: &mut OptGraph,
        type_map: &mut FlagStore<Ty>,
        lmd: NodeRef,
        alge_fn: &vola_ast::alge::Func,
    ) -> Self {
        let mut defined_vars = AHashMap::new();
        //Build the argument map, and setup the λ-Context
        for arg in &alge_fn.args {
            let arg_port = graph
                .node_mut(lmd)
                .node_type
                .unwrap_lambda_mut()
                .add_argument();

            let argport = OutportLocation {
                node: lmd,
                output: OutputType::Argument(arg_port),
            };
            defined_vars.insert(
                arg.ident.0.clone(),
                VarDef {
                    port: argport.clone(),
                    span: arg.span.clone(),
                },
            );
            type_map.set(argport.clone().into(), Ty::from(arg.clone().ty));
        }

        LmdContext {
            defined_vars,
            imported_functions: AHashMap::with_capacity(0),
        }
    }

    ///Checks if a variable with "name" already exists.
    pub fn var_exists(&self, name: &str) -> bool {
        self.defined_vars.contains_key(name)
    }

    ///Adds the VarDef. Panics if the var already existed, since we don't support shadowing atm.
    pub fn add_define(&mut self, name: String, def: VarDef) {
        let old = self.defined_vars.insert(name, def);
        assert!(old.is_none(), "Variable with that name already existed!");
    }

    pub fn known_function(&self, name: &String) -> Option<&FnImport> {
        self.imported_functions.get(name)
    }
}
