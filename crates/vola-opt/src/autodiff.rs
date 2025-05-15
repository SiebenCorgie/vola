/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! # Autodiff dialect
//!
//! Takes care of defining all nodes needed for our autodiff framework.
//!
//! The submoduls implement the actual autodiff pass as well a specific optimizations.

mod activity;
mod ad_dispatch;
mod ad_forward;
mod ad_utils;
mod canonicalize;
mod diff;

use rvsdg::{
    edge::{InportLocation, InputType, OutportLocation},
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::smallvec,
    util::abstract_node_type::AbstractNodeType,
    SmallColl,
};
use vola_common::thiserror::Error;

use crate::{
    common::{DataType, Shape, Ty},
    DialectNode, OptError, OptNode,
};

///Generic respons of a differentiation implementation of some node
pub struct AdResponse {
    ///Maps any generated differntial pairs as `(orginal_value, differential_value)`
    pub diff_mapping: SmallColl<(OutportLocation, OutportLocation)>,
    ///Signals for which Outport we need to know the derivative, and to which inport those must be connected.
    pub chained_derivatives: SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
}

impl AdResponse {
    pub fn new(src_output: OutportLocation, differentiated_output: OutportLocation) -> Self {
        Self {
            diff_mapping: smallvec![(src_output, differentiated_output)],
            chained_derivatives: SmallColl::new(),
        }
    }

    pub fn with_pairs(pairs: SmallColl<(OutportLocation, OutportLocation)>) -> Self {
        Self {
            diff_mapping: pairs,
            chained_derivatives: SmallColl::new(),
        }
    }

    pub fn with_chained_derivatives(
        mut self,
        derivatives: SmallColl<(OutportLocation, SmallColl<InportLocation>)>,
    ) -> Self {
        self.chained_derivatives = derivatives;
        self
    }

    #[allow(dead_code)]
    pub fn push_chain_derivatives(
        &mut self,
        derivative_src: OutportLocation,
        destinations: SmallColl<InportLocation>,
    ) {
        self.chained_derivatives
            .push((derivative_src, destinations));
    }
}

#[derive(Debug, Error, Clone)]
pub enum AutoDiffError {
    #[error("The activity port is not in the same region as the entrypoint node")]
    ActivityExplorationRegionError,
    #[error("Could not linearize AutoDiff entrypoint")]
    LinearizeAdFailed,
    #[error("WRT-Arg was empty")]
    EmptyWrtArg,
    #[error("Expr-Arg was empty")]
    EmptyExprArg,
    #[error("Forward AutoDiff traversal encountered unexpected node type: {0:?}")]
    FwadUnexpectedNodeType(AbstractNodeType),
    #[error("Forward AutoDiff traversal had no implementation for node: {0:?}")]
    NoAdImpl(String),
    #[error("Encountered AutoDiff node while building derivative.")]
    UnexpectedAutoDiffNode,
    #[error("\"{0:?}\" can not be differentiated")]
    UndiffNode(String),
    #[error("Failed to canonicalize: {0:?}")]
    CanonicalizationFailed(String),
    #[error("Port {0:?} was not yet handeled")]
    FwPortUnhandeled(OutportLocation),
    #[error("GammaExitVariable {0:?} was connected in one branch, but not in the other")]
    GammaExitInvalid(OutportLocation),
}

//Macro that implements the "View" trait for the Autodiff
macro_rules! implViewAutoDiff {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> Color {
                Color::from_rgba(100, 210, 100, 255)
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
            fn color(&self) -> rvsdg_viewer::Color {
                rvsdg_viewer::Color::from_rgba(100, 210, 100, 255)
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

///The `diff` entry-point node
#[derive(LangNode, Debug)]
pub struct AutoDiff {
    ///By Definition the first is the `expr` that is being differentiated, and the second input is one or more
    /// _wrt_ arguments (with-respect-to).
    #[inputs]
    pub inputs: [Input; 2],
    ///Single output that collects / represents the derivative(s) of the first input-argument with respect to all elements of the second input.
    #[output]
    pub output: Output,
}

impl AutoDiff {
    pub fn expr_input() -> InputType {
        InputType::Input(0)
    }
    pub fn wrt_input() -> InputType {
        InputType::Input(1)
    }
}

impl Default for AutoDiff {
    fn default() -> Self {
        AutoDiff {
            inputs: [Input::default(), Input::default()],
            output: Output::default(),
        }
    }
}

implViewAutoDiff!(AutoDiff, "AutoDiff");

impl DialectNode for AutoDiff {
    fn dialect(&self) -> &'static str {
        "AutoDiff"
    }

    fn try_derive_type(
        &self,
        input_types: &[Ty],
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CsgConcept>,
        _csg_defs: &ahash::AHashMap<String, vola_ast::csg::CsgDef>,
    ) -> Result<Ty, OptError> {
        //By definition we only allow expressions that
        //output scalars or vectors.
        //We also only allow _wrt_ args that are scalars or vectors.
        //
        //For those, the return type is _easy_, just the cross of its element products

        let expr_type = input_types[0].clone();

        //Make sure the expression has either scalar or vector type
        if !expr_type.is_scalar() && !expr_type.is_vector() {
            return Err(OptError::TypeDeriveError { text: format!("diff() can only be applied to expressions of type Scalar or Vector, expression had {}", expr_type) });
        }

        let wrt_type = input_types[1].clone();

        //Make sure the wrt has either scalar or vector type
        if !wrt_type.is_scalar() && !wrt_type.is_vector() {
            return Err(OptError::TypeDeriveError { text: format!("diff() can only be applied with respect to either Scalar or Vector, wrt-argument was {}", wrt_type) });
        }

        //types check out, construct the return type
        //
        //NOTE: We use the standard Jacobi construction, so the
        //      wrt-arguments change column-wise, and the input-arguments row-wise
        //      for a matrix.
        //
        //NOTE: For (scalar, vector) or (vector, scalar) pairs, we just construct the vector of return values.
        //      Mathematicaly speaking, (scalar, vector) should be a column vector, and (vector, scalar) should
        //      be a row-vector, but we don't have that concept, so we merge them just into _vector_.

        match (expr_type, wrt_type) {
            (Ty::SCALAR_REAL, Ty::SCALAR_REAL) => Ok(Ty::SCALAR_REAL),
            //diff(vec, real) or diff(real, vec).
            (
                Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width },
                },
                Ty::SCALAR_REAL,
            )
            | (
                Ty::SCALAR_REAL,
                Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width },
                },
            ) => Ok(Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Vec { width },
            }),
            //diff(vec, vec)
            (
                Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width: expr_width },
                },
                Ty::Shaped {
                    ty: DataType::Real,
                    shape: Shape::Vec { width: wrt_width },
                },
            ) => Ok(Ty::Shaped {
                ty: DataType::Real,
                shape: Shape::Matrix {
                    width: wrt_width,
                    height: expr_width,
                },
            }),
            _ => panic!("invalid type state while resolving AutoDiff type"),
        }
    }

    fn is_operation_equal(&self, other: &crate::OptNode) -> bool {
        if let Some(_other_op) = other.try_downcast_ref::<AutoDiff>() {
            true
        } else {
            false
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode::new(AutoDiff::default(), span)
    }

    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<OptNode>>],
    ) -> Option<OptNode> {
        //TODO: Can we do compile-time AD?
        None
    }
}
