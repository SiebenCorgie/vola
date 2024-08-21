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

use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
};

use crate::{common::Ty, DialectNode, OptError, OptNode};

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

impl Default for AutoDiff {
    fn default() -> Self {
        AutoDiff {
            inputs: [Input::default(), Input::default()],
            output: Output::default(),
        }
    }
}

implViewAutoDiff!(AutoDiff, "∂");

impl DialectNode for AutoDiff {
    fn dialect(&self) -> &'static str {
        "AutoDiff"
    }

    fn try_derive_type(
        &self,
        _typemap: &rvsdg::attrib::FlagStore<Ty>,
        graph: &crate::OptGraph,
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CSGConcept>,
        _csg_defs: &ahash::AHashMap<String, vola_ast::csg::CSGNodeDef>,
    ) -> Result<Option<Ty>, OptError> {
        //By definition we only allow expressions that
        //output scalars or vectors.
        //We also only allow _wrt_ args that are scalars or vectors.
        //
        //For those, the return type is _easy_, just the cross of its element products

        let expr_type = if let Some(expr_edg) = self.inputs[0].edge {
            if let Some(ty) = graph.edge(expr_edg).ty.get_type() {
                ty.clone()
            } else {
                return Ok(None);
            }
        } else {
            return Err(OptError::TypeDeriveError {
                text: format!("diff() had no expression to differentiate"),
            });
        };

        //Make sure the expression has either scalar or vector type
        if !expr_type.is_scalar() || !expr_type.is_vector() {
            return Err(OptError::TypeDeriveError { text: format!("diff() can only be applied to expressions of type Scalar or Vector, expression had {}", expr_type) });
        }

        let wrt_type = if let Some(wrt_edge) = self.inputs[1].edge {
            if let Some(ty) = graph.edge(wrt_edge).ty.get_type() {
                ty.clone()
            } else {
                return Ok(None);
            }
        } else {
            return Err(OptError::TypeDeriveError {
                text: format!("diff() wrt-argument was not set!"),
            });
        };

        //Make sure the wrt has either scalar or vector type
        if !wrt_type.is_scalar() || !wrt_type.is_vector() {
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
            (Ty::Scalar, Ty::Scalar) => Ok(Some(Ty::Scalar)),
            (Ty::Vector { width }, Ty::Scalar) | (Ty::Scalar, Ty::Vector { width }) => {
                Ok(Some(Ty::Vector { width }))
            }
            (Ty::Vector { width: expr_width }, Ty::Vector { width: wrt_width }) => {
                Ok(Some(Ty::Matrix {
                    width: wrt_width,
                    height: expr_width,
                }))
            }
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
