/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2025 Tendsin Mende
 */

//! Defines the interval-expand node as well as the interval-extension pass and the interval-to-tuple lowering pass

use rvsdg::{
    edge::InputType,
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    util::abstract_node_type::AbstractNodeType,
};
use thiserror::Error;

use crate::{common::Ty, DialectNode, OptError, OptNode};

pub mod extension;
pub mod lower_intervals;

#[derive(Debug, Error, Clone)]
pub enum IntervalError {
    #[error("Can not build interval over {0:?}")]
    UnsupportedNodeType(AbstractNodeType),
    #[error("Unsupported operation {0:?} in interval calculation.")]
    UnsupportedOp(String),
    #[error("Can not index interval with {0}. Must be either 0 (lower) or 1 (upper) bound")]
    InvalidIntervalIndex(usize),
    #[error("There is an interval-type in an exported function. This will be lowered to a tuple of the interval's data type.")]
    InExport,
}

//Macro that implements the "View" trait for the Interval
macro_rules! implViewInterval {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> Color {
                Color::from_rgba(210, 50, 150, 255)
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
                rvsdg_viewer::Color::from_rgba(210, 50, 150, 255)
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

///The `interval-extension` entry-point node
#[derive(LangNode, Debug)]
pub struct IntervalExtension {
    ///By Definition the first is the `expr` that is being analysed, the second input is the dynamic value, and the last is the interval, in which the expression is analysed.
    #[inputs]
    pub inputs: [Input; 3],
    ///Single output that collects / represents the interval of the first input-argument with respect the second argument within the given bound.
    #[output]
    pub output: Output,
}

impl IntervalExtension {
    pub fn expr_input() -> InputType {
        InputType::Input(0)
    }
    pub fn dynamic_input() -> InputType {
        InputType::Input(1)
    }

    pub fn interval() -> InputType {
        InputType::Input(2)
    }
}

impl Default for IntervalExtension {
    fn default() -> Self {
        IntervalExtension {
            inputs: [Input::default(), Input::default(), Input::default()],
            output: Output::default(),
        }
    }
}

implViewInterval!(IntervalExtension, "IntervalExtension");

impl DialectNode for IntervalExtension {
    fn dialect(&self) -> &'static str {
        "interval"
    }

    fn try_derive_type(
        &self,
        input_types: &[Ty],
        _concepts: &ahash::AHashMap<String, vola_ast::csg::CsgConcept>,
        _csg_defs: &ahash::AHashMap<String, vola_ast::csg::CsgDef>,
    ) -> Result<Ty, OptError> {
        assert_eq!(input_types.len(), 3, "should have 3 inputs");

        // Now we have to verify, that the input matches the expectations. I.e.
        // the dynamic value has an arithmetic type, and the dynamic parameter has the same
        // shape as the bounds. If so, return the Interval of the input param.

        if !input_types[0].is_shaped() || input_types[0] == Ty::CSG {
            return Err(OptError::TypeDeriveError {
                text: "Can only create intervals for (shaped) arithmetic types".to_owned(),
            });
        }

        //Make sure the dynamic-value and the interval-type match
        if let (dynty, Ty::Interval(ity)) = (&input_types[1], &input_types[2]) {
            if dynty != ity.as_ref() {
                Err(OptError::TypeDeriveError {
                    text: format!("Dynamic value does not match interval-type: {dynty} != {ity}"),
                })
            } else {
                //Its alright, therefore create the interval version
                // of the value's type
                Ok(Ty::Interval(Box::new(input_types[0].clone())))
            }
        } else {
            Err(OptError::TypeDeriveError {
                text: format!("Third parameter must be interval"),
            })
        }
    }

    fn is_operation_equal(&self, other: &crate::OptNode) -> bool {
        if let Some(_other_op) = other.try_downcast_ref::<IntervalExtension>() {
            true
        } else {
            false
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> crate::OptNode {
        OptNode::new(IntervalExtension::default(), span)
    }
}
