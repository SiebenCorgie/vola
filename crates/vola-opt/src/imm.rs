/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! The immediate value dialect. Takes care of representing typed constant values as well as constant-folding of those.

use ahash::AHashMap;
use rvsdg::{attrib::FlagStore, region::Output, rvsdg_derive_lang::LangNode};
use vola_ast::csg::{CSGConcept, CSGNodeDef};

use crate::{common::Ty, DialectNode, OptError, OptGraph, OptNode};
use rvsdg_viewer::Color;

//Macro that implements the "View" trait for an AlgeDialect op
macro_rules! implViewImmOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> Color {
                Color::from_rgba(170, 170, 200, 255)
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
                Color::from_rgba(170, 170, 200, 255)
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

implViewImmOp!(ImmScalar, "{}f", lit);
impl DialectNode for ImmScalar {
    fn dialect(&self) -> &'static str {
        "Imm"
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

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<ImmScalar>() {
            other_cop.lit == self.lit
        } else {
            false
        }
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

implViewImmOp!(ImmNat, "{}i", lit);
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

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<ImmNat>() {
            other_cop.lit == self.lit
        } else {
            false
        }
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
