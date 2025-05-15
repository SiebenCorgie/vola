/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * 2024 Tendsin Mende
 */

//! The immediate value dialect. Takes care of representing typed constant values as well as constant-folding of those.

use ahash::AHashMap;
use rvsdg::{region::Output, rvsdg_derive_lang::LangNode, smallvec::SmallVec, SmallColl};
use vola_ast::csg::{CsgConcept, CsgDef};

use crate::{
    common::{DataType, Shape, Ty},
    DialectNode, OptError, OptNode,
};
use rvsdg_viewer::Color;

//Macro that implements the "View" trait for the ImmDialect
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
        _inputs: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
        _csg_defs: &ahash::AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //NOTE: all literals are translated to a _scalar_
        Ok(Ty::scalar_type(DataType::Real))
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

///An immediate vector.
#[derive(LangNode, Debug)]
pub struct ImmVector {
    ///The immediate value
    pub lit: SmallColl<f64>,
    ///the output port the `lit` value is passed down to.
    #[output]
    pub out: Output,
}

impl ImmVector {
    pub fn new(elements: &[f64]) -> Self {
        ImmVector {
            lit: SmallVec::from_slice(elements),
            out: Output::default(),
        }
    }
}

implViewImmOp!(ImmVector, "{:?}", lit);
impl DialectNode for ImmVector {
    fn dialect(&self) -> &'static str {
        "Imm"
    }

    fn try_derive_type(
        &self,
        _inputs: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
        _csg_defs: &ahash::AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //NOTE: all literals are translated to a _scalar_
        Ok(Ty::Shaped {
            ty: DataType::Real,
            shape: Shape::Vec {
                width: self.lit.len(),
            },
        })
    }

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<ImmVector>() {
            other_cop.lit == self.lit
        } else {
            false
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(ImmVector {
                lit: self.lit.clone(),
                out: Output::default(),
            }),
        }
    }
}

///An immediate vector.
#[derive(LangNode, Debug)]
pub struct ImmMatrix {
    width: usize,
    height: usize,
    ///The immediate value
    pub lit: SmallColl<SmallColl<f64>>,
    ///the output port the `lit` value is passed down to.
    #[output]
    pub out: Output,
}

impl ImmMatrix {
    pub fn new(columns: SmallColl<SmallColl<f64>>) -> Self {
        //We are as wide as we have columns, and are as heigh has each column has elements.
        //see
        // UniformConstruct::try_derive_type for more information
        let width = columns.len();
        let height = columns[0].len();

        for col in &columns {
            assert!(col.len() == height)
        }

        ImmMatrix {
            width,
            height,
            lit: columns,
            out: Output::default(),
        }
    }
}

implViewImmOp!(ImmMatrix, "{:?}", lit);
impl DialectNode for ImmMatrix {
    fn dialect(&self) -> &'static str {
        "Imm"
    }

    fn try_derive_type(
        &self,
        _inputs: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
        _csg_defs: &ahash::AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //NOTE: all literals are translated to a _scalar_
        Ok(Ty::Shaped {
            ty: DataType::Real,
            shape: Shape::Matrix {
                width: self.width,
                height: self.height,
            },
        })
    }

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<ImmMatrix>() {
            other_cop.lit == self.lit
        } else {
            false
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(ImmMatrix {
                width: self.width,
                height: self.height,
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
        "Imm"
    }

    fn try_derive_type(
        &self,
        _inputs: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
        _csg_defs: &ahash::AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //NOTE: all literals are translated to a _scalar_
        Ok(Ty::scalar_type(DataType::Integer))
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

///An immediate bool.
#[derive(LangNode, Debug)]
pub struct ImmBool {
    ///The immediate value
    pub lit: bool,
    ///the output port the `lit` value is passed down to.
    #[output]
    pub out: Output,
}

impl ImmBool {
    pub fn new(lit: bool) -> Self {
        ImmBool {
            lit,
            out: Output::default(),
        }
    }
}

implViewImmOp!(ImmBool, "{}", lit);
impl DialectNode for ImmBool {
    fn dialect(&self) -> &'static str {
        "Imm"
    }

    fn try_derive_type(
        &self,
        _inputs: &[Ty],
        _concepts: &AHashMap<String, CsgConcept>,
        _csg_defs: &ahash::AHashMap<String, CsgDef>,
    ) -> Result<Ty, OptError> {
        //NOTE: all literals are translated to a _scalar_
        Ok(Ty::scalar_type(DataType::Bool))
    }

    fn is_operation_equal(&self, other: &OptNode) -> bool {
        if let Some(other_cop) = other.try_downcast_ref::<ImmBool>() {
            other_cop.lit == self.lit
        } else {
            false
        }
    }

    fn structural_copy(&self, span: vola_common::Span) -> OptNode {
        OptNode {
            span,
            node: Box::new(ImmBool {
                lit: self.lit.clone(),
                out: Output::default(),
            }),
        }
    }
}
