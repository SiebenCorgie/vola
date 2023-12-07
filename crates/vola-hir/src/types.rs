use tinyvec::TinyVec;
use vola_ast::common::Ty;

use crate::ast;

///Primitive types of the HIR type system. Each type contains the
/// width. So Float(32) is a 32bit float.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum HirTypePrim {
    Float(u16),
    Int { width: u16, signed: bool },
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum HirTypeShape {
    Scalar,
    Vec(usize),
    Mat { width: usize, height: usize },
}
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct HirType {
    shape: HirTypeShape,
    prim: HirTypePrim,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum HirTypeState {
    Assigned(HirType),
    Infered(HirType),
    None,
}

impl Default for HirTypeState {
    fn default() -> Self {
        HirTypeState::None
    }
}

impl From<Ty> for HirType {
    fn from(value: Ty) -> Self {
        //NOTE right now we just map everything to f32
        match value {
            Ty::Scalar => HirType {
                shape: HirTypeShape::Scalar,
                prim: HirTypePrim::Float(32),
            },
            Ty::Vector { count } => HirType {
                shape: HirTypeShape::Vec(count),
                prim: HirTypePrim::Float(32),
            },
            Ty::Mat { width, height } => HirType {
                shape: HirTypeShape::Mat { width, height },
                prim: HirTypePrim::Float(32),
            },
        }
    }
}
