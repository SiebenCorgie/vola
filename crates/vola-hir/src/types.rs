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
