use rvsdg::{
    nodes::LangNode,
    region::{Input, Output},
};
use tinyvec::TinyVec;
use vola_common::Span;

///This enum describes all non-rvsdg structural nodes we can build. Since Vola is a
/// volume DSL, most of this is algebraic in nature.
pub enum HirOpTy {
    //Mathy ops
    Add,
    Sub,
    Mul,
    Div,
    Sqrt,
    Sin,
    Cos,
    //euclidean length of a vector
    EucLength,
    Mod,
    //TODO add some more function call like stuff?

    //Defines a new primitive instance. Emited on
    // `eval x -> y` calls for `y`. translates into:
    //```
    // PrimInstance y;
    // t0 = call x();
    // y.@ = t0.
    // ```
    PrimInstance,

    //data access
    ArgStore,
    ArgLoad,
    AtLoad,
    AtStore,

    //constants related
    ImmFloat(f32),
    ImmInt(isize),
    ///Ops that assambles a constant from immediate values.
    ConstAssamble,
}

pub struct HirOp {
    ///source span this Op originates from
    pub span: Span,
    ///The flat op type
    pub op_ty: HirOpTy,

    pub inputs: TinyVec<[Input; 2]>,
    pub outputs: TinyVec<[Output; 2]>,
}

impl LangNode for HirOp {
    fn inputs(&self) -> &[Input] {
        &self.inputs
    }
    fn inputs_mut(&mut self) -> &mut [Input] {
        &mut self.inputs
    }
    fn outputs(&self) -> &[Output] {
        &self.outputs
    }
    fn outputs_mut(&mut self) -> &mut [Output] {
        &mut self.outputs
    }
}
