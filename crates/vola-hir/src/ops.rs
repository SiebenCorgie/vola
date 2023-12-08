use std::fmt::Display;

use rvsdg::{
    nodes::LangNode,
    region::{Input, Output},
};
use rvsdg_viewer::View;
use tinyvec::TinyVec;
use vola_common::Span;

///This enum describes all non-rvsdg structural nodes we can build. Since Vola is a
/// volume DSL, most of this is algebraic in nature.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
    ImmFloat(u32),
    ImmInt(isize),
    ///Ops that assambles a constant from immediate values.
    ConstAssamble,
}

impl AsRef<str> for HirOpTy {
    fn as_ref(&self) -> &str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Sqrt => "sqrt",
            Self::Sin => "sin",
            Self::Cos => "cos",
            Self::EucLength => "length",
            Self::Mod => "%",
            Self::PrimInstance => "PrimInstance",
            Self::ArgLoad => "ArgLoad",
            Self::ArgStore => "ArgStore",
            Self::AtLoad => "@ Load",
            Self::AtStore => "@ Store",
            Self::ImmFloat(_) => "ImmFloat",
            Self::ImmInt(_) => "ImmInt",
            Self::ConstAssamble => "ConstAssamble",
        }
    }
}

impl Display for HirOpTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

pub struct HirOp {
    ///source span this Op originates from
    pub span: Span,
    ///The flat op type
    pub op_ty: HirOpTy,

    pub inputs: TinyVec<[Input; 2]>,
    pub outputs: TinyVec<[Output; 2]>,
}

impl HirOp {
    pub fn new(span: Span, op_ty: HirOpTy) -> Self {
        HirOp {
            span,
            op_ty,
            inputs: TinyVec::default(),
            outputs: TinyVec::default(),
        }
    }

    ///Configures the given number of inputs / outputs
    pub fn with_inout(mut self, input_count: usize, output_count: usize) -> Self {
        for _ in 0..input_count {
            self.inputs.push(Input::default());
        }
        for _ in 0..output_count {
            self.outputs.push(Output::default());
        }

        self
    }
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

impl View for HirOp {
    fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
        rvsdg_viewer::Color {
            r: 0.9,
            g: 0.8,
            b: 0.1,
            a: 1.0,
        }
    }
    fn name(&self) -> &str {
        self.op_ty.as_ref()
    }
    fn stroke(&self) -> rvsdg_viewer::Stroke {
        rvsdg_viewer::Stroke::Line
    }
}
