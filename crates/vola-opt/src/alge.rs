//! # Alge dialect
//!

use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
};
use rvsdg_viewer::View;

use crate::DialectNode;

pub(crate) mod implblock;

//Macro that implements the "View" trait for an AlgeDialect op
macro_rules! implViewAlgeOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
                rvsdg_viewer::macroquad::color::Color::from_rgba(128, 64, 64, 255)
            }

            fn name(&self) -> String {
                format!($str, $(self.$arg)*,)
            }

            fn stroke(&self) -> rvsdg_viewer::Stroke {
                rvsdg_viewer::Stroke::Line
            }
        }
    };
}

///Simple unary op in the alge dialect
#[derive(LangNode, Debug, Clone)]
pub struct UnaryOp {
    #[input]
    input: Input,
    #[output]
    output: Output,

    op: vola_ast::alge::UnaryOp,
}

impl UnaryOp {
    pub fn new(op: vola_ast::alge::UnaryOp) -> Self {
        UnaryOp {
            input: Input::default(),
            output: Output::default(),
            op,
        }
    }
}

implViewAlgeOp!(UnaryOp, "Unary {:?}", op);

impl DialectNode for UnaryOp {
    fn dialect(&self) -> &'static str {
        "alge"
    }
}
