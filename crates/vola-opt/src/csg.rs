//! # CSG Dialect

use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
};
use vola_ast::common::Ident;

use crate::DialectNode;

pub(crate) mod exportfn;
pub(crate) mod fielddef;

//Macro that implements the "View" trait for an AlgeDialect op
macro_rules! implViewCsgOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
                rvsdg_viewer::macroquad::color::Color::from_rgba(170, 200, 170, 255)
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
            fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
                rvsdg_viewer::macroquad::color::Color::from_rgba(170, 200, 170, 255)
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

///Highlevel CSG Op where the concept identifier is verified, but not yet specialize.
/// Used to build resolved.
#[derive(LangNode, Debug)]
pub struct CsgOp {
    pub op: String,

    pub subtree_count: usize,
    #[inputs]
    pub inputs: SmallVec<[Input; 2]>,
    #[output]
    pub output: Output,
}

impl CsgOp {
    pub fn new(op: Ident, subtree_count: usize, parameter_count: usize) -> Self {
        CsgOp {
            op: op.0,
            subtree_count,
            inputs: smallvec![Input::default(); parameter_count],
            output: Output::default(),
        }
    }
}

implViewCsgOp!(CsgOp, "{:?}", op);
impl DialectNode for CsgOp {
    fn dialect(&self) -> &'static str {
        "csg"
    }
}

///CsgTree call into a defined sub tree. Akin to a function call, but gets inlined at specialization-time.
#[derive(LangNode, Debug)]
pub struct CsgCall {
    ///The field that thas is called
    pub op: String,

    #[inputs]
    pub inputs: SmallVec<[Input; 2]>,
    #[output]
    pub output: Output,
}

impl CsgCall {
    pub fn new(op: Ident, argcount: usize) -> Self {
        CsgCall {
            op: op.0,
            inputs: smallvec![Input::default(); argcount],
            output: Output::default(),
        }
    }
}

implViewCsgOp!(CsgCall, "fieldcall {:?}", op);
impl DialectNode for CsgCall {
    fn dialect(&self) -> &'static str {
        "csg"
    }
}

///Access description for a tree.
#[derive(LangNode, Debug)]
pub struct TreeAccess {
    ///The concept that is being called by the description.
    pub called_concept: String,

    #[inputs]
    pub inputs: SmallVec<[Input; 3]>,
    #[output]
    pub output: Output,
}

impl TreeAccess {
    pub fn new(called_concept: Ident, arg_count: usize) -> Self {
        TreeAccess {
            called_concept: called_concept.0,
            inputs: smallvec![Input::default(); arg_count],
            output: Output::default(),
        }
    }
}

implViewCsgOp!(TreeAccess, "TreeAccess({})", called_concept);
impl DialectNode for TreeAccess {
    fn dialect(&self) -> &'static str {
        "csg"
    }
}
