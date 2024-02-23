//! # Alge dialect
//!

use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
    smallvec::{smallvec, SmallVec},
};
use vola_ast::alge::FieldAccessor;

use crate::DialectNode;

pub(crate) mod implblock;

///Well known ops for the optimizer. Includes all _BinaryOp_ of the Ast, as well as
/// some well known _function_like_ ops in the SPIRV spec. For instance
#[derive(Debug, Clone, Copy)]
pub enum WkOp {
    //WK unary ops
    //TODO: do we really want to include NOT? I mean flipping floats is fun I guess,
    // but also error prone.
    Not,
    Neg,

    //WK _standard_ binary ops
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    //Call like ops we _know_
    Dot,
    Cross,
    Length,
    SquareRoot,
    Exp,
    Min,
    Max,
    Mix,
    Clamp,

    Abs,
    Frac,
}

impl WkOp {
    ///Returns the input-count for the op
    fn in_count(&self) -> usize {
        match self {
            WkOp::Not => 1,
            WkOp::Neg => 1,

            WkOp::Add => 2,
            WkOp::Sub => 2,
            WkOp::Mul => 2,
            WkOp::Div => 2,
            WkOp::Mod => 2,

            WkOp::Dot => 2,
            WkOp::Cross => 2,
            WkOp::Length => 1,
            WkOp::SquareRoot => 1,
            WkOp::Exp => 2,
            WkOp::Min => 2,
            WkOp::Max => 2,
            WkOp::Mix => 3,
            WkOp::Clamp => 3,

            WkOp::Abs => 1,
            WkOp::Frac => 1,
        }
    }

    pub fn try_parse(s: &str) -> Option<Self> {
        match s {
            "dot" => Some(Self::Dot),
            "cross" => Some(Self::Cross),
            "length" => Some(Self::Length),
            "sqrt" => Some(Self::SquareRoot),
            "exp" => Some(Self::Exp),
            "min" => Some(Self::Min),
            "max" => Some(Self::Max),
            "mix" | "lerp" => Some(Self::Mix),
            "clamp" => Some(Self::Clamp),
            "frac" => Some(Self::Frac),
            "abs" => Some(Self::Abs),
            _ => None,
        }
    }
}

impl From<vola_ast::alge::UnaryOp> for WkOp {
    fn from(value: vola_ast::alge::UnaryOp) -> Self {
        match value {
            vola_ast::alge::UnaryOp::Neg => Self::Neg,
            vola_ast::alge::UnaryOp::Not => Self::Not,
        }
    }
}

impl From<vola_ast::alge::BinaryOp> for WkOp {
    fn from(value: vola_ast::alge::BinaryOp) -> Self {
        match value {
            vola_ast::alge::BinaryOp::Add => Self::Add,
            vola_ast::alge::BinaryOp::Sub => Self::Sub,
            vola_ast::alge::BinaryOp::Mul => Self::Mul,
            vola_ast::alge::BinaryOp::Div => Self::Div,
            vola_ast::alge::BinaryOp::Mod => Self::Mod,
        }
    }
}

//Macro that implements the "View" trait for an AlgeDialect op
macro_rules! implViewAlgeOp {
    ($opname:ident, $str:expr, $($arg:ident),*) => {
        impl rvsdg_viewer::View for $opname {
            fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
                rvsdg_viewer::macroquad::color::Color::from_rgba(200, 170, 170, 255)
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
                rvsdg_viewer::macroquad::color::Color::from_rgba(200, 170, 170, 255)
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

///In the algebraic dialect all operations are unified into a Call-Like op.
///
/// Think of it like prefix notation. So the expression `a + b` becomes `add(a, b)`, `-a` becomes `neg(a)` etc.
///
/// this'll make optimizing easier later on.
///
/// Note that always only one result is returned. So only site-effect free ops can be modeled by this node.
#[derive(LangNode)]
pub struct CallOp {
    #[inputs]
    input: SmallVec<[Input; 2]>,
    #[output]
    output: Output,

    op: WkOp,
}

impl CallOp {
    pub fn new(op: WkOp) -> Self {
        let mut op = CallOp {
            input: SmallVec::new(),
            output: Output::default(),
            op,
        };
        //configure input count
        for _ in 0..op.op.in_count() {
            op.input.push(Input::default())
        }
        op
    }
}

implViewAlgeOp!(CallOp, "{:?}", op);
impl DialectNode for CallOp {
    fn dialect(&self) -> &'static str {
        "alge"
    }
}

///Dummy sink node for unimplemented stuff
#[derive(LangNode)]
pub struct DummyNode {
    #[output]
    out: Output,
}

impl DummyNode {
    #[allow(unused)]
    pub fn new() -> Self {
        DummyNode {
            out: Output::default(),
        }
    }
}

implViewAlgeOp!(DummyNode, "Dummynode");
impl DialectNode for DummyNode {
    fn dialect(&self) -> &'static str {
        "dummy"
    }
}

///The Eval node in itself is a call-site to some connected λ-node, when specialised.
///
/// By definition the first argument is the callable that will be called, and all following ports are arguments
/// to that call.
#[derive(LangNode, Debug)]
pub struct EvalNode {
    #[inputs]
    inputs: SmallVec<[Input; 3]>,
    ///The eval node itsel has only one output, the state that is produced by the called concept.
    #[output]
    out: Output,
}

impl EvalNode {
    pub fn new() -> Self {
        EvalNode {
            inputs: smallvec![Input::default(); 3],
            out: Output::default(),
        }
    }
}

implViewAlgeOp!(EvalNode, "Eval");
impl DialectNode for EvalNode {
    fn dialect(&self) -> &'static str {
        "alge"
    }
}

#[derive(LangNode, Debug)]
pub struct Imm {
    ///The immediate value
    pub lit: vola_ast::common::Literal,
    ///the output port the `lit` value is passed down to.
    #[output]
    out: Output,
}

impl Imm {
    pub fn new(lit: vola_ast::common::Literal) -> Self {
        Imm {
            lit,
            out: Output::default(),
        }
    }
}

implViewAlgeOp!(Imm, "{}", lit);
impl DialectNode for Imm {
    fn dialect(&self) -> &'static str {
        "alge"
    }
}

///Highlevel "field access" node. Can only be legalized when all types are resolved and the field is specialized.
#[derive(LangNode, Debug)]
pub struct FieldAccess {
    #[input]
    access_src: Input,
    access_list: SmallVec<[FieldAccessor; 1]>,
    #[output]
    output: Output,
}

impl FieldAccess {
    pub fn new(access_list: SmallVec<[FieldAccessor; 1]>) -> Self {
        FieldAccess {
            access_src: Input::default(),
            access_list,
            output: Output::default(),
        }
    }
}

//NOTE hand implementing for nicer field_acces_list
impl rvsdg_viewer::View for FieldAccess {
    fn color(&self) -> rvsdg_viewer::macroquad::color::Color {
        rvsdg_viewer::macroquad::color::Color::from_rgba(200, 170, 170, 255)
    }

    fn name(&self) -> String {
        use std::fmt::Write;
        let mut string = format!("FieldAccess: ");
        for acc in &self.access_list {
            match acc {
                FieldAccessor::Digit { digit, .. } => write!(string, ".{}", digit).unwrap(),
                FieldAccessor::Ident { ident, .. } => write!(string, ".{}", ident.0).unwrap(),
            }
        }

        string
    }

    fn stroke(&self) -> rvsdg_viewer::Stroke {
        rvsdg_viewer::Stroke::Line
    }
}

impl DialectNode for FieldAccess {
    fn dialect(&self) -> &'static str {
        "alge"
    }
}

#[derive(LangNode, Debug)]
pub struct ListConst {
    #[inputs]
    pub inputs: SmallVec<[Input; 3]>,
    #[output]
    pub output: Output,
}

impl ListConst {
    pub fn new() -> Self {
        ListConst {
            inputs: SmallVec::new(),
            output: Output::default(),
        }
    }
}

implViewAlgeOp!(ListConst, "ListConst");
impl DialectNode for ListConst {
    fn dialect(&self) -> &'static str {
        "alge"
    }
}
