use rvsdg::{
    region::{Input, Output},
    rvsdg_derive_lang::LangNode,
};
use rvsdg_viewer::{Color, View};

//Example 2.d. of the source paper
mod ex_2d;
//Example 3.a. of the source paper
mod ex_3a;
//Example 3.b. of the source paper
mod ex_3b;

//simple test for the viewer library.
mod minigraph;

///Builds the simple rvsdg structures presented in figure 2 of the RVSDG paper.
#[derive(Clone, Debug)]
pub enum MyNodes {
    Load,
    Store,
    ImmI32(i32),
    ImmChar(char),
    AryConst(usize),
    Lt,
    Gt,
    UnEq,
    Mul,
    Add,
    Sub,
}

//NOTE: Uses the LangNode derive macro. You could also implement the
// trait yourself if you want
#[derive(Clone, Debug, LangNode)]
pub struct LNode {
    node: MyNodes,
    #[inputs]
    inputs: Vec<Input>,
    #[outputs]
    outputs: Vec<Output>,
}

impl LNode {
    fn new(ty: MyNodes) -> Self {
        //configure inputs/outputs
        let (n_inputs, n_outputs) = match ty {
            MyNodes::Load => (2, 2),
            MyNodes::Store => (3, 1),
            MyNodes::ImmChar(_) => (0, 1),
            MyNodes::ImmI32(_) => (0, 1),
            //For the sake of this example
            MyNodes::AryConst(size) => (size, 1),
            MyNodes::Lt => (2, 1),
            MyNodes::Gt => (2, 1),
            MyNodes::UnEq => (2, 1),
            MyNodes::Mul => (2, 1),
            MyNodes::Add => (2, 1),
            MyNodes::Sub => (2, 1),
        };

        LNode {
            node: ty,
            inputs: vec![Input::default(); n_inputs],
            outputs: vec![Output::default(); n_outputs],
        }
    }
}

impl View for LNode {
    fn color(&self) -> Color {
        Color::from_rgba(255, 255, 128, 255)
    }

    fn name(&self) -> String {
        match self.node {
            MyNodes::Add => "add".to_owned(),
            MyNodes::AryConst(size) => format!("ArrayConst({size})"),
            MyNodes::Gt => "gt".to_owned(),
            MyNodes::ImmChar(c) => format!("ImmChar {:?} ", c.to_string()),
            MyNodes::ImmI32(imm) => format!("ImmI32({imm})"),
            MyNodes::Load => "Load".to_owned(),
            MyNodes::Lt => "lt".to_owned(),
            MyNodes::UnEq => "UnEq".to_owned(),
            MyNodes::Mul => "mul".to_owned(),
            MyNodes::Store => "store".to_owned(),
            MyNodes::Sub => "Sub".to_owned(),
        }
    }
}

fn main() {
    let ex2d = ex_2d::emit();
    assert!(ex2d.verify_parental_relations(), "2d had errors");
    rvsdg_viewer::into_svg(&ex2d, "Example_2d.svg");
    let ex3a = ex_3a::emit();
    assert!(ex3a.verify_parental_relations(), "3a had errors");
    rvsdg_viewer::into_svg(&ex3a, "Example_3a.svg");
    let ex3b = ex_3b::emit();
    assert!(ex3b.verify_parental_relations(), "3b had errors");
    rvsdg_viewer::into_svg(&ex3b, "Example_3b.svg");

    let router_test = minigraph::emit();
    assert!(
        router_test.verify_parental_relations(),
        "router_test had errors"
    );
    rvsdg_viewer::into_svg(&router_test, "router_test.svg");
}
