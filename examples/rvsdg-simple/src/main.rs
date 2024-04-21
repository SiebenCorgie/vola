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
mod nodes;
pub use nodes::{LNode, MyNodes};
//simple test for the viewer library.
mod minigraph;

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
