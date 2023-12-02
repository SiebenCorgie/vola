use rvsdg::{
    common::{CommonRvsdg, VSEdge},
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::Node,
    Rvsdg,
};
pub use rvsdg_viewer::macroquad;

use crate::{LNode, MyNodes};

pub fn emit() -> Rvsdg<LNode, VSEdge> {
    let mut graph = CommonRvsdg::<LNode>::new();
    /* Builds a graphs for the following c-style code
     * unsigned int f(unsigned int x){
     *     if (1 != x)
     *         return x * f(x-1);
     *     return 1;
     * }
     */

    graph.on_omega_node(|o|{
        o.
    });

    graph
}
