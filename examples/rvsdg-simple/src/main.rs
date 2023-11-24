use rvsdg::{
    common::{self, CommonRvsdg, VSEdge},
    edge::{LangEdge, PortIndex},
    nodes::LangNode,
    region::Port,
    EdgeRef,
};
pub use rvsdg_viewer::macroquad;
use rvsdg_viewer::{
    macroquad::{main, prelude::BLUE},
    view, View,
};
///Builds the simple rvsdg structures presented in figure 2 of the RVSDG paper.

enum MyNodes {
    Load,
    Store,
    ImmI32(i32),
    ImmChar(char),
    AryConst,
    Lt,
    Gt,
    Mul,
    Add,
}

struct LNode {
    node: MyNodes,
    inputs: Vec<Port>,
    outputs: Vec<Port>,
}

impl LNode {
    fn new(ty: MyNodes) -> Self {
        LNode {
            node: ty,
            inputs: Vec::new(),
            outputs: Vec::new(),
        }
    }
}

impl LangNode for LNode {
    fn inputs(&self) -> &[Port] {
        &self.inputs
    }
    fn outputs(&self) -> &[Port] {
        &self.outputs
    }
    fn inputs_mut(&mut self) -> &mut [Port] {
        &mut self.inputs
    }
    fn outputs_mut(&mut self) -> &mut [Port] {
        &mut self.outputs
    }
}

impl View for LNode {
    fn color(&self) -> macroquad::color::Color {
        BLUE
    }

    fn name(&self) -> &str {
        "LNODE"
    }
}

fn main() {
    let mut graph = CommonRvsdg::<LNode>::new();

    //We build figure 3.a from the source paper.
    // ```C
    // static int max(int x, int y){
    //     return x > y ? x : y;
    // }
    //
    // int f(int a, int b){
    //     puts ("max");
    //     return max(a,b);
    // }
    // ```
    graph.on_omega_node(|mut tu| {
        //Import the external "puts" function
        let import_puts = tu.import("puts");
        //Create a port for the exported f
        let export_port = tu.export("f");

        //now create the delta node for the global "max\0"  value
        let gamma_node = tu.new_global(|gamma_node| {
            gamma_node.on_region(|rb| {
                let im_m = rb.insert_node(LNode::new(MyNodes::ImmChar('m')));
                let im_a = rb.insert_node(LNode::new(MyNodes::ImmChar('a')));
                let im_x = rb.insert_node(LNode::new(MyNodes::ImmChar('x')));
                let im_term = rb.insert_node(LNode::new(MyNodes::ImmChar('\0')));
                let create_const = rb.insert_node(LNode::new(MyNodes::AryConst));

                //connect in correct order
                rb.connect(
                    im_m,
                    PortIndex::Output(0),
                    create_const,
                    PortIndex::Input(0),
                    VSEdge::Value,
                )
                .unwrap();
                rb.connect(
                    im_a,
                    PortIndex::Output(0),
                    create_const,
                    PortIndex::Input(1),
                    VSEdge::Value,
                )
                .unwrap();
                rb.connect(
                    im_x,
                    PortIndex::Output(0),
                    create_const,
                    PortIndex::Input(2),
                    VSEdge::Value,
                )
                .unwrap();
                rb.connect(
                    im_term,
                    PortIndex::Output(0),
                    create_const,
                    PortIndex::Input(3),
                    VSEdge::Value,
                )
                .unwrap();
            });
        });

        //create the lambda max function
        let f_max = tu.new_function(|lb| {
            let a_idx = lb.add_argument();
            let b_idx = lb.add_argument();
            let res_idx = lb.add_result();
            lb.on_region(|freg| {
                //insert the predicate node and connect it to the first two arguments
                let (pred_node, edges) = freg
                    .connect_node(
                        LNode::new(MyNodes::Gt),
                        &[(freg.parent(), a_idx), (freg.parent(), b_idx)],
                    )
                    .unwrap();

                //create a gamma node which emits either the a, or b input depending on the pred_node
                let gamma_id = freg.new_decission(|gamma| {
                    //Connect the predicate
                    gamma.connect_predicate(pred_node, PortIndex::Output(0));
                    //now import a and b
                    let gx_idx = gamma.add_entry_variable();
                    let gy_idx = gamma.add_entry_variable();
                    //and export our result
                    let out_idx = gamma.add_exit_variable();

                    //add both branches, first connects the first entry variable to the output
                    gamma.new_branch(|reg| {
                        reg.connect(
                            reg.parent(),
                            PortIndex::Arg {
                                subregion: 0,
                                arg_idx: 0,
                            },
                            reg.parent(),
                            PortIndex::Result {
                                subregion: 0,
                                arg_idx: 0,
                            },
                            VSEdge::value_edge(),
                        );
                    });
                    //connect second input to output
                    gamma.new_branch(|reg| {
                        reg.connect(
                            reg.parent(),
                            PortIndex::Arg {
                                subregion: 1,
                                arg_idx: 1,
                            },
                            reg.parent(),
                            PortIndex::Result {
                                subregion: 1,
                                arg_idx: 0,
                            },
                            VSEdge::value_edge(),
                        );
                    });
                });
                //now connect the gamma output to the freg output
                freg.connect(
                    gamma_id,
                    PortIndex::Output(0),
                    freg.parent(),
                    PortIndex::Result {
                        subregion: 0,
                        arg_idx: 0,
                    },
                    VSEdge::value_edge(),
                )
                .unwrap();
            })
        });

        tu
    });
}

/*
#[main("RVSDGSimple")]
async fn main() {
    let mut graph = CommonRvsdg::<LNode>::new();


    view(graph).await;
}
*/
