use rvsdg::{
    common::{CommonRvsdg, VSEdge},
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::{LangNode, Node},
    region::{Input, Output},
};
pub use rvsdg_viewer::macroquad;
use rvsdg_viewer::{
    macroquad::{main, prelude::BLUE},
    view, View,
};

///Builds the simple rvsdg structures presented in figure 2 of the RVSDG paper.
#[derive(Clone, Debug)]
enum MyNodes {
    Load,
    Store,
    ImmI32(i32),
    ImmChar(char),
    AryConst(usize),
    Lt,
    Gt,
    Mul,
    Add,
}

#[derive(Clone, Debug)]
struct LNode {
    node: MyNodes,
    inputs: Vec<Input>,
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
            MyNodes::Mul => (2, 1),
            MyNodes::Add => (2, 1),
        };

        LNode {
            node: ty,
            inputs: vec![Input::default(); n_inputs],
            outputs: vec![Output::default(); n_outputs],
        }
    }
}

impl LangNode for LNode {
    fn inputs(&self) -> &[Input] {
        &self.inputs
    }
    fn outputs(&self) -> &[Output] {
        &self.outputs
    }
    fn inputs_mut(&mut self) -> &mut [Input] {
        &mut self.inputs
    }
    fn outputs_mut(&mut self) -> &mut [Output] {
        &mut self.outputs
    }
}

impl View for LNode {
    fn color(&self) -> macroquad::color::Color {
        BLUE
    }

    fn name(&self) -> &str {
        match self.node {
            MyNodes::Add => "add",
            MyNodes::AryConst(_) => "ArrayConst",
            MyNodes::Gt => "gt",
            MyNodes::ImmChar(_) => "ImmChar",
            MyNodes::ImmI32(_) => "ImmI32",
            MyNodes::Load => "Load",
            MyNodes::Lt => "lt",
            MyNodes::Mul => "mul",
            MyNodes::Store => "store",
        }
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
        let puts_import = tu.import("puts");
        //create the global "max" value
        let global_string_max = tu.new_global(|glob| {
            glob.on_region(|reg| {
                let const_m = reg.insert_node(LNode::new(MyNodes::ImmChar('m')));
                let const_a = reg.insert_node(LNode::new(MyNodes::ImmChar('a')));
                let const_x = reg.insert_node(LNode::new(MyNodes::ImmChar('x')));
                let const_term = reg.insert_node(LNode::new(MyNodes::ImmChar('\0')));
                println!("C_M: {const_m}, C_A: {const_a}, C_X: {const_x}, C_TERM: {const_term}");
                let (const_arr, edges) = reg
                    .connect_node(
                        LNode::new(MyNodes::AryConst(4)),
                        &[
                            const_m.as_outport_location(OutputType::Output(0)),
                            const_a.as_outport_location(OutputType::Output(0)),
                            const_x.as_outport_location(OutputType::Output(0)),
                            const_term.as_outport_location(OutputType::Output(0)),
                        ],
                    )
                    .unwrap();

                assert!(edges.len() == 4);

                //Connect to output
                let parent = reg.parent();
                reg.ctx_mut()
                    .connect(
                        const_arr.as_outport_location(OutputType::Output(0)),
                        InportLocation {
                            node: parent,
                            input: InputType::Result(0),
                        },
                        VSEdge::Value,
                    )
                    .unwrap();
            });
        });

        //Create the max function
        let function_max = tu.new_function(None, |func| {
            let arg_x = func.add_argument();
            let arg_y = func.add_argument();
            let res_max = func.add_result();

            println!(
                "argx: {}, argy: {}, res: {}",
                arg_x.node, arg_y.node, res_max.node
            );

            func.on_region(|reg| {
                let (gt_node, _edges) = reg
                    .connect_node(LNode::new(MyNodes::Gt), &[arg_x, arg_y])
                    .unwrap();

                //setup the max_gamma branches and connect gt on them
                let gamma_node = reg.new_decission(|gamma| {
                    let ev0 = gamma.add_entry_variable();
                    let ev1 = gamma.add_entry_variable();
                    let ex0 = gamma.add_exit_variable();

                    println!("ev0: {}, ev1: {}, ex0: {}", ev0, ev1, ex0);

                    //branch 0 maps x to the exit variable
                    let _bx = gamma.new_branch(|branch_x| {
                        let parent = branch_x.parent();
                        branch_x
                            .ctx_mut()
                            .connect(
                                OutportLocation {
                                    node: parent,
                                    output: OutputType::EntryVariableArgument {
                                        branch: 0,
                                        entry_variable: ev0,
                                    },
                                },
                                InportLocation {
                                    node: parent,
                                    input: InputType::ExitVariableResult {
                                        branch: 0,
                                        exit_variable: ex0,
                                    },
                                },
                                VSEdge::Value,
                            )
                            .unwrap();
                    });
                    //branch 1 maps y to the exit variable
                    let _by = gamma.new_branch(|branch_y| {
                        let parent = branch_y.parent();
                        branch_y
                            .ctx_mut()
                            .connect(
                                OutportLocation {
                                    node: parent,
                                    output: OutputType::EntryVariableArgument {
                                        branch: 1,
                                        entry_variable: ev1,
                                    },
                                },
                                InportLocation {
                                    node: parent,
                                    input: InputType::ExitVariableResult {
                                        branch: 1,
                                        exit_variable: ex0,
                                    },
                                },
                                VSEdge::Value,
                            )
                            .unwrap();
                    });
                });

                //connect gt-simple-node output to gamma predicate.
                reg.ctx_mut()
                    .connect(
                        gt_node.as_outport_location(OutputType::Output(0)),
                        gamma_node.as_inport_location(InputType::GammaPredicate),
                        VSEdge::Value,
                    )
                    .unwrap();

                //Connect the gamma nodes exit variable to the
                reg.ctx_mut()
                    .connect(
                        gamma_node.as_outport_location(OutputType::ExitVariableOutput(0)),
                        res_max,
                        VSEdge::Value,
                    )
                    .unwrap();
            });
        });

        //now import global string-max and function_max to define f
        let funktion_f = tu.new_function(Some("f".to_owned()), |func| {
            let cv_fmax = func.add_context_variable();
            let cv_fputs = func.add_context_variable();
            let cv_str_max = func.add_context_variable();
            let arg_a = func.add_argument();
            let arg_b = func.add_argument();
            let arg_ctrl = func.add_argument();
            let f_res = func.add_result();
            let f_ctr_res = func.add_result();

            //NOTE: on state edges. I think the original paper has a error in the state edge for the puts function.
            //      The state edge should go args -> λ(puts) -> λ(max) -> result. Instead it goes args -> λ(puts) -> result.
            //      IMO this dose not guarantees that puts() is ordered _before_ max. In controll flow this is okay, since max()
            //      does not output anything though. For the sake of comparability I'll leave it as it is.

            func.on_region(|reg| {
                //call puts("max");
                let (apply_puts, call_edges) = reg
                    .call(
                        OutportLocation {
                            node: reg.parent(),
                            output: OutputType::ContextVariableArgument(cv_fputs),
                        },
                        &[
                            OutportLocation {
                                node: reg.parent(),
                                output: OutputType::ContextVariableArgument(cv_str_max),
                            },
                            arg_ctrl,
                        ],
                    )
                    .unwrap();
                //mutate last edge to state edge
                reg.ctx_mut().edge_mut(call_edges[2]).ty = VSEdge::State;

                //call max(a, b);
                let (apply_max, _) = reg
                    .call(
                        OutportLocation {
                            node: reg.parent(),
                            output: OutputType::ContextVariableArgument(cv_fmax),
                        },
                        &[arg_a, arg_b],
                    )
                    .unwrap();

                //We have to add the results to apply_max and apply_puts our selfs, since puts is imported, and
                // max is currently undefined.
                //
                // We could also do the omega-level interconnect before entering this function, for the def to be defined correctly.
                if let Node::Apply(apputs) = reg.ctx_mut().node_mut(apply_puts) {
                    assert!(apputs.add_output() == 0);
                }
                if let Node::Apply(app_max) = reg.ctx_mut().node_mut(apply_max) {
                    assert!(app_max.add_output() == 0);
                }

                //now connect call outputs to the λ result

                //max to output
                reg.ctx_mut()
                    .connect(
                        OutportLocation {
                            node: apply_max,
                            output: OutputType::Output(0),
                        },
                        f_res,
                        VSEdge::Value,
                    )
                    .unwrap();

                //post_puts_state to output

                reg.ctx_mut()
                    .connect(
                        OutportLocation {
                            node: apply_puts,
                            output: OutputType::Output(0),
                        },
                        f_ctr_res,
                        VSEdge::State,
                    )
                    .unwrap();
            });
        });
        //finally wire the lambda and delta nodes
        //we wire the f_max to cv0, the imported f_puts to cv1 and the glob_max to cv2
        // and finally the output of f to the export

        tu.on_region(|tureg| {
            tureg
                .ctx_mut()
                .connect(
                    function_max.as_outport_location(OutputType::LambdaDecleration),
                    funktion_f.as_inport_location(InputType::ContextVariableInput(0)),
                    VSEdge::Value,
                )
                .unwrap();
            tureg
                .ctx_mut()
                .connect(
                    puts_import,
                    funktion_f.as_inport_location(InputType::ContextVariableInput(1)),
                    VSEdge::Value,
                )
                .unwrap();

            tureg
                .ctx_mut()
                .connect(
                    global_string_max.as_outport_location(OutputType::DeltaDecleration),
                    funktion_f.as_inport_location(InputType::ContextVariableInput(2)),
                    VSEdge::Value,
                )
                .unwrap();
        });

        tu
    });

    rvsdg_viewer::into_svg(&graph, "MyFirstPrint.svg");
}

/*
#[main("RVSDGSimple")]
async fn main() {
    let mut graph = CommonRvsdg::<LNode>::new();


    view(graph).await;
}
*/
