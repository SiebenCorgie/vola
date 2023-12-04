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
    graph.on_omega_node(|tu| {
        let puts_import = tu.import("puts");
        //create the global "max" value
        let global_string_max = tu.new_global(|glob| {
            glob.on_region(|reg| {
                let const_m = reg.insert_node(LNode::new(MyNodes::ImmChar('m')));
                let const_a = reg.insert_node(LNode::new(MyNodes::ImmChar('a')));
                let const_x = reg.insert_node(LNode::new(MyNodes::ImmChar('x')));
                let const_term = reg.insert_node(LNode::new(MyNodes::ImmChar('\0')));
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
                reg.connect_to_result(
                    const_arr.as_outport_location(OutputType::Output(0)),
                    InputType::Result(0),
                )
                .unwrap();
            });
        });

        //Create the max function
        let function_max = tu.new_function(None, |func| {
            let arg_x = func.add_argument();
            let arg_y = func.add_argument();
            let res_max = func.add_result();

            func.on_region(|reg| {
                let (gt_node, _edges) = reg
                    .connect_node(LNode::new(MyNodes::Gt), &[arg_x.clone(), arg_y.clone()])
                    .unwrap();

                //setup the max_gamma branches and connect gt on them
                let (gamma_node, (ev0_in, ev1_in, ex0_out)) = reg.new_decission(|gamma| {
                    let (ev0_in, ev0_idx) = gamma.add_entry_variable();
                    let (ev1_in, ev1_idx) = gamma.add_entry_variable();
                    let (ex0_idx, ex0_out) = gamma.add_exit_variable();

                    //branch 0 maps x to the exit variable
                    let _bx = gamma.new_branch(|branch_x, bidx| {
                        let parent = branch_x.parent();
                        branch_x
                            .ctx_mut()
                            .connect(
                                OutportLocation {
                                    node: parent,
                                    output: OutputType::EntryVariableArgument {
                                        branch: bidx,
                                        entry_variable: ev0_idx,
                                    },
                                },
                                InportLocation {
                                    node: parent,
                                    input: InputType::ExitVariableResult {
                                        branch: bidx,
                                        exit_variable: ex0_idx,
                                    },
                                },
                                VSEdge::Value,
                            )
                            .unwrap();
                    });
                    //branch 1 maps y to the exit variable
                    let _by = gamma.new_branch(|branch_y, bidx| {
                        let parent = branch_y.parent();
                        branch_y
                            .ctx_mut()
                            .connect(
                                OutportLocation {
                                    node: parent,
                                    output: OutputType::EntryVariableArgument {
                                        branch: bidx,
                                        entry_variable: ev1_idx,
                                    },
                                },
                                InportLocation {
                                    node: parent,
                                    input: InputType::ExitVariableResult {
                                        branch: bidx,
                                        exit_variable: ex0_idx,
                                    },
                                },
                                VSEdge::Value,
                            )
                            .unwrap();
                    });

                    (ev0_in, ev1_in, ex0_out)
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
                    .connect(ex0_out, res_max, VSEdge::Value)
                    .unwrap();
                //Connect a and b to gamma entry variables
                reg.ctx_mut().connect(arg_x, ev0_in, VSEdge::Value).unwrap();
                reg.ctx_mut().connect(arg_y, ev1_in, VSEdge::Value).unwrap();
            });
        });

        //now import global string-max and function_max to define f
        let funktion_f = tu.new_function(Some("f".to_owned()), |func| {
            let (_cv_input_fmax, cv_arg_fmax) = func.add_context_variable();
            let (_cv_input_fputs, cv_arg_fputs) = func.add_context_variable();
            let (_cv_str_max_input, cv_str_max) = func.add_context_variable();
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
                let (apply_puts, call_edges) =
                    reg.call(cv_arg_fputs, &[cv_str_max, arg_ctrl]).unwrap();
                //mutate last edge to state edge
                reg.ctx_mut().edge_mut(call_edges[2]).ty = VSEdge::State;

                //call max(a, b);
                let (apply_max, _) = reg.call(cv_arg_fmax, &[arg_a, arg_b]).unwrap();

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
    });

    graph
}
