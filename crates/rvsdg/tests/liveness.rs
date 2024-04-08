use rvsdg::{
    common::VSEdge,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    err::LegalizationError,
    nodes::NodeType,
    Rvsdg,
};
use test_rvsdg::LNode;

mod test_rvsdg;

//Builds the graph of figure 6.c. Which should allow us to mark
// 3 nodes dead, and should mark 34-ports as live.
fn build_liveness_graph() -> Rvsdg<LNode, VSEdge> {
    let mut rvsdg = Rvsdg::new();

    rvsdg.on_omega_node(|omg| {
        //we hookup the gamma on only one output, to get the dead input.
        let g_export = omg.export();
        //add the "x" input.
        let x_input = omg.import();

        omg.on_region(|reg| {
            //setup _outer nodes_
            let nine_nine =
                reg.insert_node(LNode::new().with_inputs(0).with_outputs(1).with_name("99"));
            let gt_out =
                reg.insert_node(LNode::new().with_inputs(2).with_outputs(1).with_name("gt"));
            let six = reg.insert_node(LNode::new().with_inputs(0).with_outputs(1).with_name("6"));

            let (gammanode, _) = reg.new_decission(|gamma| {
                //add inputs
                let (_port, index) = gamma.add_entry_variable();
                assert!(index == 0);
                let (_port, index) = gamma.add_entry_variable();
                assert!(index == 1);
                let (index, _port) = gamma.add_exit_variable();
                assert!(index == 0);
                let (index, _port) = gamma.add_exit_variable();
                assert!(index == 1);

                //add the branch with the internal theta node
                let (teta_b_idx, _) = gamma.new_branch(|gbranch, bidx| {
                    let (tnode, _) = gbranch.new_loop(|tet| {
                        let (_a, _b, _c, _d) = tet.add_loop_variable();
                        let (_a, _b, _c, _d) = tet.add_loop_variable();

                        tet.on_loop(|b_theta| {
                            let nodeid = b_theta.parent_location().node;
                            let fivty = b_theta.insert_node(
                                LNode::new().with_inputs(0).with_outputs(1).with_name("50"),
                            );
                            let one = b_theta.insert_node(
                                LNode::new().with_inputs(0).with_outputs(1).with_name("1"),
                            );
                            let add = b_theta.insert_node(
                                LNode::new()
                                    .with_inputs(2)
                                    .with_outputs(1)
                                    .with_name("plus"),
                            );
                            let gt = b_theta.insert_node(
                                LNode::new().with_inputs(2).with_outputs(1).with_name("gt"),
                            );
                            //wire up the theta internals

                            b_theta
                                .connect_to_result(gt.output(0), InputType::ThetaPredicate)
                                .unwrap();
                            b_theta
                                .ctx_mut()
                                .connect(fivty.output(0), gt.input(0), VSEdge::Value)
                                .unwrap();
                            b_theta
                                .ctx_mut()
                                .connect(add.output(0), gt.input(1), VSEdge::Value)
                                .unwrap();
                            b_theta
                                .connect_to_result(add.output(0), InputType::Result(0))
                                .unwrap();
                            b_theta
                                .connect_arg_to(OutputType::Argument(0), add.input(0))
                                .unwrap();
                            b_theta
                                .ctx_mut()
                                .connect(one.output(0), add.input(1), VSEdge::Value)
                                .unwrap();
                            //theta passthrough
                            b_theta
                                .ctx_mut()
                                .connect(
                                    OutportLocation {
                                        node: nodeid,
                                        output: OutputType::Argument(1),
                                    },
                                    InportLocation {
                                        node: nodeid,
                                        input: InputType::Result(1),
                                    },
                                    VSEdge::Value,
                                )
                                .unwrap();
                        });
                    });

                    // wire up the teta node in its branch
                    gbranch
                        .connect_to_result(
                            tnode.as_outport_location(OutputType::Output(0)),
                            InputType::ExitVariableResult {
                                branch: bidx,
                                exit_variable: 0,
                            },
                        )
                        .unwrap();

                    gbranch
                        .connect_to_result(
                            tnode.as_outport_location(OutputType::Output(1)),
                            InputType::ExitVariableResult {
                                branch: bidx,
                                exit_variable: 1,
                            },
                        )
                        .unwrap();

                    gbranch
                        .connect_arg_to(
                            OutputType::EntryVariableArgument {
                                branch: bidx,
                                entry_variable: 0,
                            },
                            tnode.as_inport_location(InputType::Input(0)),
                        )
                        .unwrap();

                    gbranch
                        .connect_arg_to(
                            OutputType::EntryVariableArgument {
                                branch: bidx,
                                entry_variable: 1,
                            },
                            tnode.as_inport_location(InputType::Input(1)),
                        )
                        .unwrap();
                });
                assert!(teta_b_idx == 0);

                //add the branch with the wild arithmetics
                gamma.new_branch(|b, bidx| {
                    let mul_act_one =
                        b.insert_node(LNode::new().with_inputs(2).with_outputs(1).with_name("Mul"));
                    let mul_act_two =
                        b.insert_node(LNode::new().with_inputs(2).with_outputs(1).with_name("Mul"));

                    let mul_inact_one =
                        b.insert_node(LNode::new().with_inputs(2).with_outputs(1).with_name("Mul"));
                    let mul_inact_two =
                        b.insert_node(LNode::new().with_inputs(2).with_outputs(1).with_name("Mul"));

                    let sub =
                        b.insert_node(LNode::new().with_inputs(2).with_outputs(1).with_name("Sub"));
                    let add =
                        b.insert_node(LNode::new().with_inputs(2).with_outputs(1).with_name("Add"));

                    let div =
                        b.insert_node(LNode::new().with_inputs(2).with_outputs(1).with_name("Div"));
                    let neg =
                        b.insert_node(LNode::new().with_inputs(1).with_outputs(1).with_name("Neg"));

                    //hookup the arithmetics

                    //first hookup args to active multiplies
                    b.connect_arg_to(
                        OutputType::EntryVariableArgument {
                            branch: bidx,
                            entry_variable: 0,
                        },
                        mul_act_one.input(0),
                    )
                    .unwrap();
                    b.connect_arg_to(
                        OutputType::EntryVariableArgument {
                            branch: bidx,
                            entry_variable: 0,
                        },
                        mul_act_one.input(1),
                    )
                    .unwrap();

                    b.connect_arg_to(
                        OutputType::EntryVariableArgument {
                            branch: bidx,
                            entry_variable: 1,
                        },
                        mul_act_two.input(0),
                    )
                    .unwrap();
                    b.connect_arg_to(
                        OutputType::EntryVariableArgument {
                            branch: bidx,
                            entry_variable: 1,
                        },
                        mul_act_two.input(1),
                    )
                    .unwrap();

                    //now hook them up to the inactive multiplies as well

                    b.connect_arg_to(
                        OutputType::EntryVariableArgument {
                            branch: bidx,
                            entry_variable: 0,
                        },
                        mul_inact_one.input(0),
                    )
                    .unwrap();
                    b.connect_arg_to(
                        OutputType::EntryVariableArgument {
                            branch: bidx,
                            entry_variable: 0,
                        },
                        mul_inact_one.input(1),
                    )
                    .unwrap();

                    b.connect_arg_to(
                        OutputType::EntryVariableArgument {
                            branch: bidx,
                            entry_variable: 1,
                        },
                        mul_inact_two.input(0),
                    )
                    .unwrap();
                    b.connect_arg_to(
                        OutputType::EntryVariableArgument {
                            branch: bidx,
                            entry_variable: 1,
                        },
                        mul_inact_two.input(1),
                    )
                    .unwrap();

                    //now hookup the neg (which is dead as well)
                    b.connect_arg_to(
                        OutputType::EntryVariableArgument {
                            branch: bidx,
                            entry_variable: 1,
                        },
                        neg.input(0),
                    )
                    .unwrap();
                    let _ = b.connect_to_result(
                        neg.output(0),
                        InputType::ExitVariableResult {
                            branch: bidx,
                            exit_variable: 1,
                        },
                    );

                    //sub sub and add
                    b.ctx_mut()
                        .connect(mul_act_one.output(0), sub.input(0), VSEdge::Value)
                        .unwrap();
                    b.ctx_mut()
                        .connect(mul_act_two.output(0), sub.input(1), VSEdge::Value)
                        .unwrap();
                    b.ctx_mut()
                        .connect(mul_act_two.output(0), add.input(0), VSEdge::Value)
                        .unwrap();
                    b.ctx_mut()
                        .connect(mul_act_one.output(0), add.input(1), VSEdge::Value)
                        .unwrap();

                    //now hook both up to the div, and the div to the result
                    b.ctx_mut()
                        .connect(sub.output(0), div.input(0), VSEdge::Value)
                        .unwrap();
                    b.ctx_mut()
                        .connect(add.output(0), div.input(1), VSEdge::Value)
                        .unwrap();
                    let _ = b.connect_to_result(
                        div.output(0),
                        InputType::ExitVariableResult {
                            branch: bidx,
                            exit_variable: 0,
                        },
                    );
                });
            });

            //connect to gammanode
            //gtconnection
            reg.ctx_mut()
                .connect(nine_nine.output(0), gt_out.input(0), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(x_input, gt_out.input(1), VSEdge::Value)
                .unwrap();
            reg.ctx_mut()
                .connect(
                    gt_out.output(0),
                    InportLocation {
                        node: gammanode,
                        input: InputType::GammaPredicate,
                    },
                    VSEdge::Value,
                )
                .unwrap();

            //x to entry1 and six to entry 2
            reg.ctx_mut()
                .connect(
                    x_input,
                    InportLocation {
                        node: gammanode,
                        input: InputType::EntryVariableInput(0),
                    },
                    VSEdge::Value,
                )
                .unwrap();
            reg.ctx_mut()
                .connect(
                    six.output(0),
                    InportLocation {
                        node: gammanode,
                        input: InputType::EntryVariableInput(1),
                    },
                    VSEdge::Value,
                )
                .unwrap();
            //finally connect gamma output to export
            reg.ctx_mut()
                .connect(
                    OutportLocation {
                        node: gammanode,
                        output: OutputType::ExitVariableOutput(0),
                    },
                    g_export,
                    VSEdge::Value,
                )
                .unwrap();
        });
    });

    rvsdg
}

#[test]
fn live_port_count() {
    let rvsdg = build_liveness_graph();

    //crate::test_rvsdg::dump_graph_to_svg(&rvsdg, "liveness_graph.svg");

    let liveness_map = rvsdg.liveness();

    //found those by counting in the svg dump 🤝.
    assert!(
        liveness_map.flags.len() == 42,
        "{} != 42",
        liveness_map.flags.len()
    );
}

#[test]
fn dead_node_elimination() {
    //tests the livness code for dead code elimination. This is a somewhat
    // good test, since the case contains normal _unreachable_ nodes, as well as
    // dead node by _into-region-propagation_ nodes.

    let mut rvsdg = build_liveness_graph();

    let deleted_nodes = rvsdg.dead_node_elimination().unwrap();
    assert!(deleted_nodes.len() == 3, "{} != 3", deleted_nodes.len());
    //make sure those are the nodes we'd expect to be deleted.
    let mut mulcount = 0;
    let mut negcount = 0;
    for del in deleted_nodes {
        if let NodeType::Simple(s) = del.node_type {
            match s.name.as_str() {
                "Mul" => mulcount += 1,
                "Neg" => negcount += 1,
                o => panic!("Unexpected simple node \"{}\"", o),
            }
        } else {
            panic!("deleted was not simple node!");
        }
    }

    //finnaly check, that we actually deleted the right amount of nodes
    assert!(mulcount == 2, "expected 2 muls, got {}", mulcount);
    assert!(negcount == 1, "expected 1 neg, got {}", negcount);

    //if unsure, outcomment this and check visually

    //crate::test_rvsdg::dump_graph_to_svg(&rvsdg, "deadnode_post_delete_graph.svg");
}
