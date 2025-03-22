use rvsdg::{
    common::VSEdge,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    Rvsdg,
};
use test_rvsdg::LNode;

mod test_rvsdg;

#[test]
fn simple_cne() {
    //Builds a graph with a λ-node that just returns the addition of two
    //equal constants. We expect those constanst to be fused

    let mut rvsdg: Rvsdg<LNode, VSEdge> = Rvsdg::new();
    rvsdg.on_omega_node(|omg| {
        omg.new_function(true, |lmdb| {
            let _lmdres = lmdb.add_result();
            lmdb.on_region(|reg| {
                let const0 = reg.insert_node(
                    LNode::new()
                        .with_inputs(0)
                        .with_outputs(1)
                        .with_name("const x"),
                );
                let const1 = reg.insert_node(
                    LNode::new()
                        .with_inputs(0)
                        .with_outputs(1)
                        .with_name("const x"),
                );

                assert!(
                    reg.ctx().node(const0).node_type.unwrap_simple_ref()
                        == reg.ctx().node(const1).node_type.unwrap_simple_ref()
                );
                let (add, _edges) = reg
                    .connect_node(
                        LNode::new().with_name("+").with_inputs(2).with_outputs(1),
                        [const0.output(0), const1.output(0)],
                    )
                    .unwrap();

                reg.connect_to_result(add.output(0), InputType::Result(0))
                    .unwrap();
            });
        });
    });

    let removed = rvsdg.common_node_elemination().unwrap();
    assert!(
        removed.len() == 1,
        "expected 1 removed nodes from dead_node elemination after CNE, got {}",
        removed.len()
    );
}

#[test]
fn medium_complex_cne() {
    //Medium complex CNE test based on figure 6 _Dead and CommonNodeElemination_.

    let mut rvsdg: Rvsdg<LNode, VSEdge> = Rvsdg::new();
    rvsdg.on_omega_node(|omg| {
        omg.new_function(true, |lmdb| {
            let argx = lmdb.add_argument();
            let _lmdres = lmdb.add_result();
            let _lmdres2 = lmdb.add_result();
            lmdb.on_region(|reg| {
                let const6 = reg.insert_node(
                    LNode::new()
                        .with_inputs(0)
                        .with_outputs(1)
                        .with_name("const 6"),
                );
                let const99 = reg.insert_node(
                    LNode::new()
                        .with_inputs(0)
                        .with_outputs(1)
                        .with_name("const 99"),
                );

                let gt = reg
                    .connect_node(
                        LNode::new().with_inputs(2).with_outputs(1).with_name(">"),
                        [const99.output(0), argx],
                    )
                    .unwrap();

                let gamma = reg.new_decission(|gb| {
                    let (gamma_arg1, _ga1) = gb.add_entry_variable();
                    let (gamma_arg2, _ga2) = gb.add_entry_variable();

                    let (gamma_out1, _go1) = gb.add_exit_variable();
                    let (gamma_out2, _go2) = gb.add_exit_variable();

                    gb.new_branch(|b, branch_index| {
                        //Hookup the args first
                        b.ctx_mut()
                            .connect(argx, gamma_arg1, VSEdge::Value)
                            .unwrap();
                        b.ctx_mut()
                            .connect(const6.output(0), gamma_arg2, VSEdge::Value)
                            .unwrap();
                        b.ctx_mut()
                            .connect(
                                gt.0.output(0),
                                InportLocation {
                                    node: gamma_arg1.node,
                                    input: InputType::GammaPredicate,
                                },
                                VSEdge::Value,
                            )
                            .unwrap();

                        //Now wire through both args

                        b.connect_to_result(
                            OutportLocation {
                                node: gamma_arg1.node,
                                output: OutputType::EntryVariableArgument {
                                    branch: branch_index,
                                    entry_variable: 0,
                                },
                            },
                            InputType::ExitVariableResult {
                                branch: branch_index,
                                exit_variable: gamma_out1,
                            },
                        )
                        .unwrap();
                        b.connect_to_result(
                            OutportLocation {
                                node: gamma_arg1.node,
                                output: OutputType::EntryVariableArgument {
                                    branch: branch_index,
                                    entry_variable: 1,
                                },
                            },
                            InputType::ExitVariableResult {
                                branch: branch_index,
                                exit_variable: gamma_out2,
                            },
                        )
                        .unwrap();
                    });

                    //This is the interesting branch that wires up redundant nodes.
                    gb.new_branch(|b, branch| {
                        let xarg = OutportLocation {
                            node: gamma_arg1.node,
                            output: OutputType::EntryVariableArgument {
                                branch,
                                entry_variable: 0,
                            },
                        };
                        let (xsquare1, _) = b
                            .connect_node(
                                LNode::new().with_inputs(2).with_outputs(1).with_name("*"),
                                [xarg, xarg],
                            )
                            .unwrap();
                        let (xsquare2, _) = b
                            .connect_node(
                                LNode::new().with_inputs(2).with_outputs(1).with_name("*"),
                                [xarg, xarg],
                            )
                            .unwrap();
                        let yarg = OutportLocation {
                            node: gamma_arg1.node,
                            output: OutputType::EntryVariableArgument {
                                branch,
                                entry_variable: 1,
                            },
                        };
                        let (ysquare1, _) = b
                            .connect_node(
                                LNode::new().with_inputs(2).with_outputs(1).with_name("*"),
                                [yarg, yarg],
                            )
                            .unwrap();
                        let (ysquare2, _) = b
                            .connect_node(
                                LNode::new().with_inputs(2).with_outputs(1).with_name("*"),
                                [yarg, yarg],
                            )
                            .unwrap();
                        let (xsq_min_ysq, _) = b
                            .connect_node(
                                LNode::new().with_outputs(1).with_inputs(2).with_name("-"),
                                [xsquare1.output(0), ysquare1.output(0)],
                            )
                            .unwrap();
                        let (ysq_add_xsq, _) = b
                            .connect_node(
                                LNode::new().with_inputs(2).with_outputs(1).with_name("+"),
                                [ysquare2.output(0), xsquare2.output(0)],
                            )
                            .unwrap();

                        let (div, _) = b
                            .connect_node(
                                LNode::new().with_inputs(2).with_outputs(1).with_name("/"),
                                [xsq_min_ysq.output(0), ysq_add_xsq.output(0)],
                            )
                            .unwrap();

                        let (negdiv, _) = b
                            .connect_node(
                                LNode::new().with_inputs(1).with_outputs(1).with_name("-"),
                                [div.output(0)],
                            )
                            .unwrap();

                        b.connect_to_result(
                            div.output(0),
                            InputType::ExitVariableResult {
                                branch,
                                exit_variable: 0,
                            },
                        )
                        .unwrap();
                        b.connect_to_result(
                            negdiv.output(0),
                            InputType::ExitVariableResult {
                                branch,
                                exit_variable: 1,
                            },
                        )
                        .unwrap();
                    });
                });

                //Pass through results
                reg.connect_to_result(
                    OutportLocation {
                        node: gamma.0,
                        output: OutputType::ExitVariableOutput(0),
                    },
                    InputType::Result(0),
                )
                .unwrap();
                reg.connect_to_result(
                    OutportLocation {
                        node: gamma.0,
                        output: OutputType::ExitVariableOutput(1),
                    },
                    InputType::Result(1),
                )
                .unwrap();
            });
        });
    });

    //dump_graph_to_svg(&rvsdg, "BeforCne.svg");
    let removed = rvsdg.common_node_elemination().unwrap();
    //dump_graph_to_svg(&rvsdg, "AfterCne.svg");
    //NOTE that the CNE only renders 2 nodes _dead_. The two multiplies
    //NOTE: Note as well, that we ignore the else branch, since it doesn't do anything in this case
    assert!(
        removed.len() == 2,
        "expected to removed nodes from dead_node elemination after CNE, got {}",
        removed.len()
    );

    for removed in removed {
        assert!(removed.node_type.unwrap_simple_ref().name == "*");
    }
}
