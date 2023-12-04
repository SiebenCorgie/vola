use rvsdg::{
    common::{CommonRvsdg, VSEdge},
    edge::{InputType, OutputType},
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
    graph.on_omega_node(|o| {
        let phi_export = o.export("f".to_string());
        o.on_region(|reg| {
            let phi_node = reg.new_recursive_region(|phi| {
                //defines the recursion_variable  we put f() into.
                let (phi_res, phi_arg, _phi_out) = phi.add_recursion_variable();
                //now setup f which will be connected to `phi`'s output, and also import (as a cv)
                // itself via the `phi_fn_idx`-th rv of `phi`. Which effectively models the recursion in the DAG
                phi.on_region(|phi_reg| {
                    let fnode = phi_reg.new_function(|lambda_f| {
                        let (_cv_f_input, cv_f_arg) = lambda_f.add_context_variable();
                        let arg_x = lambda_f.add_argument();
                        let res_f = lambda_f.add_result();
                        lambda_f.on_region(|lambda_reg| {
                            //Setup unequal
                            let lit_one = lambda_reg.insert_node(LNode::new(MyNodes::ImmI32(1)));
                            let uneq = lambda_reg.insert_node(LNode::new(MyNodes::UnEq));

                            lambda_reg
                                .ctx_mut()
                                .connect(
                                    lit_one.as_outport_location(OutputType::Output(0)),
                                    uneq.as_inport_location(InputType::Input(0)),
                                    VSEdge::Value,
                                )
                                .unwrap();
                            lambda_reg
                                .ctx_mut()
                                .connect(
                                    arg_x.clone(),
                                    uneq.as_inport_location(InputType::Input(1)),
                                    VSEdge::Value,
                                )
                                .unwrap();
                            //setup f's gamma regions

                            let dec = lambda_reg.new_decission(|gamma| {
                                //import of f into gamma's region
                                let (_ev_f_input, ev_f_idx) = gamma.add_entry_variable();
                                let (_ev_x_input, ev_x_idx) = gamma.add_entry_variable();
                                let (ex_res_idx, _ex_res_out) = gamma.add_exit_variable();
                                let _litonebranch = gamma.new_branch(|lit_one_branch, bidx| {
                                    let litone =
                                        lit_one_branch.insert_node(LNode::new(MyNodes::ImmI32(1)));
                                    //Build the simple lit one branch
                                    let parent = lit_one_branch.parent();
                                    lit_one_branch
                                        .ctx_mut()
                                        .connect(
                                            litone.as_outport_location(OutputType::Output(0)),
                                            parent.as_inport_location(
                                                InputType::ExitVariableResult {
                                                    branch: bidx,
                                                    exit_variable: ex_res_idx,
                                                },
                                            ),
                                            VSEdge::Value,
                                        )
                                        .unwrap();
                                });

                                let _rec_call_branch = gamma.new_branch(|callbranch, bidx| {
                                    let parent = callbranch.parent();
                                    let litone =
                                        callbranch.insert_node(LNode::new(MyNodes::ImmI32(-1)));
                                    let (decone, _args) = callbranch
                                        .connect_node(
                                            LNode::new(MyNodes::Sub),
                                            &[
                                                parent.as_outport_location(
                                                    OutputType::EntryVariableArgument {
                                                        branch: bidx,
                                                        entry_variable: ev_x_idx,
                                                    },
                                                ),
                                                litone.as_outport_location(OutputType::Output(0)),
                                            ],
                                        )
                                        .unwrap();
                                    //this is the recursion call
                                    let (call_f, _args) = callbranch
                                        .call(
                                            parent.as_outport_location(
                                                OutputType::EntryVariableArgument {
                                                    branch: bidx,
                                                    entry_variable: ev_f_idx,
                                                },
                                            ),
                                            &[decone.as_outport_location(OutputType::Output(0))],
                                        )
                                        .unwrap();

                                    //NOTE: Since we are recusively define f(), we don't know the caller yet. Therefore the result port
                                    // count doesn't match
                                    if let Node::Apply(an) = callbranch.ctx_mut().node_mut(call_f) {
                                        //Add the output
                                        assert!(an.add_output() == 0);
                                    }

                                    //multiply the result of f by x
                                    let (mul, _args) = callbranch
                                        .connect_node(
                                            LNode::new(MyNodes::Mul),
                                            &[
                                                parent.as_outport_location(
                                                    OutputType::EntryVariableArgument {
                                                        branch: bidx,
                                                        entry_variable: ev_x_idx,
                                                    },
                                                ),
                                                call_f.as_outport_location(OutputType::Output(0)),
                                            ],
                                        )
                                        .unwrap();

                                    //use the result of f to feed back the result
                                    callbranch
                                        .ctx_mut()
                                        .connect(
                                            mul.as_outport_location(OutputType::Output(0)),
                                            parent.as_inport_location(
                                                InputType::ExitVariableResult {
                                                    branch: 1,
                                                    exit_variable: ex_res_idx,
                                                },
                                            ),
                                            VSEdge::Value,
                                        )
                                        .unwrap();
                                });
                            });
                            //Uneq -> Gamma
                            lambda_reg
                                .ctx_mut()
                                .connect(
                                    uneq.as_outport_location(OutputType::Output(0)),
                                    dec.as_inport_location(InputType::GammaPredicate),
                                    VSEdge::Value,
                                )
                                .unwrap();

                            //f import -> gamma
                            lambda_reg
                                .ctx_mut()
                                .connect(
                                    cv_f_arg,
                                    dec.as_inport_location(InputType::EntryVariableInput(0)),
                                    VSEdge::Value,
                                )
                                .unwrap();
                            // x -> gamma
                            lambda_reg
                                .ctx_mut()
                                .connect(
                                    arg_x,
                                    dec.as_inport_location(InputType::EntryVariableInput(1)),
                                    VSEdge::Value,
                                )
                                .unwrap();

                            //Gamma -> result of f()
                            lambda_reg
                                .ctx_mut()
                                .connect(
                                    dec.as_outport_location(OutputType::Output(0)),
                                    res_f,
                                    VSEdge::Value,
                                )
                                .unwrap();
                        });
                    });
                    //f() -> phi_out
                    phi_reg
                        .ctx_mut()
                        .connect(
                            fnode.as_outport_location(OutputType::Output(0)),
                            phi_res,
                            VSEdge::Value,
                        )
                        .unwrap();

                    phi_reg
                        .ctx_mut()
                        .connect(
                            phi_arg,
                            fnode.as_inport_location(InputType::ContextVariableInput(0)),
                            VSEdge::Value,
                        )
                        .unwrap();
                });
            });

            //TODO: Currently defining our selfs. Would be kinda cool if we'd be able to move that out of the builder...
            let phi_out = phi_node.as_outport_location(OutputType::RecursionVariableOutput(0));

            reg.ctx_mut()
                .connect(phi_out, phi_export, VSEdge::Value)
                .unwrap();
        });
    });

    graph
}
