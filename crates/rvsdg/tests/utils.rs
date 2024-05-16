use rvsdg::{common::VSEdge, edge::OutputType, Rvsdg};
use test_rvsdg::LNode;

mod test_rvsdg;

#[test]
fn arg_import_simple() {
    let mut graph: Rvsdg<LNode, VSEdge> = Rvsdg::new();

    let lnode = graph.on_omega_node(|omg| {
        let (_lmd, loopnode) = omg.new_function(true, |lb| {
            let lnode = lb.on_region(|r| {
                let lit = r.insert_node(LNode::new().with_outputs(1).with_name("Lit"));
                let litport = lit.output(0);
                let (loopnode, _) = r.new_loop(|lb| {
                    lb.on_loop(|r| {
                        let loc = r.parent_location();
                        r.ctx_mut().import_argument(litport, loc).unwrap();
                    })
                });
                loopnode
            });
            lnode
        });
        loopnode
    });

    assert!(
        graph
            .node(lnode)
            .node_type
            .unwrap_theta_ref()
            .loop_variable_count()
            == 1
    );
}

#[test]
fn arg_import_with_collision() {
    //Tests, that a argument is only imported once. In this case, one branch already imported lit
    // in its construction, so the second import does not have to create a new entry variable
    let mut graph: Rvsdg<LNode, VSEdge> = Rvsdg::new();

    let gamma = graph.on_omega_node(|omg| {
        let (_lmd, gamma) = omg.new_function(true, |lb| {
            let gamma = lb.on_region(|r| {
                let lit = r.insert_node(LNode::new().with_outputs(1).with_name("Lit"));
                let litport = lit.output(0);

                let (g, _) = r.new_decission(|gb| {
                    let (_bidx, imp) = gb.new_branch(|bb, _idx| {
                        let reg = bb.parent_location();
                        let (imported_lit, _) = bb.ctx_mut().import_argument(litport, reg).unwrap();
                        imported_lit
                    });
                    assert!(
                        imp.output
                            == OutputType::EntryVariableArgument {
                                branch: 0,
                                entry_variable: 0
                            }
                    );
                    let (_bidx, second) = gb.new_branch(|bb, _idx| {
                        let reg = bb.parent_location();
                        let (imported_lit, _) = bb.ctx_mut().import_argument(litport, reg).unwrap();
                        imported_lit
                    });
                    assert!(
                        second.output
                            == OutputType::EntryVariableArgument {
                                branch: 1,
                                entry_variable: 0
                            }
                    );
                });
                g
            });
            gamma
        });
        gamma
    });

    let count = graph
        .node(gamma)
        .node_type
        .unwrap_gamma_ref()
        .entry_var_count();
    assert!(count == 1, "expected 1, was {count}",);
}
