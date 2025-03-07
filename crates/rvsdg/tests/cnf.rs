use rvsdg::{common::VSEdge, edge::InputType, nodes::NodeType, util::cnf::ConstantFoldable, Rvsdg};
use rvsdg_viewer::View;
use test_rvsdg::LNode;

mod test_rvsdg;

impl ConstantFoldable<LNode, VSEdge> for LNode {
    fn try_constant_fold(
        &self,
        #[allow(unused_variables)] src_nodes: &[Option<&rvsdg::nodes::Node<LNode>>],
    ) -> Option<LNode> {
        match self.name.as_str() {
            "+" | "*" | "/" | "-" => {
                if src_nodes.len() != 2 {
                    return None;
                }
                match (
                    src_nodes[0].map(|n| &n.node_type),
                    src_nodes[1].map(|n| &n.node_type),
                ) {
                    (Some(NodeType::Simple(a)), Some(NodeType::Simple(b))) => {
                        if let (Some(c0), Some(c1)) =
                            (a.constant_gamma_branch(), b.constant_gamma_branch())
                        {
                            let new_value = match self.name().as_str() {
                                "+" => c0 + c1,
                                "-" => c0 - c1,
                                "*" => c0 * c1,
                                "/" => c0 / c1,
                                _ => panic!("oh no!"),
                            };

                            Some(
                                LNode::new()
                                    .with_name(new_value.to_string().as_str())
                                    .with_outputs(1),
                            )
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
    fn constant_gamma_branch(&self) -> Option<usize> {
        if let Ok(val) = self.name.parse::<usize>() {
            Some(val)
        } else {
            None
        }
    }
}

#[test]
fn simple_cnf() {
    //Builds a simple calculation of 40 + 2, should fold to just 2

    let mut rvsdg: Rvsdg<LNode, VSEdge> = Rvsdg::new();
    rvsdg.on_omega_node(|omg| {
        omg.new_function(true, |lmdb| {
            let _lmdres = lmdb.add_result();
            lmdb.on_region(|reg| {
                let const0 =
                    reg.insert_node(LNode::new().with_inputs(0).with_outputs(1).with_name("40"));
                let const1 =
                    reg.insert_node(LNode::new().with_inputs(0).with_outputs(1).with_name("2"));

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

    //dump_graph_to_svg(&rvsdg, "BeforCnf.svg");
    let folded = rvsdg.constant_fold().unwrap();
    //dump_graph_to_svg(&rvsdg, "AfterCnf.svg");

    //Should have removed the "+"
    assert!(folded.len() == 1);
    assert!(folded[0].unwrap_simple_ref().name() == "+");
}

#[test]
fn single_cascade_cnf() {
    //Builds a simple calculation of (40 + 2) * 3, should fold to just 126

    let mut rvsdg: Rvsdg<LNode, VSEdge> = Rvsdg::new();
    rvsdg.on_omega_node(|omg| {
        omg.new_function(true, |lmdb| {
            let _lmdres = lmdb.add_result();
            lmdb.on_region(|reg| {
                let const0 =
                    reg.insert_node(LNode::new().with_inputs(0).with_outputs(1).with_name("40"));
                let const1 =
                    reg.insert_node(LNode::new().with_inputs(0).with_outputs(1).with_name("2"));
                let const2 =
                    reg.insert_node(LNode::new().with_inputs(0).with_outputs(1).with_name("3"));

                let (add, _edges) = reg
                    .connect_node(
                        LNode::new().with_name("+").with_inputs(2).with_outputs(1),
                        [const0.output(0), const1.output(0)],
                    )
                    .unwrap();

                let (mul, _edges) = reg
                    .connect_node(
                        LNode::new().with_name("*").with_inputs(2).with_outputs(1),
                        [add.output(0), const2.output(0)],
                    )
                    .unwrap();
                reg.connect_to_result(mul.output(0), InputType::Result(0))
                    .unwrap();
            });
        });
    });

    //dump_graph_to_svg(&rvsdg, "BeforCnf.svg");
    let folded = rvsdg.constant_fold().unwrap();
    //dump_graph_to_svg(&rvsdg, "AfterCnf.svg");

    //Should have removed the "+"
    assert!(folded.len() == 2);
    //assert!(folded[0].unwrap_simple_ref().name() == "+");
}
