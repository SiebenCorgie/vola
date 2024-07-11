use rvsdg::{
    common::VSEdge,
    edge::{InportLocation, InputType, OutportLocation, OutputType},
    nodes::NodeType,
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
                        &[const0.output(0), const1.output(0)],
                    )
                    .unwrap();

                reg.connect_to_result(add.output(0), InputType::Result(0))
                    .unwrap();
            });
        });
    });

    crate::test_rvsdg::dump_graph_to_svg(&rvsdg, "before_cne");
    rvsdg.common_node_elemination().unwrap();
    crate::test_rvsdg::dump_graph_to_svg(&rvsdg, "after_cne");
}
