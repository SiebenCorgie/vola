use rvsdg::{Rvsdg, common::CommonRvsdg, nodes::NodeType};
use test_rvsdg::LNode;

mod test_rvsdg;

fn connected_graph() -> CommonRvsdg<LNode> {
    let mut rvsdg = Rvsdg::new();

    rvsdg.on_omega_node(|o| {
        let (_f, _) = o.new_function(true, |f| {
            let res = f.add_result();
            let arg0 = f.add_argument();
            let arg1 = f.add_argument();

            f.on_region(|reg| {
                let (n, _) = reg
                    .connect_node(
                        LNode::new()
                            .with_inputs(2)
                            .with_name("MyOp")
                            .with_outputs(1),
                        [arg0, arg1],
                    )
                    .unwrap();
                reg.connect_to_result(n.output(0), res.input).unwrap();
            });
        });
    });
    rvsdg
}

#[test]
fn simple_duplicate() {
    let a = connected_graph();
    let b = a.clone();

    //Nodes must be the same after cloning
    assert!(
        a.nodes()
            .zip(b.nodes())
            .fold(true, |track, (a, b)| if a != b { false } else { track })
    );
    //edges too, which effectively makes the graph equal
    assert!(
        a.edges()
            .zip(b.edges())
            .fold(true, |track, (a, b)| if a != b { false } else { track })
    )
}
fn noarg_graph() -> CommonRvsdg<LNode> {
    let mut rvsdg = Rvsdg::new();

    rvsdg.on_omega_node(|o| {
        let (_f, _) = o.new_function(true, |f| {
            let res = f.add_result();

            f.on_region(|reg| {
                let arg0 = reg.insert_node(LNode::new().with_outputs(1).with_name("Hello"));
                let arg1 = reg.insert_node(LNode::new().with_outputs(1).with_name("World"));

                let (n, _) = reg
                    .connect_node(
                        LNode::new()
                            .with_inputs(2)
                            .with_name("MyOp")
                            .with_outputs(1),
                        [arg0.output(0), arg1.output(0)],
                    )
                    .unwrap();
                reg.connect_to_result(n.output(0), res.input).unwrap();
            });
        });
    });
    rvsdg
}

#[test]
fn noarg_duplicate() {
    let a = noarg_graph();
    let b = a.clone();

    //Nodes must be the same after cloning
    assert!(
        a.nodes()
            .zip(b.nodes())
            .fold(true, |track, (a, b)| if a != b { false } else { track })
    );
    //edges too, which effectively makes the graph equal
    assert!(
        a.edges()
            .zip(b.edges())
            .fold(true, |track, (a, b)| if a != b { false } else { track })
    )
}

#[test]
fn noarg_with_change() {
    let a = noarg_graph();
    let mut b = a.clone();

    let a_before = a.nodes().collect::<Vec<_>>();

    let arg_node = b
        .nodes()
        .find(|node| {
            if let NodeType::Simple(s) = &b.node(*node).node_type {
                s.name.as_str() == "Hello"
            } else {
                false
            }
        })
        .unwrap();

    let _ = b.remove_node(arg_node).unwrap();
    //edges too, which effectively makes the graph equal
    assert_eq!(a.nodes().len(), a_before.len());
    assert!(
        a.nodes()
            .zip(a_before.into_iter())
            .fold(true, |track, (a, b)| if a != b { false } else { track })
    )
}
